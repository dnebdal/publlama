getModelsFromEndpoint = function(endpoint) {
  endpoint = sprintf("%s/tags", endpoint)
  req = httr2::request(endpoint) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_error(is_error = ~ FALSE)

  resp = httr2::req_perform(req)
  if (! httr2::resp_status(resp) == 200) return(c())

  body = httr2::resp_body_json(resp)
  models = lapply(body$models, getElement, "model")
  return(unlist(models))
}


getAndCheckEndpoint = function(endpoint_name, model) {
  endpoint = settings$endpoints[
    vapply(settings$endpoints,
    FUN=\(x) x$name==endpoint_name,
    FUN.VALUE=c(T))
  ]

  if(length(endpoint) == 0) {
    endpoints = paste(lapply(settings$endpoints, getElement, "name"), collapse=", ")
    stop(sprintf("No endpoint called \"%s\" (settings.xml has %s)", endpoint_name, endpoints))
  }

  endpoint = endpoint[[1]]
  if( ! model %in% endpoint$models ) {
    models = paste(endpoint$models, collapse="\n")
    stop(sprintf("No model called \"%s\" in %s.\nWhen loading settings, it had these:\n%s",
                 model, endpoint_name, models))
  }

  return(endpoint$endpoint)
}

askRaw = function(URL, model, query, verbose=FALSE) {
  reqbody = list(
    prompt = query,
    model  = model,
    stream = FALSE
  )
  
  req <- httr2::request(sprintf("%s/generate", URL)) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(reqbody) |>
    httr2::req_error(is_error = ~ FALSE)
  
  retries = 5
  sleepmax = 5
  sleepbase = 1
  resp = httr2::req_perform(req)
  while(httr2::resp_status(resp) != 200) {
    if(verbose) printf("[! %i]", httr2::resp_status(resp))
    retries = retries - 1
    if(retries == 0) {return(NA)}
    Sys.sleep(stats::runif(1)*sleepmax + sleepbase)
    
    resp = httr2::req_perform(req)
  }
  
  answer = httr2::resp_body_json(resp)$response
  return(answer)
  
}

#' Ask an LLM a question about an article summary
#'
#' @param model Model name(s) to ask (must be available on all endpoints)
#' @param prompt Prompt name defined in the settings
#' @param article A data frame of one or more articles to ask about
#' @param endpoint Endpoint to send the query to
#' @param qlen Number of queries to queue up per runner
#' @param verbose Print HTTP error codes if the query fails
#'
#' The endpoint_name is from the settings.xml file.
#' If you haven't changed it, the default "localhost"
#' points to http://localhost:11434, the default for ollama.
#'
#' The valid model names are fetched when initially reading the settings.
#' If you have installed a new model later, re-run publlamaInit().
#'
#' @export
askLLMVec = function(model, prompt, article, endpoint="localhost", qlen=1, verbose=FALSE) {
  Nw = length(endpoint)
  oldPlan = future::plan(future::multicore, workers=Nw*qlen)

  epMod = expand.grid(endpoint=endpoint, model=model)
  epMod$URL = apply(epMod, 1, function (r){ getAndCheckEndpoint(r[1], r[2]) })

  work = expand.grid(pmid=article$pmid, prompt=prompt, model=model)
  work$row=1:nrow(work)
  work = merge(work, article)
  work=work[order(work$row), ]
  work = work[, colnames(work)!="row"]
  work$URL = epMod$URL[ 1+(1:nrow(work) %% Nw) ]

  promptCache = lapply(prompt, function(p){ getPrompt(p)$prompt })
  names(promptCache) = prompt
  p = progressr::progressor(nrow(work))
  
  res = future.apply::future_lapply( endpoint, function(e) {
    myWork = subset(work, endpoint==e)
    res = lapply(1:nrow(myWork), function(i) {
      row = myWork[i,]
      query = promptCache[[ row$prompt ]]
      abstract = row$summary
      question = sprintf(query, row$summary)
      answer=askRaw(row$URL, row$model, question, FALSE)
      if(verbose) {
        p(sprintf("[% 20s] prompt %s, pmid %s, model %s", row$URL, row$prompt, row$pmid, row$model))
      } else {
        p()
      }
      return(data.frame(
        pmid=row$pmid,
        model=row$model,
        prompt=row$prompt,
        answer=answer
      ))
    })
    res = res[!is.null(res)]
    return(do.call(rbind, res))
  }, future.seed=TRUE)
  
  res = merge(work, do.call(rbind, res))
  future::plan(oldPlan)
  for(m in model) {
    for(p in prompt) {
      pmres = res[res$model == m & res$prompt == p, ]
      evaluator = getOrRegisterEvaluator(m, p)$id
      insertEvals(evaluator, pmres$pmid, pmres$answer)
    }
  }
  
  
  return( res )
}
