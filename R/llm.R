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

askRaw = function(URL, model, query, verbose=FALSE, retries=10) {
  reqbody = list(
    prompt = query,
    model  = model,
    stream = FALSE
  )
  
  req <- httr2::request(sprintf("%s/generate", URL)) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(reqbody) |>
    httr2::req_error(is_error = ~ FALSE)
  
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
#' @param retries Retry this many times if a query fails (sleeping randomly 1-5 sec between each)
#' @param force.new Send question to LLM even if this question+model+pmid combo has already been done
#' @param include.title Include the title as well as the abstract in the text send to the LLM
#' #'
#' The endpoint_name is from the settings.xml file.
#' If you haven't changed it, the default "localhost"
#' points to http://localhost:11434, the default for ollama.
#'
#' The valid model names are fetched when initially reading the settings.
#' If you have installed a new model later, re-run publlamaInit().
#' 
#' By default, questions that have already been asked (that is, there is an answer
#' in the DB for the same model, prompt and pubmed id) use that existing answer.
#' If there are multiple, the newest answer is returned.
#' To override this and ask again, set force.new=TRUE.
#'
#' @export
askLLMVec = function(model, prompt, article, endpoint="localhost", 
                     qlen=1, verbose=FALSE, retries=10, force.new=FALSE, 
                     include.title=TRUE) {
  Nw = length(endpoint)
  if(parallelly::supportsMulticore()) {
    oldPlan = future::plan(future::multicore, workers=Nw*qlen)
  } else {
    oldPlan = future::plan(future::multisession, workers=Nw*qlen)
  }
  
  if(length(unique(article$pmid)) != nrow(article)) {
    stop("Error: article$pmid is not unique")
    return(NULL)
  }
  
  epMod = expand.grid(endpoint=endpoint, model=model)
  epMod$URL = apply(epMod, 1, function (r){ getAndCheckEndpoint(r[1], r[2]) })

  work = expand.grid(pmid=article$pmid, prompt=prompt, model=model)
  work$row=1:nrow(work)
  work = merge(work, article)
  work=work[order(work$row), ]
  work = work[, colnames(work)!="row"]
  work$URL = epMod$URL[ 1+(1:nrow(work) %% Nw) ]
  work$prompt = as.character(work$prompt)
  work$model = as.character(work$model)

  promptCache = lapply(prompt, function(p){ getPrompt(p)$prompt })
  names(promptCache) = prompt
  p = progressr::progressor(nrow(work))
  
  res = future.apply::future_lapply( endpoint, function(e) {
    myWork = subset(work, endpoint==e)
    res = lapply(1:nrow(myWork), function(i) {
      row = myWork[i,]
      evaluator = getOrRegisterEvaluator(row$model, row$prompt)
      ans = getEvals(evaluator=evaluator$id, pmid=row$pmid)
      if(nrow(ans) > 0 & !force.new) {
        answer = ans$answer[1]
        p("...skipped...")
      } else {
        query = promptCache[[ row$prompt ]]
        if (include.title) {
          abstract = row$title %_% " \n " %_% row$summary
        } else {
            abstract = row$summary
        }
        question = sprintf(query, row$summary)
        answer=askRaw(row$URL, row$model, question, FALSE, retries=retries)
        if(verbose) {
          p(sprintf("[% 20s] pmid %s, prompt %s, model %s", row$URL, row$pmid, row$prompt, row$model))
        } else {
          p()
        }
        
        evaluator = getOrRegisterEvaluator(row$model, row$prompt)$id
        insertEvals(evaluator, row$pmid, answer)
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
  
  return( res )
}



#' Bulk submit LLM questions
#'
#' @param tbl Data frame of questions. Must have columns pmid, model, promptid.
#' @param endpoints Endpoint(s) to send the query to
#' @param qlen Number of queries to queue up per runner
#' @param verbose Print HTTP error codes if the query fails
#' @param retries Retry this many times if a query fails (sleeping randomly 1-5 sec between each)
#' @param force.new Send question to LLM even if this question+model+pmid combo has already been done (default TRUE)
#' @param include.title Include the title as well as the abstract in the text send to the LLM
#' 
#' The endpoint_name is from the settings.xml file.
#' If you haven't changed it, the default "localhost"
#' points to http://localhost:11434, the default for ollama.
#'
#' The valid model names are fetched when initially reading the settings.
#' If you have installed a new model later, re-run publlamaInit().
#' 
#' By default, questions that have already been asked (that is, there is an answer
#' in the DB for the same model, prompt and pubmed id) use that existing answer.
#' If there are multiple, the newest answer is returned.
#' To override this and ask again, set force.new=TRUE.
#'
#' @export
askLLMBulk = function(tbl, endpoints="localhost", 
                     qlen=1, verbose=FALSE, retries=10, force.new=TRUE, 
                     include.title=TRUE) {
  if(! all(c("pmid", "model", "promptid") %in% colnames(tbl)) ) {
    stop("Error: askLLMBulk requires argument 'tbl' to include columns 'pmid', 'model' and 'promptid'")
    return(NULL)
  }
  
  # Add everything we need to the table
  tbl$evaluatorID = unlist(apply(tbl, 1, function(row) {
    res = getOrRegisterEvaluator(row['model'], as.integer(row['promptid']))
    return(res$id)
  }))
  tbl$row = 1:nrow(tbl)
  articles = lapply(tbl$pmid, getOneArticle)
  articles = do.call(rbind, articles)
  tbl$title = articles$title
  tbl$summary = articles$summary

  # Check that all endpoints have all models
  epMod = expand.grid(endpoints=endpoints, model=unique(tbl$model))
  epMod$URL = apply(epMod, 1, function (r){ getAndCheckEndpoint(r[1], r[2]) })
  
  ## Map each daemon to an endpoint:
  # Start qlen daemons per endpoint
  Np = length(endpoints) * qlen
  mirai::daemons(Np, .compute = "publlama")
  
  # Gather the pid of each daemon
  mirai::with_daemons(.compute = "publlama", {
    pids = mirai::mirai_map(1:Np, \(i){Sys.getpid()})[mirai::.flat]
  })
  
  # Make a pid : endpoint table and look up the URL for each (with an arbitrary model)
  pid2ep = data.frame(pid=pids, ep = rep(endpoints, qlen))
  pid2ep$URL = unlist(lapply(pid2ep$ep, function(ep) {
    getAndCheckEndpoint(ep, tbl$model[1])
  }))
  
  promptCache = lapply(unique(tbl$promptid), function(p){ getPrompt(p)$prompt })
  names(promptCache) = unique(tbl$promptid)
  #p = progressr::progressor(steps=nrow(tbl), label="askLLMBulk")
  mirai::with_daemons(.compute = "publlama", {
    res = mirai::mirai_map(
      tbl[, c("pmid", "promptid", "model", "title", "summary")],
      .f = \(pmid, promptid, model, title, summary) {
        URL = pid2ep$URL[which(pid2ep$pid == Sys.getpid())] 
        abstract = ifelse(include.title, title %_% '\n', "")
        abstract = abstract %_% summary
        query = promptCache[[ as.character(promptid) ]]
        question = sprintf(query, abstract)
        answer = askRaw(URL, model, question, FALSE, retries=1)
        return(answer)
      }
    )
  })
  
 #insertEvals(evaluatorID, pmid, answer)

  return(res)
  #mirai::daemons(0, .compute = "publlama")
}
