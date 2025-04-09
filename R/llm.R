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

#' Ask an LLM a question about an article summary
#' 
#' @param endpoint_name Endpoint to send the query to
#' @param model A model name (e.g. "phi4:latest") that's available on the endpoint
#' @param prompt The question to ask
#' @param summary The article summary to ask about
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
askLLM = function(model, prompt, summary, endpoint_name="localhost", verbose=TRUE) {
  URL = getAndCheckEndpoint(endpoint_name, model)
  
  if (length(grep("%s", prompt)) == 0) {
    # Glue on a %s if the prompt doesn't have one
    # There are ways to make this fail, but it'll
    # cover the common "the question is plain English" case.
    prompt = sprintf("%s\n%%s", prompt)
  }
  
  reqbody = list(
    prompt = sprintf(prompt, summary),
    model  = model,
    stream = FALSE
  )

  req <- httr2::request(sprintf("%s/generate", URL)) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(reqbody) |>
    httr2::req_error(is_error = ~ FALSE)

  retries = 5
  sleepmax = 9
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
askLLMVec = function(model, prompt, article, endpoint="localhost", verbose=TRUE) {
  work = expand.grid(endpoint=endpoint, pmid=article$pmid, prompt=prompt, model=model)
  epMod = expand.grid(endpoint=endpoint, model=model)
  epMod$URL = apply(epMod, 1, function (r){ getAndCheckEndpoint(r[1], r[2]) })
  work = merge(work, epMod)
  work = merge(work, article)
  
  res = pbmcapply::pbmclapply(1:nrow(work), mc.cores=length(endpoint), FUN = function(i) {
    query = settings$prompts[[ work$prompt[i] ]]
    abstract = article$summary[i]
    question = sprintf(query, abstract)
    
    askRaw(work$URL[i], work$model[i], abstract, verbose)
  })
  
  return(res)
}
