printf = function(str, ...) {
    cat(sprintf(str, ...))
}

#' Infix paste0
#' 
#' @param x,y Two strings to be concatenated
'%_%' <- function(x,y) paste0(x, y)

#' Grab something JSON-like from a string
#' 
#' @param text The text to search
#' 
#' Returns anything from the first \{ to the last \}, inclusive.
#' @export
grabJSON = function(text) {
  text = gsub("<think>.*</think>", "", text)
  return(gsub("^[^{]*([{].*[}])[^}]*$", "\\1", text ))
}

fixNull = function(l) {
  nulls = unlist(lapply(l, is.null))
  l[ names(nulls[nulls]) ] <- "NA"
  return(l)
}

addMissing = function(l, names_all, NA_string="NA") {
  missing = setdiff(names_all, names(l))
  l[missing] <- NA_string
  return(l)
}

collapseArrays = function(i) {
  lapply(i, paste, collapse=", ")
}

tryJSON = function(txt, fallback=list()) {
  tryCatch(jsonlite::fromJSON(txt), error=function(e){fallback} )
}

#' Explode a vector of JSON strings to a table
#' 
#' @param raw_json A vector of strings containing unparsed JSON text
#' @param NA_string The value used to fill gaps from missing values (default "NA")
#' 
#' This is a "best effort" attempt: It fills gaps and missing values
#' and attempts to return a data.frame with as may rows as the input,
#' and with one column for each unique key seen in the JSON, padded out
#' with the NA_string value where needed. 
#' @export
jsonToTable = function(raw_json, NA_string="NA") {
  items = lapply(raw_json, function(j) fixNull(tryJSON(grabJSON(j))) )
  superset_names = unique(Reduce(c, lapply(items, names)))
  items = lapply(items, addMissing, superset_names, NA_string)
  items = lapply(items, collapseArrays)
  do.call(rbind, lapply(items, as.data.frame))
}

#' Remove leading and trailing whitespace from a string
#' 
#' @param str The string to trim
#' @export
strip = function(str) {
  gsub("^[[:space:]]+([^[:space:]].*[^[:space:]])[[:space:]]+$", "\\1", str)
}

#' Initialize the publlama package before use
#'
#' @param dbFile Where to keep the article database
#' @param settingsFile Where to store the settings
#' @param entrezKey Use this Entrez API key
#' @param entrezKeyFile Read the Entrez api key from this file
#' 
#' The settings.xml file is used to define the queries
#' and ollama-style endpoints, so be prepared to edit it to your needs.
#' You can also set the entrez key or keyfile in the settings file.
#'
#' @export
publlamaInit = function(dbFile="publlama_db/database.sqlite",
                  settingsFile="publlama_db/settings.xml",
                  entrezKey=NA,
                  entrezKeyFile=NA
                  ) {
  dbDir = dirname(dbFile)
  if(! dir.exists(dbDir)) {
    warning(sprintf("Creating directory %s\n", dbDir))
    dir.create(dbDir)
  }
  
  settingsDir = dirname(settingsFile)
  if(! dir.exists(settingsDir)) {
    warning(sprintf("Creating directory %s\n", settingsDir))
    dir.create(settingsDir)
  }
  
  if(! file.exists(settingsFile)) {
    warning(sprintf("The settings file (%s) is missing.\n%s\n%s\n", 
                    settingsFile,
                    "It has been created as a template,",
                    "edit it and re-run pubLlamaInit().")
    )
    
    file.copy(system.file("extdata/settings.xml", package="publlama"), settingsFile)
  }
  
  readSettings(settingsFile)
  settings$dbCon = initdb(dbFile)
  insertQueries(settings$dbCon, settings)
  for (i in 1:length(settings$prompts)) {
    getOrRegisterPrompt(names(settings$prompts)[i], settings$prompts[[i]])
  }
  
  if (!is.na(entrezKey)) settings$entrez_key = entrezKey
  else if (!is.na(entrezKeyFile)) {
    settings$entrez_key = strip(readLines(entrezKeyFile))
  }
  
  rentrez::set_entrez_key(key = settings$entrez_key)
  
  return(invisible(settings))
}

#' Get the queries defined in the settings file
#' Returns a list.
#' 
#' @export
getQueries = function() {
  return(settings$queries)
}

#' Get the prompts defined in the settings file
#' Returns a list.
#' 
#' @export
getPrompts = function() {
  return(settings$prompts)
}

#' Get the endpoints defined in the settings file
#' Returns a list.
#' 
#' @export
getEndpoints = function() {
  return(settings$endpoints)
}