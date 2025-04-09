printf = function(str, ...) {
    cat(sprintf(str, ...))
}

#' Grab something JSON-like from a string
#' 
#' @param text The text to search
#' 
#' Returns anything from the first \{ to the last \}, inclusive.
#' @export
grabJSON = function(text) {
  return(gsub("^[^{]*([{].*[}])[^}]*$", "\\1", text ))
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