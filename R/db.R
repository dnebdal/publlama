
initdb = function(dbFilename) {
  db = DBI::dbConnect(RSQLite::SQLite(), dbFilename)
  dbTables = DBI::dbGetQuery(db, "PRAGMA table_list")$name

  createFile = system.file("extdata/create.sql.xml", package="publlama")
  
  createStatements_xml = XML::xmlInternalTreeParse(createFile)
  createStatements = XML::xpathApply(createStatements_xml, "//create/table", XML::xmlValue)
  tables = unlist(XML::xpathApply(createStatements_xml, "//create/table", XML::xmlGetAttr, "name"))
  missingTables = which(! tables %in% dbTables)
  
  if (! all(tables %in% dbTables)) {
    # We're missing at least one table
    cat(paste("Creating tables: ", paste(tables[missingTables], collapse=","), "\n"))
    lapply(createStatements[missingTables], function(stm) {DBI::dbExecute(db, stm)} )
    cat("ok.\n")
  }

  return(db)
}

insertQueries = function(db, settings) {
  insertQuery = function(db, name, type, query) {
    query = strip(query)
    query = gsub("[[:space:]]", " ", query)
    DBI::dbExecute(db, "INSERT INTO Queries(name, type, query) VALUES (:n, :t, :q)",
                    params=list(n=name, t=type, q=query))
  }

  dbQueries = DBI::dbGetQuery(db, "SELECT name FROM Queries")$name
  queries = lapply(names(settings$queries), function(i){
                     x=getElement(settings$queries, i); x$name=i; x})

  x = lapply(queries, function(query) {
    if (query$name %in% dbQueries){ return(NA) }
    insertQuery(db, query$name, query$type, query$query)
  })
  return(invisible(x))
}

#' Get a pubmed query by name from the DB
#' 
#' @param name Name of a query (from settings.xml)
#' @export
getQueryByName = function(name) {
  res = DBI::dbGetQuery(settings$dbCon , "SELECT * from Queries where name = :n", list(n=name))
  if (nrow(res) == 0) return(NA)
  query = res$query
  attr(query, "id") <- res$id
  return(query)
}

getOrRegisterPrompt = function(name, prompt) {
  checksum = digest::sha1(prompt)
  res = DBI::dbGetQuery(
    settings$dbCon, 
    "SELECT * from Prompts where checksum = :c AND name = :n",
    list(c=checksum, n=name)
  )

  if (nrow(res) == 0) {
   stm = DBI::dbSendQuery(
     settings$dbCon, 
     "INSERT INTO Prompts (name, checksum, prompt) VALUES (:n, :c, :p) RETURNING *",
     list(n=name, c=checksum, p=prompt)
   )
   res = DBI::dbFetch(stm)
   DBI::dbClearResult(stm)
  }
  
  return(res)
}

getPrompt = function(nameOrID) {
  if (is.numeric(nameOrID)) {
   res = DBI::dbGetQuery(
     settings$dbCon,
     "SELECT * FROM Prompts WHERE id = :i", 
     list(i=nameOrID)
   )
  } else {
    res = DBI::dbGetQuery(
      settings$dbCon,
      "SELECT * FROM Prompts WHERE name = :n ORDER BY id desc LIMIT 1", 
      list(n=nameOrID)
    )
  }
  return(res)
}

#' Get info about an evaluator (a model + prompt pair)
#' 
#' @param model The name of a model, e.g. "phi4:latest"
#' @param promptID The name of a prompt, from settings.xml
#' @export
getOrRegisterEvaluator = function(model, promptID) {
  pid = getPrompt(promptID)$id
  DBI::dbBegin(settings$dbCon)
  res = DBI::dbGetQuery(
      settings$dbCon,
      "SELECT * FROM Evaluators WHERE model = :m AND prompt = :p",
      list(m=model, p=pid)
  )  

  if(nrow(res) == 0) {
    stm = DBI::dbSendQuery(
      settings$dbCon, 
      "INSERT INTO Evaluators (human, prompt, model, name) " %_% 
      "VALUES (FALSE, :p, :m, 'LLM-auto') RETURNING *",
      list(p=pid, m=model)
    )
    res = DBI::dbFetch(stm)
    DBI::dbClearResult(stm)
  }
  DBI::dbCommit(settings$dbCon)
  return(res)
}

#' Store an answer from an evaluator for a given article
#' 
#' @param evaluatorID Evaluator, typically from getOrRegisterEvaluator
#' @param pmid One or more pubmed id numbers
#' @param answer One or more answers (arbitrary string, doesn't have to be valid JSON)
insertEvals = function(evaluatorID, pmid, answer) {
  stm = DBI::dbSendStatement(settings$dbCon,
    "INSERT INTO Evals (evaluator, pmid, answer) VALUES (:e, :p, :a)"
  )
  DBI::dbBind(stm, data.frame(e=evaluatorID, p=pmid, a=answer) )
  DBI::dbClearResult(stm)
}

#' Retreive evaluations (LLM answers)
#' 
#' @param evaluator Filter by evaluator
#' @param pmid Filter by Pubmed ID
#' @param before Filter by evaluation date
#' @param after Filter by evaluation date
#'
#' Using all default values will fetch every single evaluation.
#' The evaluator and pmid fields accept multiple values.
#' The evaluators are ID numbers from [getOrRegisterEvaluator()]. 
#' 
#' @examplesIf FALSE
#' # Get answers to the 'Llama_example' question asked to phi4
#' # in 2025
#' ev = getOrRegisterEvaluator('phi4:latest', 'Llama_example')
#' answers = getEvals(evaluator=ev$id, after='2025-01-01')
#' 
#' @export
getEvals = function(evaluator=NA, pmid=NA, before="9999-12-31", after="0001-01-01" ) {
  params = list(before=before, after=after)
  query = "SELECT ans.pmid, ans.evaluator, ans.timestamp, evs.model, pr.name as query, ans.answer " %_%
          "FROM Evals as ans, Evaluators as evs, Prompts as pr " %_%
          "WHERE ans.evaluator==evs.id AND evs.prompt==pr.id " %_%
          "AND ans.timestamp <= :before AND ans.timestamp >= :after "
  
  if(! is.single.NA(evaluator)) {
    evaluator = paste0(evaluator, ",")
    query = query %_% sprintf("AND evaluator IN (%s) ", evaluator)
  }
  
  if(! is.single.NA(pmid)) {
    pmid = paste0(pmid, collapse=",")
    query = query %_% sprintf("AND pmid IN (%s) ",  pmid)
  }
  print(query)
  print(params)
  return(DBI::dbGetQuery(settings$dbCon, query, params))
}

insertHits = function(db, articles) {
  queryid = attr(articles, "queryid")
  if (is.null(queryid)) {
    warning("Articles not saved as query hits - " %_% 
              "article data.frame had no queryid attribute")
    return()
  }
  
  existing = DBI::dbGetQuery(db, 
    "SELECT pmid FROM Hits WHERE query = :queryid", list(queryid=queryid)
  )$pmid
  articles = articles[! articles$pmid %in% existing, ]
  if (nrow(articles) > 0) {
    articles$query = queryid
    stm = DBI::dbSendStatement(db, "INSERT INTO Hits (pmid, query) VALUES (:pmid, :query)")
    DBI::dbBind(stm, articles[, c("pmid", "query")])
    DBI::dbClearResult(stm)
  }
  printf("Inserted %i search query hits\n", nrow(articles))
}

insertArticles = function(db, articles) {
  existing = DBI::dbGetQuery(db, "SELECT pmid FROM Articles")$pmid
  new = articles[! articles$pmid %in% existing, ]
  if(nrow(new) > 0) {
    stm = DBI::dbSendStatement(db,
      "INSERT INTO Articles ( pmid,  pubtype,  pubdate,  doi,  journal,  title,  authors,  summary)
                   VALUES   (:pmid, :pubtype, :pubdate, :doi, :journal, :title, :authors, :summary)"
      )
    DBI::dbBind(stm, new)
    DBI::dbClearResult(stm)
  }
  printf("Inserted %i new articles\n", nrow(new))
}

#' Get articles from the DB
#' 
#' @param from Start date (inclusive)
#' @param to End date (inclusive)
#' @export
getArticles = function(from="1000-01-01", to="3000-01-01") {
  db = settings$dbCon
  args = list(from=from, to=to)
  sql = "SELECT * FROM Articles a WHERE a.pubdate >= :from AND a.pubdate <= :to"
  res = DBI::dbGetQuery(db, sql, args)
  return(res)
}
