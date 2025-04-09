
initdb = function(dbFilename) {
  db = DBI::dbConnect(RSQLite::SQLite(), dbFilename)
  dbTables = DBI::dbGetQuery(db, "PRAGMA table_list")$name

  createFile = system.file("extdata/create.sql.xml", package="publlama")
  
  createStatements_xml = XML::xmlInternalTreeParse(createFile)
  createStatements = XML::xpathApply(createStatements_xml, "//create/table", XML::xmlValue)
  tables = unlist(XML::xpathApply(createStatements_xml, "//create/table", XML::xmlGetAttr, "name"))

  if (! all(tables %in% dbTables)) {
    # We're missing at least one table
    cat("Creating tables: ")
    lapply(createStatements, function(stm) {DBI::dbExecute(db, stm)} )
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
    list(x=checksum, n=name)
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

getOrRegisterEvaluator = function(name, model, promptID) {
  pid = getPrompt(promptID)$id
  
  res = DBI::dbGetQuery(
    settings$dbCon,
    "SELECT * FROM Evaluators WHERE name = :n AND model = :m AND prompt = :p",
    list(n=name, m=model, p=pid)
  )
  
  if(nrow(res) == 0) {
    stm = DBI::dbSendQuery(
      settings$dbCon, 
      "INSERT INTO Evaluators (human, prompt, model, name) VALUES (FALSE, :p, :m, :n) RETURNING *",
      list(p=promptID, m=model, n=name)
    )
    res = DBI::dbFetch(stm)
    DBI::dbClearResult(stm)
  }
  
  return(res)
}

insertHits = function(db, articles) {
  queryid = attr(articles, "queryid")
  existing = DBI::dbGetQuery(db, "SELECT pmid FROM Hits WHERE query = :queryid", list(queryid=queryid))$pmid
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
