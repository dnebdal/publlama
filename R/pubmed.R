parseArticleXML = function(xml) {
  getNodeSet = XML::getNodeSet
  xpathApply = XML::xpathApply
  xmlValue   = XML::xmlValue

  pdate = getNodeSet(xml, "PubmedData/History/PubMedPubDate[@PubStatus='pubmed']")[[1]]
  year  = xpathApply(pdate, "Year", xmlValue)
  month = xpathApply(pdate, "Month", xmlValue)
  day   = xpathApply(pdate, "Day", xmlValue)
  pubDate = sprintf("%i-%02i-%02i", as.integer(year), as.integer(month), as.integer(day))

  doiXML = xpathApply(xml, "MedlineCitation/Article/ELocationID[@EIdType='doi']", xmlValue)
  doi = ifelse(length(doiXML) == 0, "", doiXML[[1]])

  pubtypes = xpathApply(xml, "MedlineCitation/Article/PublicationTypeList/PublicationType", xmlValue)
  pubtypes = paste(pubtypes, collapse=";")

  abstracts = getNodeSet(xml, "MedlineCitation/Article/Abstract/AbstractText")
  abstracts = unlist(lapply(abstracts, XML::saveXML))
  summary = paste(abstracts, collapse=" ")

  authors = getNodeSet(xml, "MedlineCitation/Article/AuthorList/Author")
  authornames = lapply(authors, function(x){
    lastName = xmlValue(x[["LastName"]])
    initials = xmlValue(x[["Initials"]])
    return(sprintf("%s, %s", lastName, initials))
    } )
  authorList = paste(unlist(authornames), collapse="; ")

  return(data.frame(
      pmid = as.integer(xpathApply(xml, "MedlineCitation/PMID", xmlValue)[[1]]),
      pubtype = pubtypes,
      pubdate = pubDate,
      doi = doi,
      journal = xpathApply(xml, "MedlineCitation/Article[1]/Journal/ISOAbbreviation", xmlValue)[[1]],
      authors = authorList,
      title = xpathApply(xml, "MedlineCitation/Article[1]/ArticleTitle", xmlValue)[[1]],
      summary = summary
  ))
}

#' Performs a named pubmed query, stores the results
#' 
#' @param queryname Name of the query to run
#'
#' Queries and their names are defined in the settings file, see publlamaInit().
#' 
#' @export
searchPubmed = function(queryname) {
  retmax=100
  db = settings$dbCon
  query = getQueryByName(queryname)
  if (is.na(query)) return(NA) # raise an error instead, maybe?

  searchRes = rentrez::entrez_search(db="pubmed", term=query, use_history=TRUE)
  N = searchRes$count
  starts = seq(0, N-1, retmax)

  printf("Fetching %i PMIDs in %i blocks of %i for query %s:\n",
         N, length(starts), retmax, queryname)

  res = pbmcapply::pbmclapply(starts, function(start) {
    rentrez::entrez_fetch(
      db="pubmed",
      web_history=searchRes$web_history,
      retstart=start,
      retmax=retmax,
      rettype="xml"
    )
  }, mc.cores=1)

  # At this point, res is a list of text blocks,
  # each of which contains <retmax> articles inside an article set.
  # We want this converted to a list of XML objects, one per article
  # and then parsed into a data.frame
  res = lapply(res, function(block){
    XML::getNodeSet(XML::xmlInternalTreeParse(block), "//PubmedArticleSet/PubmedArticle")
  } )
  res = do.call(c, res)
  res = do.call(rbind, lapply(res, parseArticleXML))
  attr(res, "queryname") <- queryname
  attr(res, "queryid")   <- attr(query, "id")
  
  insertArticles(settings$dbCon, res)
  insertHits(settings$dbCon, res)
  
  return(res)
}

