#' Convert Pubmed to PMC IDs
#'
#' @param pmid One or mode PMIDs to convert
#' 
#' Returns a vector of the same length as the input, 
#' with corresponding PMC IDs or NA.
#'
#' @export
pmid2pmcid = function(pmid) {
  if(is.single.NA(pmid)) {
    return(NA)
  }
  
  if(length(pmid) > 1) {
    pmc = rentrez::entrez_link("pubmed", id = pmid, db = "pmc", by_id = T )
    pmc = lapply(pmc, function(p) {
      ifelse("pubmed_pmc" %in% names(p$links), p$links$pubmed_pmc, NA)
    } )
    return(unlist(pmc))  
  } else {
    pmc = rentrez::entrez_link("pubmed", id = pmid, db = "pmc")
    return( ifelse("pubmed_pmc" %in% names(pmc$links), pmc$links$pubmed_pmc, NA))
  }
}

parsePMC = function(node) {
  if(! "XMLInternalDocument" %in% class(node)) {
   return(data.frame(pmc=NA, pmid=NA, body=NA))
  }
  pmc  = XML::xpathApply(node, 
                         "//article/front/article-meta/article-id[@pub-id-type='pmcid']", 
                         XML::xmlValue
                         ) |> unlist() |> coalesce(NA)
  pmid = XML::xpathApply(node, 
                         "//article/front/article-meta/article-id[@pub-id-type='pmid']", 
                         XML::xmlValue
                         ) |> unlist() |> coalesce(NA)
  body = XML::xpathApply(node, 
                         "//article/body", 
                         XML::saveXML
                         ) |> unlist() |> coalesce(NA)
  return(data.frame(pmc, pmid, body))
}



fetchPMC = function(pmcid) {
  block = rentrez::entrez_fetch("pmc", id=pmcid, rettype="xml")
  parts = XML::xpathApply(XML::xmlInternalTreeParse(block), "//pmc-articleset/article", XML::saveXML)
  articles = lapply(parts, XML::xmlInternalTreeParse)
  article_rows = lapply(articles, parsePMC)
  article_tbl = Reduce(rbind, article_rows)
  
  article_tbl$pmc = gsub("PMC", "", article_tbl$pmc)
  
  res = data.frame(pmc = pmcid)
  res = merge(res, article_tbl, by="pmc", all.x=TRUE)
}

