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
   return(data.frame(pmc=NA, pmid=NA, specificUse=NA, license=NA, body=NA))
  }
  pmc  = XML::xpathApply(
    node, 
    "//article/front/article-meta/article-id[@pub-id-type='pmcid']", 
    XML::xmlValue
    ) |> unlist() |> coalesce(NA)
  pmid = XML::xpathApply(
    node, 
    "//article/front/article-meta/article-id[@pub-id-type='pmid']", 
    XML::xmlValue
    ) |> unlist() |> coalesce(NA)
  body = XML::xpathApply(
    node, 
    "//article/body", 
    XML::saveXML
    ) |> unlist() |> coalesce(NA)
  specificUse = XML::xpathApply(
    node, 
    "//article/front/article-meta/permissions/license/ali:license_ref",
    XML::xmlGetAttr, 'specific-use' 
    )|> unlist() |> coalesce(NA)
  license = XML::xpathApply(
    node, 
    "//article/front/article-meta/permissions/license/ali:license_ref",
    XML::xmlValue
    )|> unlist() |> coalesce(NA)

  return(data.frame(pmc, pmid, specificUse, license, body))
}

#' Download one or more fulltext articles from PMC.
#'
#' @param pmcid One or more PMC IDs
#'
#' Returns a data frame with on row per PMC id, same order:
#'  - `pmc` The PMC ID of the article
#'  - `pmid` The PubMed ID of the article
#'  - `specificUse` The specific use the license applies to
#'  - `license` URL to the license the article is available under
#'  - `body` The XML body of the article
#' 
#' If the article is not available in PMC, most rows will be NA.
#' If PMC has the article, but does not allow XML download, the 'body' field
#' will be NA. 
#' 
#' When the full text is available, the 'specificUse' will typically be 'textmining', 
#' and the license some creative commons license, like https://creativecommons.org/licenses/by-nc-nd/4.0/
#' 
#' The body field contains the XML version of the article, inside <body> tags. 
#' It may be missing namespace definitions for e.g. mml: (MathML).
#' @export
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

