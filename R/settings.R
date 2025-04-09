# Package-local "global"
settings <- new.env()

readSettings <- function(settingsFile="settings.xml") {
    sx = XML::xmlInternalTreeParse(settingsFile)

    # Entrez key
    entrezSettings = unlist(lapply(XML::xpathApply(sx, "//settings/entrez/*"), XML::xmlName))
    if ("file" %in% entrezSettings) {
        keytxt <- readLines(XML::xpathApply(sx, "//settings/entrez/file", XML::xmlValue)[[1]])
        settings$entrez_key = strip(keytxt)
    } else if ("key" %in% entrezSettings) {
        keytxt <- XML::xpathApply(sx, "//settings/entrez/key", XML::xmlValue)[[1]]
        settings$entrez_key = strip(keytxt)
    }

    # LLM settings
    runners = XML::xpathApply(sx, "//settings/endpoints/endpoint", XML::xmlValue)
    runners = lapply(runners, \(x) gsub("/$", "", x))
    runnerNames = XML::xpathApply(sx, "//settings/endpoints/endpoint", XML::xmlGetAttr, "name")
    names(runners) = unlist(runnerNames)

    settings$endpoints = lapply(runnerNames, \(name) {
        res <- list(endpoint = unlist(runners[[name]]), name = name)
        res$models = getModelsFromEndpoint(res$endpoint)
        res
    })

    # LLM prompts
    promptNames = unlist(XML::xpathApply(sx, "//settings/llm/prompts/prompt", XML::xmlGetAttr, "name"))
    prompts = lapply(XML::xpathApply(sx, "//settings/llm/prompts/prompt", XML::xmlValue), strip)
    names(prompts) = promptNames
    settings$prompts = prompts

    # PubMed queries
    queryNames = unlist(XML::xpathApply(sx, "//settings/queries/query", XML::xmlGetAttr, "name"))
    queryTypes = unlist(XML::xpathApply(sx, "//settings/queries/query", XML::xmlGetAttr, "type"))
    queries = XML::xpathApply(sx, "//settings/queries/query", XML::xmlValue)
    sq <- Map(function(n, t, q) {
        list(type = t, query = q)
    }, queryNames, queryTypes, queries)
    settings$queries <- sq


    return(settings)
}
