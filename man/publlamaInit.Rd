% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{publlamaInit}
\alias{publlamaInit}
\title{Initialize the publlama package before use}
\usage{
publlamaInit(
  dbFile = "publlama_db/database.sqlite",
  settingsFile = "publlama_db/settings.xml",
  entrezKey = NA,
  entrezKeyFile = NA
)
}
\arguments{
\item{dbFile}{Where to keep the article database}

\item{settingsFile}{Where to store the settings}

\item{entrezKey}{Use this Entrez API key}

\item{entrezKeyFile}{Read the Entrez api key from this file

The settings.xml file is used to define the queries
and ollama-style endpoints, so be prepared to edit it to your needs.
You can also set the entrez key or keyfile in the settings file.}
}
\description{
Initialize the publlama package before use
}
