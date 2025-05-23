% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{getArticles}
\alias{getArticles}
\title{Get articles from the DB}
\usage{
getArticles(
  from = "1000-01-01",
  to = "3000-01-01",
  types.include = NA,
  types.exclude = NA,
  queries = NA
)
}
\arguments{
\item{from}{Start date (inclusive)}

\item{to}{End date (inclusive)}

\item{types.include}{Only get articles that have at least one of these types}

\item{types.exclude}{Exclude articles that have any of these types}

\item{queries}{Only get articles that were hits for the given query/-ies.

When not specified, arguments are maximally inclusive:
getArticles() with no arguments returns every hit that has been returned
as the result of any query you have run, no matter the article type
or which query, from year 1000 to 9999 (which should cover most of pubmed).}
}
\description{
Get articles from the DB
}
