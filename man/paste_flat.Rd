% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paste_flat.R
\name{paste_flat}
\alias{paste_flat}
\title{Flatten String}
\usage{
paste_flat(..., .before = NULL, c = " ", recycle0 = FALSE)
}
\arguments{
\item{...}{one or more R objects, to be converted to character vectors.}

\item{.before}{a character vector.}

\item{c}{an optional character string to separate the results. Not NA_character_. When collapse is a string, the result is always a string (character of length 1).}

\item{recycle0}{logical indicating if zero-length character arguments should result in the zero-length chara}
}
\value{
a character vector.
}
\description{
Flatten String
}
