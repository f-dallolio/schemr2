% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auto_add_ns0.R
\name{auto_add_ns0}
\alias{auto_add_ns0}
\title{Automatically add namespace to a call.}
\usage{
auto_add_ns0(x, ..., .match_call = TRUE, .match_call_defaults = TRUE)
}
\arguments{
\item{x}{a \code{call_simple}.}

\item{...}{.}

\item{.match_call}{a logical. if \code{TRUE}, \code{call_match} is also called. Defaults to \code{TRUE}.}

\item{.match_call_defaults}{a logical. Defaults to \code{TRUE}..}
}
\value{
a \code{call_simple}(namespaced).
}
\description{
Automatically add namespace to a call.
}
\examples{
call <- quote(data.frame(x))
call
# data.frame(x)
auto_add_ns0(call)
# base::data.frame(x, row.names = NULL, check.rows = FALSE, check.names = TRUE,
#     fix.empty.names = TRUE, stringsAsFactors = FALSE)
}
