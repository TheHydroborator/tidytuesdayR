% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tt_load.R
\name{tt_load}
\alias{tt_load}
\title{Load TidyTuesday data from Github}
\usage{
tt_load(x, week, ...)
}
\arguments{
\item{x}{string representation of the date of data to pull, in YYYY-MM-dd format, or just numeric entry for year}

\item{week}{left empty unless x is a numeric year entry, in which case the week of interest should be entered}

\item{...}{pass methods to the parsing functions. These will be passed to ALL files, so be careful.}
}
\value{
tt_data object (list class)
}
\description{
Load TidyTuesday data from Github
}
\examples{
tt_output <- tt_load("2019-01-15")
}
