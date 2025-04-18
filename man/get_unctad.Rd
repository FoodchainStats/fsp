% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unctad.R
\name{get_unctad}
\alias{get_unctad}
\title{Get UNCTAD data}
\usage{
get_unctad(file)
}
\arguments{
\item{file}{An UNCTAD download file (see \code{\link[=acquire_unctad]{acquire_unctad()}}). If omitted data
will be downloaded automatically}
}
\value{
A tibble of UNCTAD commodity price data
}
\description{
Get UNCTAD data
}
\examples{
\dontrun{
data <- get_unctad()

acquire_unctad("~/downloads")

get_unctad("~/downloads/commodityprices.csv")
}
}
\seealso{
Other UNCTAD: 
\code{\link{acquire_unctad}()},
\code{\link{get_unctad_metadata}()}
}
\concept{UNCTAD}
