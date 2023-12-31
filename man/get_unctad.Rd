% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unctad.R
\name{get_unctad}
\alias{get_unctad}
\title{Get UNCTAD data}
\usage{
get_unctad(file)
}
\arguments{
\item{file}{An UNCTAD 7zip download file (see \code{\link[=url_unctad]{url_unctad()}} for where to get
it)}
}
\value{
A tibble of UNCTAD commodity price data
}
\description{
Get UNCTAD data
}
\examples{

a <- system.file("extdata", "unctad-example.csv.7z", package = "fsp")
a
get_unctad(a)

}
\seealso{
Other {UNCTAD}: 
\code{\link{acquire_unctad}()},
\code{\link{get_unctad_metadata}()},
\code{\link{url_unctad}()}
}
\concept{{UNCTAD}}
