% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ct.R
\name{get_ct_metadata}
\alias{get_ct_metadata}
\title{Get Consumer Trends metadata}
\usage{
get_ct_metadata(rawfile)
}
\arguments{
\item{rawfile}{An ONS ct.csv file}
}
\value{
A tibble of series metadata
}
\description{
Get Consumer Trends metadata
}
\examples{
\dontrun{
get_ct_metadata()
}
}
\seealso{
Other Consumer Trends: 
\code{\link{acquire_ct}()},
\code{\link{get_ct_quarter}()},
\code{\link{get_ct_year}()}
}
\concept{Consumer Trends}
