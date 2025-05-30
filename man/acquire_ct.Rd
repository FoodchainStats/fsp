% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acquire.R
\name{acquire_ct}
\alias{acquire_ct}
\title{Download ONS Consumer Trends dataset}
\usage{
acquire_ct(path)
}
\arguments{
\item{path}{Folder to put the downloaded data in. If missing a tempfile will
be created. If specified the downloaded file will be named 'ct.csv'.}
}
\value{
The file path and name of the downloaded file.
}
\description{
Download ONS Consumer Trends dataset
}
\examples{
\dontrun{
jobs03 <- acquire_ct()

file <- acquire_ct("~/downloads")
}
}
\seealso{
Other Consumer Trends: 
\code{\link{get_ct_metadata}()},
\code{\link{get_ct_quarter}()},
\code{\link{get_ct_year}()}
}
\concept{Consumer Trends}
