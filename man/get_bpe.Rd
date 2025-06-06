% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bpe.R
\name{get_bpe}
\alias{get_bpe}
\title{Extract data from a Business Population Estimate xlsx file}
\usage{
get_bpe(rawfile, year = 2024)
}
\arguments{
\item{rawfile}{A BPE xlsx file. If omitted, file will be downloaded}

\item{year}{If data is to be downloaded, the year required (> 2012)}
}
\value{
A datafrae of BPE data
}
\description{
Extract data from a Business Population Estimate xlsx file
}
\examples{
\dontrun{
# In one go
data <- get_bpe(year = 2021)

# or download first
file <- acquire_bpe(year = 2023)
data <- get_bpe(file)
}
}
\seealso{
Other Business Population Estimates: 
\code{\link{acquire_bpe}()},
\code{\link{url_bpe}()}
}
\concept{Business Population Estimates}
