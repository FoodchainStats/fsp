% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acquire.R
\name{acquire_bpe}
\alias{acquire_bpe}
\title{Download Business Population Estimate data}
\usage{
acquire_bpe(year = 2024, path)
}
\arguments{
\item{year}{The year to download data for (> 2012)}

\item{path}{Folder to put the downloaded data in. If missing a tempfile will
be created. If specified the downloaded file will be named
'bpe_year.xlsx'.}
}
\value{
The file path of the downloaded file
}
\description{
Download Business Population Estimate data
}
\examples{
\dontrun{
bpe <- acquire_bpe(year = 2022)

file <- acquire_bpe("~/downloads")
}
}
\seealso{
Other Business Population Estimates: 
\code{\link{get_bpe}()},
\code{\link{url_bpe}()}
}
\concept{Business Population Estimates}
