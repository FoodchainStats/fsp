% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/urls.R
\name{url_bpe}
\alias{url_bpe}
\title{URL for Business Population Estimates}
\usage{
url_bpe(year = 2024)
}
\arguments{
\item{year}{The year to get data for (> 2012)}
}
\value{
The url for the BPE collection page
}
\description{
Returns a URL for a \href{https://www.gov.uk/government/collections/business-population-estimates}{Business Population Estimates}
detailed MS Excel spreadsheet. The URL is generated using the year parameter
and depends on gov.uk page naming consistency.
}
\examples{
url_bpe()
}
\seealso{
Other Business Population Estimates: 
\code{\link{acquire_bpe}()},
\code{\link{get_bpe}()}
}
\concept{Business Population Estimates}
