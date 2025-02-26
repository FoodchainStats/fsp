#' URL for the current JOBS03 dataset
#'
#' Scrapes the filename of the latest dataset from
#' [JOBS03](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employeejobsbyindustryjobs03)
#'
#' @return The url for the dataset.
#' @family {JOBS03}
#' @export
#'
#' @examples
#' \dontrun{
#' url_jobs03()
#' }
url_jobs03 <- function() {
  url <- "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employeejobsbyindustryjobs03"
  
  html <- rvest::read_html(url)
  
  link <- html |>
    rvest::html_elements(".btn--thick") |> 
    rvest::html_attr("href")
  
  file <- paste0("https://www.ons.gov.uk", link)
  
  # see dirname and basename to check the filename
  
  return(file)
  
}


#' URL for the current Consumer Trends dataset
#'
#' @return The url for the dataset.
#' @family {Consumer Trends}
#' @export
#'
#' @examples
#' url_ct()
url_ct <- function() {
  url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/satelliteaccounts/datasets/consumertrends/current/ct.csv"
  return(url)
}



#' URL for Defra Agricultural Workforce data
#' 
#' Scrapes the filename of the latest data from the Defra [Agricultural
#' workforce](https://www.gov.uk/government/statistical-data-sets/agricultural-workforce-in-the-united-kingdom-at-1-june)
#' site. The file is an ODS spreadsheet.
#'
#' @return The url for the dataset.
#' @family {Agricultural workforce}
#' @export
#'
#' @examples
#' \dontrun{
#' url_ag_workforce()
#' }
url_ag_workforce <- function() {
  url <- "https://www.gov.uk/government/statistical-data-sets/agricultural-workforce-in-the-united-kingdom-at-1-june"
  
  html <- rvest::read_html(url)
  links <- html |> rvest::html_elements("a")
  linktext <- links |> rvest::html_text2()
  linkurls <- links |> rvest::html_attr("href")
  datalink <- which(stringr::str_detect(linktext, "Agricultural workforce in the United Kingdom at 1 June"))
  
  file <- linkurls[datalink]
  
  return(file)
}




#' URL for UNCTAD commodity price indices data
#'
#' When pasted into a browser, the url will download a file in the form of a
#' 7zip archive. The [acquire_unctad()], [get_unctad()] and
#' [get_unctad_metadata()] functions will download and unzip this file. This
#' package includes an example file - access it using: `system.file("extdata",
#' "unctad-example.csv.7z", package = "fsp")`
#'
#' @return the url for commodity price indices
#' @family {UNCTAD}
#' @export
#'
#' @examples
#' \dontrun{
#' url_unctad()
#' }
url_unctad <- function() {

  # url <- "https://unctadstat.unctad.org/datacentre/"
  url <- "https://unctadstat-api.unctad.org/bulkdownload/US.CommodityPrice_M/US_CommodityPrice_M"
  
  return(url)
  
    
  # url <- "https://unctadstat.unctad.org/EN/BulkDownload.html"
  # 
  # html <- rvest::read_html(url)
  # 
  # links <- html |> rvest::html_elements("a")
  # linktext <- links |> rvest::html_text2()
  # linkurls <- links |> rvest::html_attr("href")
  # datalink <- which(stringr::str_detect(linktext, "Free market commodity prices, monthly"))
  # filename <- linkurls[datalink]
  # 
  # file <- paste0("https://unctadstat.unctad.org", filename)
  # return(file)
  
}



#' URL for the current Annual Business Survey dataset
#' 
#' Returns the latest file from the [ONS](https://www.ons.gov.uk/businessindustryandtrade/business/businessservices/datasets/uknonfinancialbusinesseconomyannualbusinesssurveysectionsas)
#'
#' @return The url for the dataset
#' @family Annual Business Survey
#' @export
#'
#' @examples
#' url_abs()
url_abs <- function() {
  url <- "https://www.ons.gov.uk/file?uri=/businessindustryandtrade/business/businessservices/datasets/uknonfinancialbusinesseconomyannualbusinesssurveysectionsas/current/abssectionsas.xlsx"
  return(url)
}




#' URL for Business Population Estimates
#'
#' Returns a URL for a [Business Population Estimates](https://www.gov.uk/government/collections/business-population-estimates)
#' detailed MS Excel spreadsheet. The URL is generated using the year parameter
#' and depends on gov.uk page naming consistency.
#'
#' @param year The year to get data for (> 2012)
#' 
#' @returns The url for the BPE collection page
#' @family Business Population Estimates
#' @export
#'
#' @examples
#' url_bpe()
url_bpe <- function(year = 2024) {
  
  if(year <=2013 | year >= as.numeric(format(Sys.Date(), "%Y"))) message("Are you sure you've used a valid year (> 2012)?")
  
  url <- paste0("https://www.gov.uk/government/statistics/business-population-estimates-", year)
  
  if(httr::http_error(url)) {
    stop("No valid url is available")
  }
  
  
  html <- rvest::read_html(url)
  links <- html |> rvest::html_elements("a")
  linktext <- links |> rvest::html_text2()
  linkurls <- links |> rvest::html_attr("href")
  
  if(year <=2018) {
    datalink <- which(stringr::str_detect(linktext, "detailed tables"))
  } else {
    datalink <- which(stringr::str_detect(linktext, "detailed tables \\(MS Excel\\)"))
  }
  
  file <- linkurls[datalink]
  if(!endsWith(file, "xlsx")) {
  message("xls files will need to be manually converted to xlsx beforehand. \nSee the options for get_bpe for specifying file location.")
  }
  return(file)
  
}