#' URL for the current JOBS03 dataset
#'
#' @return The url for JOBS03.
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
#' @return The url for the landing page that contains the dataset.
#'   [get_jobs03_data()] uses this function and some webscraping to download and
#'   extract thte actual data file.
#' @export
#'
#' @examples
#' url_ag_workforce()
url_ag_workforce <- function() {
  url <- "https://www.gov.uk/government/statistical-data-sets/agricultural-workforce-in-the-united-kingdom-at-1-june"
  return(url)
}