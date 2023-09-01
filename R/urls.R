#' Title
#'
#' @return
#' @export
#'
#' @examples
url_jobs03 <- function() {
  url <- "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employeejobsbyindustryjobs03"
  
  html <- rvest::read_html(url)
  
  link <- html |>
    rvest::html_elements(".btn--thick") |> 
    rvest::html_attr("href")
  
  file <- paste0("https://www.ons.gov.uk", link)
  
  return(file)
  
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
url_ct <- function() {
  url <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/satelliteaccounts/datasets/consumertrends/current/ct.csv"
  return(url)
}



#' Title
#'
#' @return
#' @export
#'
#' @examples
url_ag_workforce <- function() {
  url <- "https://www.gov.uk/government/statistical-data-sets/agricultural-workforce-in-the-united-kingdom-at-1-june"
  return(url)
}