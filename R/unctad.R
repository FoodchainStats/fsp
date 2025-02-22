#' Get UNCTAD data
#'
#' @param file An UNCTAD 7zip download file (see [url_unctad()] for where to get
#'   it)
#'
#' @return A tibble of UNCTAD commodity price data
#' @family UNCTAD
#' @export
#'
#' @examples
#' 
#' a <- system.file("extdata", "unctad-example.csv.7z", package = "fsp")
#' a
#' get_unctad(a)
#' 
get_unctad <- function(file){

  if(missing(file)) {
    stop("Please supply an UNCTAD 7zip file. See 'url_unctad()' for details of where to get this file")
  }
  
  file <- acquire_unctad(file)
  

  colspec <- readr::cols(
    Period = readr::col_character(),
    `Period Label` = readr::col_character(),
    Commodity = readr::col_character(),
    `Commodity Label` = readr::col_character(),
    Prices = readr::col_double(),
    `Prices Footnote` = readr::col_character(),
    `Prices Missing value` = readr::col_character()
  )
  

data <- readr::read_csv(file, col_types = colspec) |> 
  janitor::clean_names() |> 
  dplyr::mutate(date = lubridate::ymd(.data$period, truncated = 1)) |> 
  dplyr::relocate(date) |> 
  dplyr::select(-"period", -"period_label")

return(data)

}


#' Get UNCTAD metadata
#'
#' @param file An UNCTAD 7zip download file (see [url_unctad()] for where to get
#'   it)
#'
#' @return A tibble of UNCTAD commodity price metadata
#' @family UNCTAD
#' @export
#'
#' @examples

#' a <- system.file("extdata", "unctad-example.csv.7z", package = "fsp")
#' a
#' get_unctad_metadata(a)
get_unctad_metadata <- function(file) {

    if(missing(file)) {
    stop("Please supply an UNCTAD 7zip file. See 'url_unctad()' for details of where to get this file")
  }
  
  data <- get_unctad(file)

  commodities <- data |> 
    dplyr::count(.data$commodity, .data$commodity_label)  
  
  return(commodities)
}