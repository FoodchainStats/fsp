#' Get UNCTAD data
#'
#' @param file An UNCTAD download file (see [acquire_unctad()]). If omitted data
#'   will be downloaded automatically
#'
#' @return A tibble of UNCTAD commodity price data
#' @family UNCTAD
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_unctad()
#' 
#' acquire_unctad("~/downloads")
#' 
#' get_unctad("~/downloads/commodityprices.csv")
#' }
get_unctad <- function(file){

  if(missing(file)) {
    file <- acquire_unctad()
  } else {
    file
  }

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
#' @param file An UNCTAD download file (see [acquire_unctad()]). If omitted data
#'   will be downloaded automatically
#'
#' @return A tibble of UNCTAD commodity price metadata
#' @family UNCTAD
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- get_unctad_metadata()
#' 
#' acquire_unctad("~/downloads")
#' 
#' get_unctad_metadata("~/downloads/commodityprices.csv")  
#' }
get_unctad_metadata <- function(file) {

    if(missing(file)) {
    # stop("Please supply an UNCTAD 7zip file. See 'url_unctad()' for details of where to get this file")
    data <- get_unctad()
  } else {
    data <- get_unctad(file)
  }
  
  commodities <- data |> 
    dplyr::count(.data$commodity, .data$commodity_label)  
  
  return(commodities)
}