#' Get UNCTAD data
#'
#' @param file An UNCTAD bulk download csv
#'
#' @return A tibble of UNCTAD commodity price data
#' @family {UNCTAD}
#' @export
#'
#' @examples
#' \dontrun{
#' get_unctad()
#' }
get_unctad <- function(file){

  if(missing(file)) {
    file <- acquire_unctad()
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
#' @return A tibble of UNCTAD commodity price metadata
#' @family {UNCTAD}
#' @export
#'
#' @examples
#' \dontrun{
#' get_unctad_metadata()
#' }
get_unctad_metadata <- function() {
  
  data <- get_unctad()

  commodities <- data |> 
    dplyr::count(.data$commodity, .data$commodity_label)  
  
  return(commodities)
}