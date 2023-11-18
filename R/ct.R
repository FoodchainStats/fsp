#' Get Consumer Trends metadata
#'
#' @param rawfile An ONS ct.csv file
#'
#' @return A tibble of series metadata
#' @family {Consumer Trends}
#' @export
#'
#' @examples
#' \dontrun{
#' get_ct_metadata()
#' }
get_ct_metadata <- function(rawfile) {
  
  if(!missing(rawfile)){
    if(!file.exists(rawfile)) stop(paste(rawfile, "does not exist"))
  }
  
  if(missing(rawfile)){
    url <- url_ct()
    tmp <- tempfile()
    utils::download.file(url, tmp)
    rawfile <- tmp
  }
  
  metadata <- readr::read_csv(rawfile, n_max = 6, show_col_types = FALSE) |>
    t() |>
    tibble::as_tibble(rownames = "series", .name_repair = "unique") |>
    (\(.) stats::setNames(., .[1,]))() |>
    # setNames( .[1,]) |>
    # rename(series = Title) |>
    dplyr::filter(.data$Title != "Title") |>
    janitor::clean_names() |>
    dplyr::relocate("cdid")
  
  return(metadata)  
  
  
}




#' Get Consumer Trends quarterly data
#'
#' @param rawfile An ONS ct.csv file
#'
#' @return A tibble of quarterly series data
#' @family {Consumer Trends}
#' @export
#'
#' @examples
#' \dontrun{
#' get_ct_quarter()
#' }
get_ct_quarter <- function(rawfile) {
  
  if(!missing(rawfile)){
    if(!file.exists(rawfile)) stop(paste(rawfile, "does not exist"))
  }
  
  if(missing(rawfile)){
    url <- url_ct()
    tmp <- tempfile()
    utils::download.file(url, tmp)
    rawfile <- tmp
  }  
  
  
  ct_quarter <- readr::read_csv(rawfile, skip = 1, show_col_types = FALSE) |>
    dplyr::filter(nchar(.data$CDID) == 7 & .data$CDID != "PreUnit") |>
    dplyr::mutate(CDID = lubridate::yq(.data$CDID)) |>
    dplyr::rename(date = "CDID") |>
    tidyr::pivot_longer(cols = 2:tidyr::last_col(), names_to = "cdid") |>
    dplyr::mutate(value = as.numeric(.data$value),
                  period = "Q") |>
    dplyr::filter(!is.na(.data$value))
  
  # clean up if file was downloaded
  if(missing(rawfile)){
    unlink(rawfile)
  }
  
  return(ct_quarter)
  
}



#' Get Consumer Trends annual data
#'
#' @param rawfile An ONS ct.csv file
#'
#' @return A tibble of annual series data
#' @family {Consumer Trends}
#' @export
#'
#' @examples
#' \dontrun{
#' egt_ct_year()
#' }
get_ct_year <- function(rawfile) {
  
  if(!missing(rawfile)){
    if(!file.exists(rawfile)) stop(paste(rawfile, "does not exist"))
  }
  
  if(missing(rawfile)){
    url <- url_ct()
    tmp <- tempfile()
    utils::download.file(url, tmp)
    rawfile <- tmp
  }  
 
  ct_year <- readr::read_csv(rawfile, skip = 1, show_col_types = FALSE) |>
    dplyr::filter(nchar(.data$CDID) == 4 & .data$CDID != "Unit") |>
    dplyr::mutate(CDID = lubridate::ymd(paste(.data$CDID, "-01-01"))) |>
    dplyr::rename(date = "CDID") |>
    tidyr::pivot_longer(cols = 2:tidyr::last_col(), names_to = "cdid") |>
    dplyr::mutate(value = as.numeric(.data$value),
                  period = "Y") |>
    dplyr::filter(!is.na(.data$value))
  
  # clean up if file was downloaded
  if(missing(rawfile)){
    unlink(rawfile)
  }
  
  return(ct_year)
   
  
}