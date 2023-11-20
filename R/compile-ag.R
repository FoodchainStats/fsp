#' Compile GB Agricultural workforce data
#'
#' @return A dataframe of quarterly data
#' @family {Agricultural workforce}
#' @export
#'
#' @examples
#' \dontrun{
#' data <- compile_ag_workforce()
#' }
compile_ag_workforce <- function() {

w <- acquire_ag_workforce()

ag <- get_ag_workforce(w)

ag_gb <- ag |> 
  dplyr::filter(.data$country %in% c("England", "Wales", "Scotland", "Scotland(e)"),
                .data$category == "Total labour force (incl. farmers and spouses)") |> 
  dplyr::group_by(.data$year) |> 
  dplyr::summarise(value = sum(.data$value)) |> 
  dplyr::mutate(date = lubridate::ymd(.data$year, truncated = 2) + months(2),
                sector = "Agriculture", value = .data$value/1000) |> 
  dplyr::select(-"year")

mindate <- min(ag_gb$date)

# extend the max date to the whole of the latest year. But latest jobs03 might
# not run up to December. How to handle?
maxdate <- lubridate::make_date(lubridate::year(max(ag_gb$date)), month = 12, day = 1)

z <- data.frame(date = seq.Date(mindate, maxdate, by = "3 month")) |>
  dplyr::left_join(ag_gb, by = dplyr::join_by(date)) |> 
  tidyr::fill(c("value", "sector")) |> 
  tibble::as_tibble()

return(z)

}

