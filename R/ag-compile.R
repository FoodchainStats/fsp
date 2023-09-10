#' Title
#'
#' @return
#' @export
#'
#' @examples
compile_ag <- function() {

w <- acquire_ag_workforce()

ag <- get_ag_workforce(w)

ag_gb <- ag |> 
  dplyr::filter(country %in% c("England", "Wales", "Scotland"),
                category == "Total labour force (incl. farmers and spouses)") |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(value = sum(value)) |> 
  dplyr::mutate(date = lubridate::ymd(year, truncated = 2) + months(2),
                sector = "Agriculture", value = value/1000) |> 
  dplyr::select(-year)

mindate <- min(ag_gb$date)
maxdate <- max(ag_gb$date)

z <- data.frame(date = seq.Date(mindate, maxdate, by = "3 month")) |>
  dplyr::left_join(ag_gb) |> 
  tidyr::fill(c(value, sector))

return(z)

}

