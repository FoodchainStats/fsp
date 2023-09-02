#' Title
#'
#' @return
#' @export
#'
#' @examples
get_jobs03_data <- function(file){
  
  jobs03 <- acquire_jobs03()

  data <- readxl::read_excel(jobs03, sheet = "8. GB Totals", skip = 2,col_types = c("text")) 
  
  out <- data|> 
    dplyr::mutate(date = jobs03_date(.data$`...1`)) |> 
    dplyr::select(-.data$`...1`, -.data$`...2`) |> 
    dplyr::relocate(date) |> 
    unpivotr::as_cells() |> 
    unpivotr::behead("left", date) |> 
    unpivotr::behead("up", "industry") |> 
    unpivotr::behead("up-left", "sic_section") |> 
    unpivotr::behead("up", "sic_division") |> 
    dplyr::select(date = .data$date.header, .data$sic_section, .data$sic_division, .data$industry, value = .data$chr) |> 
    dplyr::filter(!is.na(date)) |> 
    dplyr::mutate(value = as.numeric(.data$value),
                  sic_division = dplyr::case_when(
                    stringr::str_length(.data$sic_division) >10 ~ 
                    sprintf("%.*f", 2, as.numeric(.data$sic_division)),
                                       .default = .data$sic_division
                    )
                  )

  return(out)
  
}
