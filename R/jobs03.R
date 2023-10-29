#' Extract data from a JOBS03 xls file
#'
#' @param file A JOBS03 xls file. If omitted, file will be downloaded
#' @param sheet Spreadsheet tab name or number
#'
#' @return
#' @export
#'
#' @examples
get_jobs03_data <- function(file, sheet = "8. GB Totals"){
  
  if(missing(file)) {
    jobs03 <- acquire_jobs03()
  } else {
    jobs03 <- file
  }
  
  # excel_format reports ODS files as xls, but the function was crashing when
  # reading the data, so exclude ods files for now pending a resolution
  # 
  if(is.na(readxl::excel_format(jobs03)) || stringr::str_ends(jobs03, "ods")) {
    stop(paste(jobs03, "is not an xls or xlsx spreadsheet file"))
  } else if (any(grepl("8. GB Totals", readxl::excel_sheets(jobs03))) == FALSE) {
    stop(paste(jobs03,
               "does not contain a tab named `8. GB Totals`. \n",
               "If your file does contain a valid JOBS03 tab, please specify the sheet name or number."))
  }
  
  data <- suppressMessages(
          readxl::read_excel(jobs03, 
                             sheet = "8. GB Totals",
                             skip = 2,
                             col_types = c("text")) 
                            )  
                           
  out <- data|> 
    dplyr::mutate(date = jobs03_date(.data$`...1`)) |>
    dplyr::select(-.data$`...1`, -.data$`...2`) |> 
    dplyr::relocate(date) |> 
    unpivotr::as_cells() |> 
    unpivotr::behead("left", date) |> 
    unpivotr::behead("up", "industry") |> 
    unpivotr::behead("up-left", "sic_section") |> 
    unpivotr::behead("up", "sic_division") |> 
    dplyr::select(date = .data$date.header, 
                  .data$sic_section, 
                  .data$sic_division, 
                  .data$industry, 
                  value = .data$chr) |> 
    dplyr::filter(!is.na(date)) |> 
    dplyr::mutate(value = as.numeric(.data$value),
                  sic_division = dplyr::case_when(
                    stringr::str_length(.data$sic_division) >10 ~ 
                    sprintf("%.*f", 2, as.numeric(.data$sic_division)),
                    .data$sic_section == "G-T" ~ "45-98",
                    .data$sic_section == "A-T" ~ "01-98",
                                       .default = .data$sic_division
                    )
                  )

  return(out)
  
}
