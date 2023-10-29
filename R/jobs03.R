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
  
  message("Reading JOBS03")
  
  data <- suppressMessages(
          readxl::read_excel(jobs03, 
                             sheet = "8. GB Totals",
                             skip = 2,
                             col_types = c("text")) 
                            )  
# the unvectorised jobs03_date2 function is much slower but doesnt throw
# warnings, whereas jobs03_date does. So I switched it out. See helpers-jobs03.R
# for function details.
  out <- data|> 
    dplyr::select(-"...2") |> 
    unpivotr::as_cells() |> 
    unpivotr::behead("left", "date") |> 
    unpivotr::behead("up", "industry") |> 
    unpivotr::behead("up-left", "sic_section") |> 
    unpivotr::behead("up", "sic_division") |> 
    dplyr::select("date", 
                  "sic_section", 
                  "sic_division", 
                  "industry", 
                  value = "chr") |> 
    dplyr::filter(!is.na(date), stringr::str_length(date) < 15) |> 
    dplyr::mutate(date = purrr::map_chr(.data$date, jobs03_date2, .progress = TRUE)) |>
    dplyr::mutate(date = as.Date(.data$date)) |> 
    # dplyr::mutate(date = jobs03_date(.data$date)) |>
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
