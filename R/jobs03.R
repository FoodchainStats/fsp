source("~/Team Scotts Dropbox/David Lee/Work/fsp-test/R/urls.R")
source("~/Team Scotts Dropbox/David Lee/Work/fsp-test/R/acquire.R")
source("~/Team Scotts Dropbox/David Lee/Work/fsp-test/R/helpers-jobs03.R")


get_jobs03_data <- function(){

  jobs03 <- acquire_jobs03()

  data <- readxl::read_excel(jobs03, sheet = "8. GB Totals", skip = 2,col_types = c("text")) 
  
  x <- data|> 
    dplyr::mutate(date = jobs03_date(`...1`)) |> 
    dplyr::select(-`...1`, -`...2`) |> 
    dplyr::relocate(date) |> 
    unpivotr::as_cells() |> 
    unpivotr::behead("left", date) |> 
    unpivotr::behead("up", industry) |> 
    unpivotr::behead("up-left", sic_section) |> 
    unpivotr::behead("up", sic_division) |> 
    dplyr::select(date = date.header, sic_section, sic_division, industry, value = chr) |> 
    dplyr::filter(!is.na(date)) |> 
    dplyr::mutate(value = as.numeric(value),
                  sic_division = dplyr::case_when(stringr::str_length(sic_division) >10 ~ sprintf("%.*f", 2, as.numeric(sic_division)),
                                       .default = sic_division)
                  )

  return(x)
  
}