jobs03_date <- function(string){
  
  # this is clunky but vectorised, may need more error checking

  out <- dplyr::case_when(
    stringr::str_starts(string, "[0-9][0-9][0-9][0-9][0-9]") == TRUE ~ as.character(as.Date(as.numeric(string), origin = "1899-12-30")),
    stringr::str_starts(string, "[A-Z][a-z][a-z] [0-9][0-9]") == TRUE ~ as.character(lubridate::my(stringr::str_sub(string, 1, 6))),
    .default = NA
  )
  
  out <- as.Date(out)
  return(out)
    
}


jobs03_date2 <- function(string) {
  
  # not vectorised, use mutate(x = purrr::map_chr(y, jobs03_date2))
  
  out <- NA
  
   if(is.na(string)) {
     out <- NA
     return(out)
     }

   if(stringr::str_starts(string, "[0-9][0-9][0-9][0-9][0-9]")){
      out <- as.character(as.Date(as.numeric(string), origin = "1899-12-30"))
   }
    
  
   if(stringr::str_starts(string, "[A-Z][a-z][a-z] [0-9][0-9]")){
      out <- as.character(lubridate::my(stringr::str_sub(string, 1, 6)))
   }
  
  # out <- as.Date(out)
  return(out)
  
}

#' Dataset of JOBS03 SIC codes and food chain sectors
#' 
#' The 'official' SIC definition of the food chain that Defra uses is described
#' in the [pocketbook
#' glossary](https://www.gov.uk/government/statistics/food-statistics-pocketbook/food-statistics-in-your-pocket#glossary).
#' This dataset uses the SIC codes included in JOBS03. It is useful to join
#' against raw JOBS03 data to extract food sectors of innterest.
#'
#' @return A tibble of SIC codes and sectors.
#' @export
#'
#' @examples
#' jobs03_sectors()
jobs03_sectors <- function(){
  
  sectors <- tibble::tibble(sector = c("Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Wholesale", "Retail", "Retail", "Catering", "Catering", "Catering", "Fishing"),
                            sic_division = c("10.1-5", "10.6-8", "10.9", "11-12", "46.30", "47.20", "47.11", "56.10", "56.20", "56.30", "03"))
  
  return(sectors)
}

