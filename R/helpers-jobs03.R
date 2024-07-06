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
#' against raw JOBS03 data to extract food sectors of interest.
#'
#' @param include_fishing logical: include the definition of the fishing industry
#' @param include_total logical: include the definition of total UK industry
#' @return A tibble of SIC codes and sectors.
#' @family {JOBS03}
#' @export
#'
#' @examples
#' jobs03_sectors()
jobs03_sectors <- function(include_fishing = TRUE, include_total = TRUE) {
  
  fish <-  ""
  total <- ""
  
  if(!include_fishing) fish <- "Fishing" 
  if(!include_total) total <- "All Industries"
  
  x <- c(fish, total)
  
  sectors <- tibble::tibble(sector = c("Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Wholesale", "Retail", "Retail", "Catering", "Catering", "Catering", "Fishing", "All Industries"),
                            sic_division = c("10.1-5", "10.6-8", "10.9", "11-12", "46.30", "47.20", "47.11", "56.10", "56.20", "56.30", "03", "01-98"))
  
  out <- sectors |> 
    dplyr::filter(! .data$sector %in% x)
  
  return(out)
}



#' Dataset of  AUK JOBS03 SIC codes and food chain sectors
#' 
#' Deprecated. See the parameters of [jobs03_sectors()] for ways of excluding
#' fishing or total industry definitions.
#'
#' @return A tibble of SIC codes and sectors.
#' @family {JOBS03}
#' @export
#'
#' @examples
#' jobs03_AUK_sectors()
jobs03_AUK_sectors <- function(){
  
  sectors <- tibble::tibble(sector = c("Manufacturing", "Manufacturing", "Manufacturing", "Manufacturing", "Wholesale", "Retail", "Retail", "Catering", "Catering", "Catering", "Fishing", "total_industries"),
                            sic_division = c("10.1-5", "10.6-8", "10.9", "11-12", "46.30", "47.20", "47.11", "56.10", "56.20", "56.30", "03","01-98"))
  
  return(sectors)
}


