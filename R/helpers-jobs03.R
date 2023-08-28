jobs03_date <- function(string){
  
  # this is clunky but vectorised, may need more error checking

  out <- dplyr::case_when(
    stringr::str_starts(string, "[0-9][0-9][0-9][0-9][0-9]") == TRUE ~ as.character(as.Date(as.numeric(string), origin = "1899-12-30")),
    stringr::str_starts(string, "[A-Z][a-z][a-z] [0-9][0-9]") == TRUE ~ as.character(lubridate::my(stringr::str_sub(string, 1, 6))),
    .default = ""
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

jobs03_sectors <- function(){
  
  sectors <- tibble::tibble(sector = c("Manufacturing", "Manufacturing", "Wholesale", "Retail", "Retail", "Catering", "Catering", "Catering"),
                            sic = c("10.1-5", "10.6-8", "46.30", "47.20", "47.11", "56.10", "56.20", "56.30"))
  
  return(sectors)
}

