#' Title
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
acquire_jobs03 <- function(path){
  
    if (!missing(path)) {
      if (!dir.exists(path)) 
        stop(paste(path, "does not exist"))
    }
  
    url <- url_jobs03()
    
    if (missing(path)) {
      tmp <- tempfile()
      utils::download.file(url, tmp)
      jobs03file <- tmp
    } else {
      utils::download.file(url, destfile = paste0(path, "/", 
                                                  "jobs03.xls"))
      jobs03file <- paste0(path, "/", "jobs03.xls")
    }
    
    return(jobs03file)
  
  
}




#' Title
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
acquire_ct <- function(path){
  
  if (!missing(path)) {
    if (!dir.exists(path)) 
      stop(paste(path, "does not exist"))
  }
  
  url <- url_ct()
  
  if (missing(path)) {
    tmp <- tempfile()
    utils::download.file(url, tmp)
    ctfile <- tmp
  } else {
    utils::download.file(url, destfile = paste0(path, "/", 
                                                "ct.csv"))
    ctfile <- paste0(path, "/", "ct.csv")
  }
  
  return(ctfile)
  
  
}



#' Title
#'
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
acquire_ag_workforce <- function(path) {
  
  if (!missing(path)) {
    if (!dir.exists(path)) 
      stop(paste(path, "does not exist"))
  }
  
  url <- url_ag_workforce()
  html <- rvest::read_html(url)
  links <- html |> rvest::html_elements("a")
  linktext <- links |> rvest::html_text2()
  linkurls <- links |> rvest::html_attr("href")
  datalink <- which(stringr::str_detect(linktext, "Agricultural workforce in the United Kingdom at 1 June"))
  
  filename <- linkurls[datalink]
  
  if (missing(path)) {
    tmp <- tempfile()
    utils::download.file(filename, tmp)
    ag_workforce_file <- tmp
  } else {
    utils::download.file(filename, destfile = paste0(path, "/", 
                                                "ag_workforce.ods"))
    ag_workforce_file <- paste0(path, "/", "ag_workforce.ods")
  }
  
  return(ag_workforce_file)
}