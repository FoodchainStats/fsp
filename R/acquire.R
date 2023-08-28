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