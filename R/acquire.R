#' Download ONS JOBS03 dataset
#'
#' @param path Folder to put the downloaded data in. If missing a tempfile will
#'   be created. If specified the downloaded file will be named 'jobs03.xls'.
#'
#' @return The file path and name of the downloaded file.
#' @family JOBS03
#' @export
#'
#' @examples
#' \dontrun{
#' jobs03 <- acquire_jobs03()
#' 
#' file <- acquire_jobs03("~/downloads")
#' }
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




#' Download ONS Consumer Trends dataset
#'
#' @param path Folder to put the downloaded data in. If missing a tempfile will
#'   be created. If specified the downloaded file will be named 'ct.csv'.
#'
#' @return The file path and name of the downloaded file.
#' @family Consumer Trends
#' @export
#'
#' @examples
#' \dontrun{
#' jobs03 <- acquire_ct()
#' 
#' file <- acquire_ct("~/downloads")
#' }
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



#' Download Defra Agricultural Workforce dataset
#' 
#' The downloaded file will be an ODS spreadsheet.
#'
#' @param path Folder to put the downloaded data in. If missing a tempfile will
#'   be created. If specified the downloaded file will be named
#'   'ag_workforce.ods'.
#'
#' @return The file path and name of the downloaded file.
#' @family Agricultural workforce
#' @export
#'
#' @examples
#' \dontrun{
#' jobs03 <- acquire_ag_workforce()
#' 
#' file <- acquire_ag_workforce("~/downloads")
#' }
acquire_ag_workforce <- function(path) {
  
  if (!missing(path)) {
    if (!dir.exists(path)) 
      stop(paste(path, "does not exist"))
  }
  
  url <- url_ag_workforce()

  
  if (missing(path)) {
    tmp <- tempfile()
    utils::download.file(url, tmp)
    ag_workforce_file <- tmp
  } else {
    utils::download.file(url, destfile = paste0(path, "/", 
                                                "ag_workforce.ods"))
    ag_workforce_file <- paste0(path, "/", "ag_workforce.ods")
  }
  
  return(ag_workforce_file)
}




#' Extract UNCTAD Commodity Price indices
#' 
#' Downloads and unzips UNCTAD data (see [url_unctad()]). Internally uses the
#' `archive` library which requires libarchive-dev to be installed on Ubuntu
#' machines.
#'
#' @param path Folder to put the downloaded data in. If missing a tempfile will
#'   be created. If specified the downloaded file will be named
#'   'commodityprices.csv'.
#'
#' @return The file path and name of the downloaded file.
#' @family UNCTAD
#' @export
#'
#' @examples
#' \dontrun{
#' a <- acquire_unctad(a)
#' a
#' 
#' acquire_unctad("~/downloads")
#' }
acquire_unctad <- function(path) {
  
  # if(missing(file)) {
  #   stop("Please supply an UNCTAD 7zip file. See 'url_unctad()' for details of where to get this file")
  # }
  
  if (!missing(path)) {
    if (!dir.exists(path)) 
      stop(paste(path, "does not exist"))
  }
  
  z7 <- tempfile()
  url <- url_unctad()
  req <- httr2::request(url)
  resp <- req |> httr2::req_perform()
  resp |> 
    httr2::resp_body_raw() |> 
    writeBin(z7)
  
  # tmp1 <- tempfile()
  tmpd <- tempfile()
  # utils::download.file(url, tmp1)
  archive::archive_extract(z7, tmpd)
  unctadfile <- list.files(tmpd, full.names = TRUE)
  
  
  if (!missing(path)) {
    file.copy(unctadfile, to = paste0(path, "/", "commodityprices.csv"))
    unctadfile <- paste0(path, "/", "commodityprices.csv")
  }
  
  return(unctadfile)
  
}




#' Download Business Population Estimate data
#'
#' @param year The year to download data for (> 2012)
#' @param path Folder to put the downloaded data in. If missing a tempfile will
#'   be created. If specified the downloaded file will be named
#'   'bpe_year.xlsx'.
#'
#' @returns The file path of the downloaded file
#' @family Business Population Estimates
#' @export
#'
#' @examples
#' \dontrun{
#' bpe <- acquire_bpe(year = 2022)
#' 
#' file <- acquire_bpe("~/downloads")
#' }
acquire_bpe <- function(year = 2024, path) {
  
  if (!missing(path)) {
    if (!dir.exists(path)) 
      stop(paste(path, "does not exist"))
  }
  
  url <- url_bpe(year = year)
  
  if(httr::http_error(url)) {
    stop("Invalid url")
  }

  
  filext <- dplyr::case_when(tools::file_ext(url) == "xls" ~ ".xls",
                             tools::file_ext(url) == "xlsx" ~ ".xlsx",
                             .default = "")
  
  
  if (missing(path)) {
    tmp <- tempfile()
    utils::download.file(url, tmp)
    bpefile <- tmp
  } else {
    utils::download.file(url, destfile = paste0(path, "/", 
                                                "bpe_", year, filext))
    bpefile <- paste0(path, "/","bpe_", year, filext)
  }
  
  return(bpefile)
  
}