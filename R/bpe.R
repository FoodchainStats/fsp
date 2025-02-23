#' Extract data from a Business Population Estimate xlsx file
#'
#' @param rawfile A BPE xlsx file. If omitted, file will be downloaded
#' @param year If data is to be downloaded, the year required (> 2012) 
#'
#' @returns A datafrae of BPE data
#' @family Business Population Estimates
#' @export
#'
#' @examples
#' \dontrun{
#' # In one go
#' data <- get_bpe(year = 2021)
#' 
#' # or download first
#' file <- acquire_bpe(year = 2023)
#' data <- get_bpe(file)
#' }
get_bpe <- function(rawfile, year = 2024) {
  
  if(!missing(rawfile)){
    if(!file.exists(rawfile)) stop(paste(rawfile, "does not exist"))
  }
  
  if(missing(rawfile)){
    url <- url_bpe(year = year)
    tmp <- tempfile()
    utils::download.file(url, tmp)
    rawfile <- tmp
  }  

  cells <- tidyxl::xlsx_cells(rawfile, sheets = "Table 6")
  formats <- tidyxl::xlsx_formats(rawfile)
  
  bold <- formats$local$font$bold
  
  
  out <- cells |> 
    dplyr::filter(row >= 7) |> 
    dplyr::filter(!.data$is_blank) |> 
    unpivotr::behead("up", "category") |> 
    unpivotr::behead_if(bold[.data$local_format_id] == TRUE,
                        direction = "left-up",
                        name = "sic_desc") |> 
    unpivotr::behead("left", "size") |> 
    dplyr::mutate(sic_id = stringr::str_extract(.data$sic_desc, "[0-9][0-9]")) |> 
    dplyr::select("category", "sic_id", "sic_desc", "size", numeric)
  
  return(out)
}