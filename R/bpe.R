#' Title
#'
#' @param url 
#'
#' @returns
#' @export
#'
#' @examples
compile_bpe <- function(url) {
  
  tmp <- tempfile()
  utils::download.file(url, tmp)
  file <- tmp
  
  cells <- tidyxl::xlsx_cells(file, sheets = "Table 6")
  
  formats <- tidyxl::xlsx_formats(file)
  
  bold <- formats$local$font$bold
  
  
  out <- cells |> 
    dplyr::filter(row >= 7) |> 
    dplyr::filter(!is_blank) |> 
    unpivotr::behead("up", "category") |> 
    unpivotr::behead_if(bold[local_format_id] == TRUE,
                        direction = "left-up",
                        name = "sic_desc") |> 
    unpivotr::behead("left", "size") |> 
    dplyr::mutate(sic_id = stringr::str_extract(sic_desc, "[0-9][0-9]")) |> 
    dplyr::select(category, sic_id, sic_desc, size, numeric)
  
  return(out)
}