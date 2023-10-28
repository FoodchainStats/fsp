#' Extract data from an Agricultural workforce ods file
#'
#' @param file A spreadsheet in ods format. If omitted, will be downloaded.
#'
#' @return A tibble of workforce data
#' @export
#'
#' @examples
get_ag_workforce <- function(file) {
  
  if(missing(file)) {
    file <- acquire_ag_workforce()
  } 

  file <- readODS::read_ods(file, sheet = "Ag_workforce_by_country", col_names = FALSE)
    
  cells <- unpivotr::as_cells(file) |> 
    dplyr::filter(dplyr::between(row, 2, 18)) |> 
    # dplyr::select(row, col, data_type, numeric, character, date) |> 
    unpivotr::behead("left", "cat1") |> 
    unpivotr::behead("left", "cat2") |> 
    unpivotr::behead("left", "cat3") 

  title <- 
    dplyr::filter(cells, .data$chr == "England") |> 
    dplyr::select(.data$row, .data$col) |> 
    dplyr::mutate(row = .data$row-1) |> 
    dplyr::inner_join(cells, by = c("row", "col"))


  partitions <- unpivotr::partition(cells, title)


data <- purrr::map(partitions$cells, \(x){
  data_cells <- x |> 
    dplyr::filter(row >=3)
  
  year <- x |> 
    dplyr::filter(row == 2) |> 
    dplyr::select(row, col, year = .data$chr) |> 
    tidyr::fill(year, .direction = "downup")
  
  
  out <- data_cells |> 
    unpivotr::enhead(year, "up") |> 
    unpivotr::behead("up", "country") |> 
    dplyr::group_by(.data$country) |> 
    dplyr::filter(!is.na(.data$chr)) |> 
    tidyr::fill(.data$cat1, .direction = "down") |> 
    tidyr::fill(.data$cat2, .direction = "down") |> 
    dplyr::mutate(category = paste(.data$cat1, .data$cat2, .data$cat3, sep = " - "),
                  category = stringr::str_remove_all(.data$category, " - NA"),
                  chr = as.numeric(.data$chr),
                  country = stringr::str_remove(.data$country, "\\([a-z]\\)")) |> 
    dplyr::select(year, .data$country, .data$category, value = .data$chr)
  
  return(out)
  
}) |> 
  purrr::list_rbind()

return(data)

}