#' Extract data from an Agricultural workforce ods file
#'
#' @param file A spreadsheet in ods format. If omitted, will be downloaded using
#'   [acquire_ag_workforce()].
#' @param sheet Spreadsheet tab name or number
#'
#' @return A tibble of workforce data
#' @family Agricultural workforce
#' @export
#'
#' @examples
#' \dontrun{
#' # download and extract in one go
#' ag <- get_ag_workforce()
#' 
#' # or download separately and extract 
#' ag <- acquire_ag_workforce()
#' data <- get_ag_workforce(ag)
#' 
#' }
get_ag_workforce <- function(file, sheet = "Ag_workforce_by_country") {
  
  if(missing(file)) {
    file <- acquire_ag_workforce()
  } 

  file <- suppressMessages(
    readODS::read_ods(file, sheet = sheet, col_names = FALSE)
  )
  
  cells <- unpivotr::as_cells(file) |> 
    dplyr::filter(dplyr::between(row, 2, 18)) |> 
    # dplyr::select(row, col, data_type, numeric, character, date) |> 
    unpivotr::behead("left", "cat1") 

  title <- 
    dplyr::filter(cells, .data$chr == "England") |> 
    dplyr::select("row", "col") |> 
    dplyr::mutate(row = .data$row-1) |> 
    dplyr::inner_join(cells, by = c("row", "col"))


  partitions <- unpivotr::partition(cells, title)


data <- purrr::map(partitions$cells, \(x){
  data_cells <- x |> 
    dplyr::filter(row >=3)
  
  year <- x |> 
    dplyr::filter(row == 2) |> 
    dplyr::select(row, col, year = "chr") |> 
    tidyr::fill(year, .direction = "downup")
  
  # warnings suppressed in as.numeric because I think its picking up on 'nc' in
  # the spreadsheet - a spurious warning
  out <- data_cells |> 
    unpivotr::enhead(year, "up") |> 
    unpivotr::behead("up", "country") |> 
    dplyr::group_by(.data$country) |> 
    dplyr::filter(!is.na(.data$chr)) |> 
    tidyr::fill("cat1", .direction = "down") |> 
    dplyr::mutate(category = .data$cat1,
                  category = stringr::str_remove_all(.data$category, " - NA"),
                  chr = suppressWarnings(as.numeric(.data$chr)),
                  country = stringr::str_remove(.data$country, "\\([a-z]\\)")) |> 
    dplyr::select(year, "country", "category", value = "chr") |> 
    dplyr::ungroup()
  
  return(out)
  
}) |> 
  purrr::list_rbind()

return(data)

}