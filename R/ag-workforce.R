get_ag_workforce <- function(file) {

  file <- readODS::read_ods(file, sheet = "Ag_workforce_by_country", col_names = FALSE)
    
  cells <- unpivotr::as_cells(file) |> 
    dplyr::filter(dplyr::between(row, 2, 18)) |> 
    # dplyr::select(row, col, data_type, numeric, character, date) |> 
    unpivotr::behead("left", cat1) |> 
    unpivotr::behead("left", cat2) |> 
    unpivotr::behead("left", cat3) 

  title <- 
    dplyr::filter(cells, chr == "England") |> 
    dplyr::select(row, col) |> 
    dplyr::mutate(row = row-1) |> 
    dplyr::inner_join(cells, by = c("row", "col"))


  partitions <- unpivotr::partition(cells, title)


data <- purrr::map(partitions$cells, \(x){
  data_cells <- x |> 
    dplyr::filter(row >=3)
  
  year <- x |> 
    dplyr::filter(row == 2) |> 
    dplyr::select(row, col, year = chr) |> 
    tidyr::fill(year, .direction = "downup")
  
  
  out <- data_cells |> 
    unpivotr::enhead(year, "up") |> 
    unpivotr::behead("up", country) |> 
    dplyr::filter(!is.na(chr)) |> 
    tidyr::fill(cat1, .direction = "down") |> 
    tidyr::fill(cat2, .direction = "down") |> 
    dplyr::mutate(category = paste(cat1, cat2, cat3, sep = " - "),
                  category = stringr::str_remove_all(category, " - NA"),
                  chr = as.numeric(chr)) |> 
    dplyr::select(year, country, category, value = chr)
  
  return(out)
  
}) |> 
  purrr::list_rbind()

return(data)

}