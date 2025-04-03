#' Test whether a string is a valid Pocketbook indicator id
#'
#' @description
#' A Pocketbook indicator can be uniquely identified from 2 hierarchical
#' components separated by periods:
#' 
#' - section (1-9)
#' - indicator (1-99)
#' 
#' 
#' @param string String containing a Pocketbook indicator id
#'
#' @return A logical
#' @seealso [parse_indicator()] extracts the components of an indicator id
#' @family Helpers
#' @export
#'
#' @examples
#' # A valid id
#' check_indicator("2.3")
#' 
#' # An invalid id
#' check_indicator("10.3")
check_indicator <- function(string) {
  # check that the first chars fit the pattern
  check <- stringr::str_starts(string, pattern = "[1-9]\\.[1-9][0-9]?")
  # fail if there are extra chars at the end
  if(check) {
    if(nchar(stringr::str_extract(string, pattern = "[1-9]\\.[1-9][0-9]?")) == nchar(string)){
      check = TRUE
    } else{
      check = FALSE
    }
    
    
  }
  return(check)
}


# Remove punctuation and make camel case
clean_string <- function(string) {
  string <- stringr::str_to_lower(string)
  string <- stringr::str_remove_all(string, "[:punct:]")
  string <- stringr::str_replace_all(string, " ", "_")
  return(string)
}


make_filename <- function(id, desc) {
  desc <- clean_string(desc)
  check <- check_indicator(id)
  if(check == FALSE) {
    rlang::abort(paste(id, "is not a valid indicator id. See`check_indicator`."))
  }
  id<- stringr::str_split_1(id, pattern = "\\.")
  id <- stringr::str_flatten(id, collapse = "_")
  filename <- paste(id, desc, stringr::str_replace_all(Sys.Date(),"-","_"), sep = "_")
  return(filename)
}


#' Parse a Pocketbook indicator id into its components
#'
#' @param indicator_id A valid Pocketbook indicator id
#'
#' @return A named list of the indicator id components
#' @seealso [check_indicator()] tests whether an indicator id is valid
#' @family Helpers
#' @export
#'
#' @examples
#' i <- parse_indicator("3.2")
#' i$section
#' i$indicator
parse_indicator <- function(indicator_id) {
  
  check <- check_indicator(indicator_id)
  if(check == FALSE) {
    rlang::abort(paste(indicator_id, "is not a valid indicator id. See`check_indicator`."))
  }
  
    s <- stringr::str_sub(indicator_id,1,1)
    i <- stringr::str_extract(stringr::str_sub(indicator_id, 3, stringr::str_length(indicator_id)), "[0-9]*")
  
  indicator <- list(section = s, 
                    indicator = i)
  return(indicator)
  
}


#' Save a Pocketbook graphic to the S3 bucket
#'
#' Graphics are saved into a folder structure on the bucket. This is currently
#' 'section_x/iS_I/output/graphics'. Filenames are derived from the indicator
#' id, description and curent date. The bucket location is encoded in
#' [s3_bucket()].
#'
#' @param graphic A `ggplot` chart object
#' @param indicator_id A valid Pocketbook indicator id. See [check_indicator()].
#' @param indicator_desc A title for the graphic
#'
#' @return Saves a png and svg file to the S3 bucket. The filename will be the
#'   indicator id, description and current date (with all punctuation removed),
#'   delimited by underscores '_'.
#' @seealso [save_csv()] for saving data
#' @family Helpers
#' @export
#'
#' @examples
#' \dontrun{
#' chart <- ggplot2::ggplot(mtcars) + 
#' ggplot2::geom_point(ggplot2::aes(x = mpg, y = wt))
#' 
#' save_graphic(chart, "1.1", "Test graphic")
#' #file created will be '1_1_test_graphic.png/svg'
#' }
save_graphic <- function(graphic, indicator_id, indicator_desc = "") {
  gtype <- class(graphic) %in% c("gg", "ggplot", "patchwork") 
  if(!all(gtype)) {
    rlang::abort("object must be a ggplot graphic")
  }
  fname <- make_filename(indicator_id, indicator_desc)
  id <- parse_indicator(indicator_id)
  s3path <- paste0("section_", id$section, "/i", id$section, "_", id$indicator, "/output/graphics/", fname)
  tmp <- tempfile()
  
  png <- suppressMessages(aws.s3::object_exists(paste0(s3path, ".png"), s3_bucket()))
  svg <- suppressMessages(aws.s3::object_exists(paste0(s3path, ".svg"), s3_bucket()))
  
  if(png|svg) {
    query <- readline(prompt = "A file with this name already exists. Overwrite? (Y/N)")
    if(tolower(query) != "y" ) {
      rlang::abort("Graphics not saved")
    }
  }
  
  ggplot2::ggsave(tmp, plot = graphic, device = "png", width = 960/72, height = 640/72, dpi = 72)
  aws.s3::put_object(tmp,
                     object = paste0(s3path, ".png"),
                     bucket = s3_bucket(),
                     headers = c("x-amz-acl" = "bucket-owner-full-control"))
  
  ggplot2::ggsave(tmp, plot = graphic, device = "svg", width = 960/72, height = 640/72, dpi = 72)
  aws.s3::put_object(tmp,
                     object = paste0(s3path, ".svg"),
                     bucket = s3_bucket(),
                     headers = c("x-amz-acl" = "bucket-owner-full-control"))
  
  # return(fname)
  rlang::inform(c("Saving file",
                  paste0(s3_bucket(), "/", s3path, ".png"), 
                  paste0(s3_bucket(), "/", s3path, ".svg")))
}



#' Save Pocketbook data for publication to the S3 bucket
#'
#' Data is saved as a csv into a folder structure on the bucket. This is
#' currently 'section_x/iS_I/output/graphics'. Filenames are derived from the
#' indicator id, description and current date. The bucket location is encoded in
#' [s3_bucket()].
#'
#' @param data A data frame
#' @param indicator_id A valid Pocketbook indicator id. See [check_indicator()].
#' @param indicator_desc A title for the graphic
#'
#' @return Saves a png and svg file to the S3 bucket. The filename will be the
#'   indicator id, description and current date (with all punctuation removed),
#'   delimited by underscores '_'.
#' @seealso [save_graphic()] for saving images 
#' @family Helpers
#' @export
#'
#' @examples
#' \dontrun{
#' save_csv(mtcars, "1.1", "mtcars data")
#' }
save_csv <- function(data, indicator_id, indicator_desc = "") {
  if(!any(class(data) %in% c("data.frame", "tbl_df", "tbl"))){
    rlang::abort("object is not a data frame")
  }
  
  fname <- make_filename(indicator_id, indicator_desc)
  id <- parse_indicator(indicator_id)
  s3path <- paste0("section_", id$section, "/i", id$section, "_", id$indicator, "/output/csv/", fname)
  
  csv <- suppressMessages(aws.s3::object_exists(paste0(s3path, ".csv"), s3_bucket()))
  
  if(csv) {
    query <- readline(prompt = "A file with this name already exists. Overwrite? (Y/N)")
    if(tolower(query) != "y" ) {
      rlang::abort("Data not saved")
      
    }
  }
  
  aws.s3::s3write_using(data, 
                        FUN = utils::write.csv, 
                        object = paste0(s3path, ".csv"),
                        bucket = s3_bucket(),
                        opts = list("headers" =list("x-amz-acl" = "bucket-owner-full-control")),
                        row.names = FALSE)
  
  rlang::inform(c("Saving file",
                  paste0(s3_bucket(), "/", s3path, ".csv")))
  
  
}
