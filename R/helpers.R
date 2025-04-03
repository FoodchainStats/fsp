ignore_unused_imports <- function() {
  # To get rid of NOTE about use of ggplot in vignette
  # https://r-pkgs.org/dependencies-in-practice.html#how-to-not-use-a-package-in-imports
  
  ggplot2::ggplot()
}

#' Location of the Pocketbook Amazon S3 bucket 
#' 
#' The bucket is used to store final publication data and graphics, as well as
#' underlying raw data.
#'
#' @return A string containing the bucket location
#' @family Helpers
#' @export
#'
#' @examples
#' s3_bucket()
s3_bucket <- function() {
  bucket <- "s3-ranch-066"
  return(bucket)
}