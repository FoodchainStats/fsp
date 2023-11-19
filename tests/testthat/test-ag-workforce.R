test_that("get_ag_workforce returns a data frame with a positive length", {
  skip_on_cran()
  
  df <- get_ag_workforce()
  
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) == 4)
  
})
