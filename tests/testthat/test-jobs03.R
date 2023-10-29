test_that("get_jobs03_data returns a data frame witha positive length", {
  skip_on_cran()
  
  df <- get_jobs03_data()
  
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) == 5)
})
