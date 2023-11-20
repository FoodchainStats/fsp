test_that("multiplication works", {
  skip_on_cran()
  # test fails on windows in gh actions, seems to be readxl/readOS and unzip
  # failure - no access to windows machine to investigate
  skip_on_os("windows")
  
  df <- compile_ag_workforce()
  
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) == 3)
  
})
