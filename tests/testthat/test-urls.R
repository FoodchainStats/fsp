test_that("url_jobs03 returns an xls", {
  url <- url_jobs03()
  
  expect_equal(stringr::str_sub(url, -3, -1), "xls")
  
})


test_that("url_ag_workforce returns an ods", {
  url <- url_ag_workforce()
  
  expect_equal(stringr::str_sub(url, -3, -1), "ods")
  
})


test_that("url_unctad returns a 7z", {
  skip(message = "Skipping UNCTAD until fixed")
  url <- url_unctad()
  
  expect_equal(stringr::str_sub(url, -2, -1), "7z")
  
})
