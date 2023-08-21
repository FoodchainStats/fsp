library(rvest)

url <- "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employeejobsbyindustryjobs03"

html <- read_html(url)

link <- html |>
  html_elements(".btn--thick") |> html_attr("href")

file <- paste0("https://www.ons.gov.uk", link)

download.file(url = file, destfile = "~/jobs03.xls")

