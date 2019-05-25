context("Check that paper data is downloading")

test_that("DownloadPaperData functions correctly", {
  
  download_raw_paper_data(from_url = FALSE)
  
  expect_true(dir.exists(here::here("working-data", "paper", "raw")))
  expect_true(dir.exists(here::here("working-data", "paper", "raw", "andrews-lter")))
  expect_true(file.exists(here::here("working-data", "paper", "raw", "andrews-lter", "andrews.csv")))
  
}
)