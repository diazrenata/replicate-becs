context("Check that paper data is okay")

test_that("DownloadPaperData functions correctly", {
  
  download_raw_paper_data(from_url = FALSE)
  
  expect_true(dir.exists(here::here("working-data", "paper", "raw")))
  expect_true(dir.exists(here::here("working-data", "paper", "raw", "andrews-lter")))
  expect_true(file.exists(here::here("working-data", "paper", "raw", "andrews-lter", "andrews.csv")))
  
})

test_that("ProcessData functions correctly", {
  process_raw_data()
  expect_true(dir.exists(here::here("working-data", "paper", "processed")))
  expect_true(length(list.files(here::here("working-data", "paper", "processed"))) == 9)
})