
test_that("file setup works", {
  setup_files(storagepath)
  expect_true(dir.exists(file.path(storagepath, 'data', 'paper', 'raw')))
  expect_true(dir.exists(file.path(storagepath, 'ernest-2005-files')))
  expect_true(file.exists(file.path(storagepath, 'kstable.csv')))
  
})

context("Check that community datasets are correct")

test_that("process data function works", {
  
  if(!dir.exists(file.path(storagepath, 'data', 'paper', 'raw', 'andrews'))) {
    download_raw_paper_data(storagepath)
  }
  
  process_raw_data(storagepath)

  expect_true(dir.exists(file.path(storagepath, 'data', 'paper', 'processed')))
  expect_true(file.exists(file.path(storagepath, 'data', 'paper', 'processed', 'andrews-processed.csv')))
  expect_true(file.exists(file.path(storagepath, 'data', 'paper', 'processed', 'niwot-processed.csv')))
  expect_true(file.exists(file.path(storagepath, 'data', 'paper', 'processed', 'portal-processed.csv')))
  expect_true(file.exists(file.path(storagepath, 'data', 'paper', 'processed', 'sev-5pgrass-processed.csv')))
  expect_true(file.exists(file.path(storagepath, 'data', 'paper', 'processed', 'sev-5plarrea-processed.csv')))
  expect_true(file.exists(file.path(storagepath, 'data', 'paper', 'processed', 'sev-goatdraw-processed.csv')))
  expect_true(file.exists(file.path(storagepath, 'data', 'paper', 'processed', 'sev-rsgrass-processed.csv')))
  expect_true(file.exists(file.path(storagepath, 'data', 'paper', 'processed', 'sev-rslarrea-processed.csv')))
  expect_true(file.exists(file.path(storagepath, 'data', 'paper', 'processed', 'sev-two22-processed.csv')))
  
  
})

test_that("Communities have the expected dimensions", {
  
  communities <- load_paper_data(storagepath)
  
  expect_true(length(communities) == 9)
  expect_false(anyNA(communities))
  
  summary_stats_comparison = compare_summary_stats(storagepath)
  
  expect_false(anyNA(summary_stats_comparison))
  expect_true(min(summary_stats_comparison$new_richness) == 9)
  expect_true(min(summary_stats_comparison$new_min_mass) == 4)
  expect_true(max(summary_stats_comparison$new_max_mass) == 194.5)
  
  
})