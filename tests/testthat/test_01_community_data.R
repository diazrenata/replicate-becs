storagepath <- file.path(system.file(package= 'replicatebecs'), 'data')

test_that("Communities have the expected dimensions", {

  communities <- load_paper_data(storagepath = storagepath)
  
  expect_true(length(communities) == 9)
  expect_false(anyNA(communities))
  
  summary_stats_comparison = compare_summary_stats(storagepath)
  
  expect_false(anyNA(summary_stats_comparison))
  expect_true(min(summary_stats_comparison$new_richness) == 9)
  expect_true(min(summary_stats_comparison$new_min_mass) == 4)
  expect_true(max(summary_stats_comparison$new_max_mass) == 194.5)
  
  
})