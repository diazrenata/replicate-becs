context("Check that paper results files work")

test_that("loading paper results works", {
  appendix_b = load_tidy_appendix_b()
  expect_true(ncol(appendix_b) == 4)
  expect_true(nrow(appendix_b) == 36)
})

test_that("summary stats comparison works", {
  
  summary_stats_comparison = compare_summary_stats()
  
  expect_false(anyNA(summary_stats_comparison))
  expect_true(min(summary_stats_comparison$new_richness) == 9)
  expect_true(min(summary_stats_comparison$new_min_mass) == 4)
  expect_true(max(summary_stats_comparison$new_max_mass) == 194.5)
  
})
