context("Check that paper results files work")

test_that("loading paper results works", {
  appendix_b = load_tidy_appendix_b()
  expect_true(ncol(appendix_b) == 4)
  expect_true(nrow(appendix_b) == 36)
})