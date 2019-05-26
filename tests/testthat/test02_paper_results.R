context("Check that paper results files work")

test_that("loading paper results works", {
  appendix_b = load_tidy_appendix_b()
  expect_true(colnames(appendix_b) == c("site_a", "site_b", "max_d", "ernest_p_val"))
})