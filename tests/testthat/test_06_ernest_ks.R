context("Check that comparing to Ernest KS test results works")

communities = load_paper_data()
community_pairs = setup_community_combinations(communities)

test_that("KS value comparison works", {
  
  ks_results = lapply(community_pairs, FUN = ks_bsd)
  
  compared_results = compare_ernest_ks_values(ks_results)
  
  expect_true(is.data.frame(compared_results))
  expect_false(anyNA(compared_results))
  expect_silent(plot_crosscomm_ks_pvals(compared_results))
  
  
})