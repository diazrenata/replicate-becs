context("Check that creating BSDs and BSEDs works")

communities = load_paper_data()

test_that("size energy calculations work", {

  test_community_energy = add_energy_sizeclass(communities[[1]])
  
  expect_true(nrow(test_community_energy) == 750) 
  expect_true(ncol(test_community_energy) == 5)
  expect_false(anyNA(test_community_energy))
  
})


test_that("make_bsd works", {
  
  test_bsd = make_bsd(communities[[1]])
  
  expect_true(nrow(test_community_energy) == 9) 
  expect_true(ncol(test_community_energy) == 6)
  expect_false(anyNA(test_bsd))
  
})