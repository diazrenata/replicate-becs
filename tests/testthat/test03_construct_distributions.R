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
  
  expect_true(nrow(test_bsd) == 9) 
  expect_true(ncol(test_bsd) == 6)
  expect_false(anyNA(test_bsd))
  
})


test_that("make_bsed works", {
  
  test_community_energy = add_energy_sizeclass(communities[[1]])
  
  test_bsed = make_bsed(test_community_energy)
  
  expect_true(nrow(test_bsed) == 23) 
  expect_true(ncol(test_bsed) == 4)
  expect_false(anyNA(test_bsed))
  expect_true(sum(test_bsed$total_energy_proportional) == 1)
  
})

test_that("energetic dominance works", {
  test_community_energy = add_energy_sizeclass(communities[[1]])
  
  test_edom = energetic_dominance(test_community_energy)
  
  expect_true(ncol(test_edom) == 4)
  expect_true(nrow(test_edom) == 3)
  expect_false(anyNA(test_edom))
  
})