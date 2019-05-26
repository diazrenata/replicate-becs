context("Check that KS tests give expected results")

# test data from Zar 1999 example 22.11 and 22.12
moths = data.frame(height = c(1.4, 2.6, 3.3, 4.2, 4.7, 5.6, 5.6, 6.4, 7.7, 9.3, 10.6, 11.5, 12.4, 18.6, 22.3))

set.seed(352)
uniform_moths = data.frame(height = runif(n = 15, min = 1, max = 40))

test_that("base KS function works", {
  
  moths_ks = zar_ks_test(moths, delta_correction = F, 
                         focal_column = "height", 
                         expected_range = c(0, 25))
  
  expect_true(moths_ks$signif)
  expect_true((round(moths_ks$d, digits = 3) == .371))
  expect_true((moths_ks$p_min == 0.02))
  expect_true((moths_ks$p_max == 0.05))
  
  uniform_moths_ks = zar_ks_test(uniform_moths, delta_correction = F, 
                                 focal_column = "height", 
                                 expected_range = c(1, 40))
  
  expect_false(uniform_moths_ks$signif)
  expect_true((round(uniform_moths_ks$d, digits = 3) == .242))
  expect_true(uniform_moths_ks$p_min == 0.05)
  expect_true(uniform_moths_ks$p_max == 1)
  
})

test_that("delta corrected KS function works", {
  
  moths_ks = zar_ks_test(moths, delta_correction = T, focal_column = "height", 
                         expected_range = c(0, 25), n_or_i = 'i')
  
  expect_true(moths_ks$signif)
  expect_true((round(moths_ks$d, digits = 3) == .361))
  expect_true((moths_ks$p_min == 0.02))
  expect_true((moths_ks$p_max == 0.05))
  
  uniform_moths_ks = zar_ks_test(uniform_moths, delta_correction = T, 
                                 focal_column = "height", 
                                 expected_range = c(1, 40), n_or_i = "i")
  
  expect_false(uniform_moths_ks$signif)
  expect_true((round(uniform_moths_ks$d, digits = 3) == .199))
  expect_true(uniform_moths_ks$p_min == 0.5)
  expect_true(uniform_moths_ks$p_max == 1)
  
})