context("Check that functions for bootstrap BSED analysis work")

test_that("doi works", {
  same_one = data.frame(individual_species_ids = "notimpt",
                        individual_sizes = abs(rnorm(100, mean = 100, sd = 50))) %>%
    add_energy_sizeclass() %>%
    make_bsed()
  
  same_two = data.frame(individual_species_ids = "notimpt",
                        individual_sizes = abs(rnorm(100, mean = 100, sd = 50))) %>%
    add_energy_sizeclass() %>%
    make_bsed()
  
  same_doi = signif(doi(same_one, same_two), digits= 2)
  

  diff_one = data.frame(individual_species_ids = "notimpt",
                        individual_sizes = abs(rnorm(100, mean = 500, sd = 50))) %>%
    add_energy_sizeclass() %>%
    make_bsed()
  
  diff_two = data.frame(individual_species_ids = "notimpt",
                        individual_sizes = abs(rnorm(100, mean = 100, sd = 50))) %>%
    add_energy_sizeclass() %>%
    make_bsed()
  
  diff_doi = signif(doi(diff_one, diff_two), digits= 2)
  
  expect_true(diff_doi == 2)
  
  expect_true(same_doi < diff_doi)
  
  })
  