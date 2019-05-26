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

test_that("uniform size-abundance works", {
  
  communities = load_paper_data()
  
  test_uniform_bsed = uniform_size_abund_bsed(communities[[1]])
  
  expect_true(ncol(test_uniform_bsed) == 4)
  expect_true(min(test_uniform_bsed$size_class) > 0.2)
  expect_false(anyNA(test_uniform_bsed))
  
})