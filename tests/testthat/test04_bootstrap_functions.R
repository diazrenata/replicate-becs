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
  
  calculated_uniform_bsed = calculate_uniform_size_abund_bsed(communities[[1]])
  
  expect_true(ncol(calculated_uniform_bsed) == 4)
  expect_true(min(calculated_uniform_bsed$size_class) > 0.2)
  expect_false(anyNA(calculated_uniform_bsed))
  
  sampled_uniform_bsed = sample_uniform_size_abund_bsed(communities[[1]])
  expect_true(ncol(sampled_uniform_bsed) == 4)
  expect_false(anyNA(sampled_uniform_bsed))
  
  uniform_bsed_doi = doi(calculated_uniform_bsed, sampled_uniform_bsed)
  
  expect_true(mode(uniform_bsed_doi) == "numeric")
  expect_true(uniform_bsed_doi <= 2)
  expect_true(uniform_bsed_doi >= 0)
  
})

test_that("bootstrap wrapper works for uniform", {
  communities = load_paper_data()
  nbootstraps = 10
  bootstraps = draw_bootstrap_samples(raw_community = communities[[1]], nbootstraps = nbootstraps)
  
  expect_true(is.list(bootstraps))
  expect_true(length(bootstraps) == 3)
  expect_true(length(bootstraps$sampled_bseds) == nbootstraps)
  
  bootstrap_dois = calculate_bootstrap_uniform_dois(bootstraps)
  expect_true(is.list(bootstrap_dois))
  expect_true(bootstrap_dois$empirical_doi >= 0)
  expect_true(bootstrap_dois$empirical_doi <= 2)
  expect_false(anyNA(bootstrap_dois$sampled_dois))
  expect_true(all(bootstrap_dois$sampled_dois >= 0))
  expect_true(all(bootstrap_dois$sampled_dois <= 2))
  
  bootstrap_pval = calculate_bootstrap_p(bootstrap_dois)
  expect_false(is.na(bootstrap_pval))
  expect_true(is.numeric(bootstrap_pval))
  expect_true(bootstrap_pval >= 0)
  expect_true(bootstrap_pval <= 1)

  })

test_that("bootstrap wrapper works for cross communities", {
  communities = load_paper_data()
  community_pairs = setup_community_combinations(communities)
  nbootstraps = 25
  
  cross_bootstraps = draw_bootstrap_samples(raw_community = community_pairs[[1]],
                                            assumption = "cross_communities",
                                            nbootstraps = nbootstraps)
  
  expect_false(anyNA(cross_bootstraps))
  expect_true(length(cross_bootstraps$sampled_bseds) == nbootstraps)
  
  cross_bootstrap_dois = calculate_crosscomm_dois(cross_bootstraps)
  expect_false(anyNA(cross_bootstrap_dois))
  expect_true(is.numeric(cross_bootstrap_dois$empirical_doi))
  expect_true(cross_bootstrap_dois$empirical_doi >= 0)
  expect_true(cross_bootstrap_dois$empirical_doi <= 2)
  expect_true(all(cross_bootstrap_dois$sampled_dois >= 0))
  expect_true(all(cross_bootstrap_dois$sampled_dois <= 2))
  
  })
