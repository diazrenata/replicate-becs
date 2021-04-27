context("Check that plots work")

communities <- load_paper_data()

test_that("plotting a bsed works", {

  bsed <- communities[[1]] %>%
    add_energy_sizeclass() %>%
    make_bsed() 
  
  expect_warning(plot_bsed(bsed), regexp = "limits")

})


test_that("plotting a bsd works", {
  
  bsd <- communities[[1]] %>%
    add_energy_sizeclass() %>%
    make_bsd()
  
  expect_warning(bsd_plot <- plot_bsd(bsd), regexp = "limits")
  
})

test_that("replicating figure 1 works", {
  communities_energy = lapply(communities, add_energy_sizeclass)
  bseds = lapply(communities_energy, make_bsed)
  
  expect_warning(bseds_plot <- plot_paper_dists(bseds, dist_type = "bsed"), regexp = "limits")
  
  bsds = lapply(communities_energy, make_bsd)
  expect_warning(bseds_plot <- plot_paper_dists(bsds, dist_type = "bsd"),  regexp = "limits")
  
  
  })
  
test_that("plotting energetic dominance works", {
  communities_energy = lapply(communities, add_energy_sizeclass)
  bseds = lapply(communities_energy, make_bsed)
  e_doms = lapply(communities_energy, energetic_dominance)
  
  expect_silent(plot_e_dom(e_doms))
  
})

test_that("plotting bootstrapped uniform BSEDs works", {
  nbootstraps = 10
  bootstraps = draw_bootstrap_samples(raw_community = communities[[1]], nbootstraps = nbootstraps)
  bootstrap_dois = calculate_bootstrap_uniform_dois(bootstraps)
  expect_warning(plot_bsed_bootstrap_results(bootstrap_dois), regexp = "intercept")
})

test_that("plotting bootstrapped cross-community BSEDs works", {
  community_pairs = setup_community_combinations(communities)
  nbootstraps = 10
  
  cross_bootstraps = draw_bootstrap_samples(raw_community = community_pairs[[1]],
                                            assumption = "cross_communities",
                                            nbootstraps = nbootstraps)
  
  cross_bootstrap_dois = calculate_crosscomm_dois(cross_bootstraps)
  expect_warning(plot_bsed_bootstrap_results(cross_bootstrap_dois), regexp = "intercept")
})

test_that("plotting multiple bootstrapped cross-community BSEDs works", {
  community_pairs = setup_community_combinations(communities)[1:5]
  nbootstraps = 10
  
  multi_cross_bootstraps = lapply(community_pairs, FUN = draw_bootstrap_samples,
                                            assumption = "cross_communities",
                                            nbootstraps = nbootstraps)
  
  multi_cross_bootstrap_dois = lapply(multi_cross_bootstraps, calculate_crosscomm_dois)
  expect_warning(plot_crosscomm_bseds(multi_cross_bootstrap_dois), regexp = "intercept")
  
  expect_warning(plot_bootstrap_pvals(multi_cross_bootstrap_dois), regexp = "intercept")
})

