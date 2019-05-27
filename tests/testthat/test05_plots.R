context("Check that plots work")

communities <- load_paper_data()

test_that("plotting a bsed works", {

  bsed <- communities[[1]] %>%
    add_energy_sizeclass() %>%
    make_bsed()
  
  expect_silent(bsed_plot <- plot_bsed(bsed))

})


test_that("plotting a bsd works", {
  
  bsd <- communities[[1]] %>%
    add_energy_sizeclass() %>%
    make_bsd()
  
  expect_silent(bsd_plot <- plot_bsd(bsd))
  
})

test_that("replicating figure 1 works", {
  communities_energy = lapply(communities, add_energy_sizeclass)
  bseds = lapply(communities_energy, make_bsed)
  
  expect_silent(bseds_plot <- plot_paper_dists(bseds, dist_type = "bsed"))
  
  bsds = lapply(communities_energy, make_bsd)
  expect_silent(bseds_plot <- plot_paper_dists(bsds, dist_type = "bsd"))
  
  
  })
  
test_that("plotting energetic dominance works", {
  communities_energy = lapply(communities, add_energy_sizeclass)
  bseds = lapply(communities_energy, make_bsed)
  e_doms = lapply(communities_energy, energetic_dominance)
  
  expect_silent(plot_e_dom(e_doms))
  
})