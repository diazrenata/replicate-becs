context("Check that GMM functions work")

communities <- load_paper_data()

test_that("making an ISD works", {

  isd <- communities[[1]] %>%
    add_energy_sizeclass() %>%
    make_isd()
  
  expect_true(is.data.frame(isd))
  expect_false(anyNA(isd))
  expect_true(colnames(isd)[7] == "ln_energy")
    
})


test_that("plotting an ISD works", {
  
  isd <- communities[[1]] %>%
    add_energy_sizeclass() %>%
    make_isd()
  
  expect_silent(isd_plot <- plot_isd(isd))
  
  communities_energy <- lapply(communities, add_energy_sizeclass)
  
  isds <- lapply(communities_energy, make_isd)
  
  expect_silent(isd_paper_plot <- plot_paper_dists(dists = isds, dist_type = "isd"))
  
})

test_that("fitting a GMM and retrieving results work", {
  
  isd <- communities[[1]] %>%
    add_energy_sizeclass() %>%
    make_isd()
  
  isd_gmm <- fit_gmm(isd)
  
  expect_true(is.list(isd_gmm))
  
  expect_type(get_ngaussians(isd_gmm), type = "integer")
  expect_type(get_gaussianmeans(isd_gmm), type = "double")
  expect_length(get_gaussianmeans(isd_gmm), get_ngaussians(isd_gmm))
  
  isd_pdf <- get_pdf(isd_gmm)
  
  expect_type(get_modes(isd_pdf), "double")
  expect_type(get_bic(isd_gmm), "double")
  })
