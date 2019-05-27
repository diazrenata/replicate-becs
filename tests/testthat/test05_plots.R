context("Check that plots work")

communities <- load_paper_data()

test_that("plotting a bsed works", {

  bsed <- communities[[1]] %>%
    add_energy_sizeclass() %>%
    make_bsed()
  
  expect_silent(bsed_plot <- plot_bsed(bsed))

})