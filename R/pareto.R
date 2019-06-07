library(replicatebecs)

rawdat <- load_paper_data()[[1]]

isd <- rawdat %>%
  add_energy_sizeclass() %>%
  make_isd() 

v <- isd$ln_size

library(poweRlaw)

v_pl <- conpl$new(v)
est <- estimate_xmin(v_pl)
v_pl$xmin <- est

plot(v_pl)

bs <- bootstrap_p(v_pl)

ecd <- ecdf(v)

plot(ecd)

# 
# data(moby)
# m = displ$new(moby)
# m$getXmin()
# m$getPars()
# m$setXmin(2)
# m$setPars(2)
# plot(m)
# 
# data(moby_sample)
# m = displ$new(moby_sample)
# estimate_xmin(m)
# m$setXmin(7)
# estimate_pars(m)

est_alpha <- estimate_pars()

powerdist <- poweRlaw::dplcon(alpha = )


#' Fit GMM to individual size distribution
#' Using `mclust::Mclust`
#' @param isd result of `make_isd`
#' @return mclust fit
#' @export
#' @importFrom mclust Mclust mclustBIC mclust.options emControl
fit_gmm <- function(isd){
  this_fit <- mclust::Mclust(isd$ln_size, G = 1:15, modelNames = "V", 
                             prior = NULL, 
                             control = emControl(), 
                             initialization = NULL, 
                             warn = mclust.options("warn"), 
                             x =  NULL, 
                             verbose = interactive())
  return(this_fit)
}

#' @title Density plot of GMM
#' @param gmm result of fit_gmm
#' @param gmm_name not used
#' @return density plot
#' @export
plot_gmm <- function(gmm, gmm_name = NULL){
  this_plot <- plot(gmm, what = "density")
  return(this_plot)
}
