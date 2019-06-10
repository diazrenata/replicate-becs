#' @title Make ISD
#' @param community_energy community df with energy & sizeclass columns added
#' @return ISD table
#' @export
make_isd <- function(community_energy){
  this_isd <- community_energy %>%
    dplyr::mutate(ln_size = log(individual_sizes), ln_energy = log(individual_energy))
  
  return(this_isd)
}

#' @title Plot isd
#' @param isd result of `make_isd`
#' @param isd_name community name, defaults NULL
#' @return plot of ISD 
#' @export
plot_isd <- function(isd, isd_name = NULL){
  
  if(!is.null(isd_name)) {
    plot_name = paste0("Individuals size distribution: ", isd_name)
  } else {
    plot_name = "Individuals size distribution"
  }
  
  this_isd_plot <- ggplot2::ggplot(data = isd, ggplot2::aes(isd$ln_size, xmin = 0, xmax = 1)) +
    ggplot2::geom_histogram(data = isd, stat = "bin", 
                            binwidth = 0.01,
                            show.legend = NA,
                            inherit.aes = TRUE)  +
    ggplot2::labs(x = "ln(size)", y = "Number of individuals", title = plot_name) +
    ggplot2::theme_bw()
  
  return(this_isd_plot)
}


#' Fit GMM to individual size distribution
#' Using `mclust::Mclust`
#' @param isd result of `make_isd`
#' @return mclust fit
#' @export
#' @importFrom mclust Mclust mclustBIC mclust.options emControl densityMclust
fit_gmm <- function(isd){
  this_fit <- mclust::densityMclust(isd$ln_size, G = 1:15, modelNames = "V", 
                 prior = NULL, 
                 control = emControl(), 
                 initialization = NULL, 
                 warn = mclust.options("warn"), 
                 x =  NULL, 
                 verbose = interactive())
  return(this_fit)
}

#' Get number of gaussians
#' @param gmm result of fit_gmm
#' @return ngaussians
#' @export
get_ngaussians <- function(gmm) {
  return(gmm$G)
}


#' Get modes of PDF
#' @param pdf result of get_pdf
#' @importFrom spatialEco local.min.max
#' @importFrom dplyr filter
#' @importFrom graphics plot
#' @export
get_modes <- function(pdf) {
  mode_ps <- spatialEco::local.min.max(pdf$density, plot = F)$maxima 
  modes <- dplyr::filter(pdf, density %in% mode_ps)
  modes <- modes$sizes
  return(modes)
}

#' Get PDF from fitted gmm
#' @param gmm result of fit_gmm
#' @return pdf vector
#' @importFrom stats predict
#' @export
get_pdf <- function(gmm) {
  sizes <- seq(0, 8, by = 0.1)
  gmm_pdf <- predict(gmm, newdata = sizes, what = "dens", logarithm = F)
  pdf <- data.frame(sizes = sizes, density = gmm_pdf)
  return(pdf)
}

#' Get means of gaussians
#' @param gmm result of fit_gmm
#' @return means of gaussians
#' @export
get_gaussianmeans <- function(gmm){
  return(gmm$parameters$mean)
}

#' Get BIC for modes
#' @param gmm result of fit_gmm
#' @return BIC of modes
#' @export
get_bic <- function(gmm){
  return(gmm$BIC)
}
