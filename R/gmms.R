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
#' @importFrom mclust Mclust
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

#' Get number of modes
#' @param gmm result of fit_gmm
#' @return nmodes
#' @export
get_nmodes <- function(gmm) {
  return(gmm$G)
}

#' Get means of modes
#' @param gmm result of fit_gmm
#' @return means of modes
#' @export
get_modemeans <- function(gmm){
  return(gmm$parameters$mean)
}

#' Get BIC for modes
#' @param gmm result of fit_gmm
#' @return BIC of modes
#' @export
get_bic <- function(gmm){
  return(gmm$BIC)
}