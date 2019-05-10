#' @title Compute distribution overlap index for BSEDs
#'
#' @description Calculate DOI for two distributions or one distribution compared to uniform. 
#'
#' @param dist_a First distribution
#' @param dist_b Second distribution, if NULL, uniform 
#'
#' @return DOI for two distributions
#'
#' @export

doi <- function(dist_a, dist_b = NULL)
{

  if(is.null(dist_b)){
    dist_b = rep(1/length(dist_a), length(dist_a))
  }
  
  doi = sum(abs(dist_a - dist_b))
  
  return(doi)
}