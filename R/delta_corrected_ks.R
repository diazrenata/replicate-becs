#' @title d-Corrected Kolgomorov-Smirnov test
#'
#' @description $delta$-corrected KS test
#'
#' @param dist_a First distribution
#' @param dist_b Second distribution, if NULL, uniform 
#'
#' @return KS statistic
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