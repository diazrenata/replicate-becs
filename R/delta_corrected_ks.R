#' @title d-Corrected Kolgomorov-Smirnov test
#'
#' @description  delta-corrected KS test
#'
#' @param bsd distribution
#' @param delta_correction T/F use delta correction for small sample sizes
#' @param focal_column name of column for the distribution to be evaluated
#' @param expected_range vector of expected min and max values for uniform; defaults to min and max of measurement column
#' @param n_or_i "n" or "i"
#'
#' @return list of signif to p = 0.05, d value, dcrit
#'
#' @export

zar_ks_test <- function(distribution, delta_correction = F, focal_column = "species_mean_mass", 
                        expected_range = NULL, n_or_i = 'n')
{
  
  x = distribution %>%
    dplyr::rename(measurement = focal_column)
  
  nvals = nrow(x)
  
  if(is.null(expected_range)) {
    expected_min = min(x$measurement)
    expected_max = max(x$measurement)
  } else {
    expected_min = min(expected_range)
    expected_max = max(expected_range)
  }
  
  rel_f = x %>%
    dplyr::arrange(measurement) %>%
    dplyr::group_by(measurement) %>%
    dplyr::mutate(val_freq = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    tibble::rownames_to_column(var = 'i') %>%
    dplyr::mutate(cum_freq = NA)
  
  # I don't know how to do this using dplyr -- RMD
  for(i in 1:nrow(rel_f)) {
    rel_f$cum_freq[i] <- sum(rel_f$val_freq[1:i])
  }
  
  rel_f = rel_f %>%
    dplyr::mutate(rel_freq = cum_freq / nvals,
                  exp_cum_freq = (measurement - expected_min),
                  exp_rel_freq = exp_cum_freq / (expected_max - expected_min))
  
  if(delta_correction) {
    
    d_crits = read.csv(paste0(here::here(), '/data/delta_kstable.csv'), stringsAsFactors = F)
    
    rel_f = rel_f %>%
      dplyr::mutate(rel_delta_freq = cum_freq / (nvals + 1),
                    rel_delta_freq1 = (cum_freq - 1) / (nvals - 1),
                    d_0 = abs(rel_delta_freq - exp_rel_freq),
                    d_1 = abs(rel_delta_freq1 - exp_rel_freq)) %>%
      dplyr::select(d_0, d_1, i) %>%
      tidyr::gather(key = 'delta', value = 'd', -i) %>%
      dplyr::mutate(delta = ifelse(delta == 'd_0', 0, 1))
    
    if(n_or_i == 'i') { 
      nvals = NULL
    }
    
    p_vals_d0 = find_ps(rel_f = rel_f, d_crits = d_crits, delta = 0, nvals = nvals)
    
    p_vals_d1 = find_ps(rel_f = rel_f, d_crits = d_crits, delta = 1, nvals = nvals)
    
    if (p_vals_d0$p_min < p_vals_d1$p_min) {
      p_vals = p_vals_d0
    } else {
      p_vals = p_vals_d1
    }
    
  } else {
    
    d_crits = read.csv(paste0(here::here(), '/data/kstable.csv'))
    
    rel_f = rel_f %>%
      dplyr::mutate(d = abs(rel_freq - exp_rel_freq),
                    d2 = abs(dplyr::lag(rel_freq, n= 1, default = 0) - exp_rel_freq))
  
    p_vals = find_ps(rel_f = rel_f, d_crits = d_crits, nvals = nvals)
    
  }
  p_min = p_vals$p_min
  p_max = p_vals$p_max
  d = p_vals$d
  
  signif = (p_max <= 0.05)
  
  results = list(signif = signif, p_max = p_max, p_min = p_min, d = d)
  
  return(results)
  
}

#' find ps
#'
#' @description helper for ks test
#' @param rel_f rel_f table
#' @param d_crits d_crit table 
#' @param deltaval 0, 1, or null for non-delta-corrected
#' @param nvals sample size, or NULL in which case i is used
#'
#' @return list of pvals
#' @export

find_ps = function(rel_f, d_crits, deltaval = NULL, nvals = NULL) {
  
  if(is.null(deltaval)) {
    d = max(c(rel_f$d, rel_f$d2))
    
    d_comparison = d_crits %>%
      dplyr::filter(n == nvals)
    
  } else {
    rel_f = rel_f %>%
      dplyr::filter(delta == deltaval)
    
    this_rel = rel_f %>%
      dplyr::filter(d == max(d))
    
    d = this_rel$d
    i = this_rel$i
    
    if(is.null(nvals)) {
    d_comparison = d_crits %>%
      dplyr::filter(n == i, delta == deltaval)
    } else {
      d_comparison = d_crits %>%
        dplyr::filter(n == nvals, delta == deltaval)
    }
  }
  
  p_min = d_comparison %>%
    dplyr::filter(dcrit > d) 
  
  if(nrow(p_min) >= 1) {
    p_min = p_min %>%
      dplyr::filter(alpha == max(alpha)) %>%
      dplyr::select(alpha) %>%
      as.numeric()
  } else {
    p_min = 0
  }
  
  p_max = d_comparison %>%
    dplyr::filter(dcrit < d)
  
  if(nrow(p_max) >= 1) {
    p_max = p_max %>%
      dplyr::filter(alpha == min(alpha)) %>%
      dplyr::select(alpha) %>%
      as.numeric()
  } else {
    p_max = 1
  }
  
  return(list(p_min = p_min, p_max = p_max, d = d))
}

