#' @title d-Corrected Kolgomorov-Smirnov test
#'
#' @description  delta-corrected KS test
#'
#' @param bsd distribution
#' @param delta_correction T/F use delta correction for small sample sizes
#'
#' @return list of signif to p = 0.05, d value, dcrit
#'
#' @export

zar_ks_test <- function(bsd, delta_correction = F)
{
  x = bsd %>%
    dplyr::select(species_mean_mass)
  
  rel_f = x %>%
    dplyr::arrange(species_mean_mass) %>%
    dplyr::group_by(species_mean_mass) %>%
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
    dplyr::mutate(rel_freq = cum_freq / nrow(bsd),
                  exp_cum_freq = (species_mean_mass - min(species_mean_mass)),
                  exp_rel_freq = exp_cum_freq / (max(species_mean_mass) - min(species_mean_mass)))
  
  if(delta_correction) {
    
    rel_f = rel_f %>%
      dplyr::mutate(rel_delta_freq = cum_freq / (nrow(bsd) + 1),
                    rel_delta_freq1 = (cum_freq - 1) / (nrow(bsd) - 1))
    
    d_0 = abs(rel_f$rel_delta_freq - rel_f$exp_rel_freq) %>%
      max()
    d_1 = abs(rel_f$rel_delta_freq1 - rel_f$exp_rel_freq) %>%
      max()
    
    dcrits = read.csv(paste0(here::here(), '/data/delta_kstable.csv'), stringsAsFactors = F)
  
    d_comparison = data.frame(dsubs = c(d_0, d_1), n = nrow(bsd), delta = c(0, 1)) %>%
      dplyr::left_join(dcrits, by = c('n', 'delta')) %>%
      dplyr::filter(dcrit < dsubs) %>%
      dplyr::filter(alpha == min(alpha))
    
    if(nrow(d_comparison) > 0) {
      signif = T
      d = d_comparison$dsubs
      dcrit = d_comparison$dcrit
    } else {
      signif = F
      d = NULL
      dcrit = NULL
    }
    
  } else {
  d = abs(rel_f$rel_freq - rel_f$exp_rel_freq)
  
  d2 = abs(dplyr::lag(rel_f$rel_freq, n= 1, default = 0) - rel_f$exp_rel_freq)
  
  d = c(d,d2)
  d = max(d)
  
  dcrits = read.csv(paste0(here::here(), '/data/kstable.csv'))
  
  dcrit = dcrits$dcrit_05[which(dcrits$n == nrow(bsd))]
  signif = (d >= dcrit)
  
  }
  
  results = list(signif = signif, d = d, dcrit = dcrit)
  
  
  return(results)
}



