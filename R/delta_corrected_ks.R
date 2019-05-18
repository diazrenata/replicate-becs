#' @title d-Corrected Kolgomorov-Smirnov test
#'
#' @description $delta$-corrected KS test
#'
#' @param dist_a distribution
#'
#' @return list of signif to p = 0.05, d value, dcrit
#'
#' @export

zar_ks_test <- function(bsd)
{
  x = bsd %>%
    dplyr::select(species_mean_mass)
  
  rel_f = x %>%
    dplyr::arrange(species_mean_mass) %>%
    dplyr::group_by(species_mean_mass) %>%
    dplyr::mutate(val_freq = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::add_rownames(var = 'i') %>%
    dplyr::mutate(cum_freq = NA)
  
  # I don't know how to do this using dplyr -- RMD
  for(i in 1:nrow(rel_f)) {
    rel_f$cum_freq[i] <- sum(rel_f$val_freq[1:i])
  }
  
  rel_f = rel_f %>%
    dplyr::mutate(rel_freq = cum_freq / nrow(bsd),
                  exp_cum_freq = (species_mean_mass),
                  exp_rel_freq = exp_cum_freq / (max(species_mean_mass)))
  
  d = abs(rel_f$rel_freq - rel_f$exp_rel_freq)
  
  d2 = abs(dplyr::lag(rel_f$rel_freq, n= 1, default = 0) - rel_f$exp_rel_freq)
  
  d = c(d,d2)
  d = max(d)
  
  dcrits = read.csv('data/kstable.csv')
  
  dcrit = dcrits$dcrit_05[which(dcrits$n == nrow(bsd))]
  
  signif = (d >= dcrit)
  
  results = list(signif = signif, d = d, dcrit = dcrit)
  
  return(results)
}
