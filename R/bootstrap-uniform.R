#' Generate a BSED from a completely uniform size-abundance distribution
#'
#' Based on a raw community df.
#' For DOI comparisons.
#'
#' @param raw_community table of sizes and ids
#'
#' @return bsed from a uniform size abundance distribution of min and max size of the raw community(one individual of every size)
#' @export
#'
uniform_size_abund_bsed <- function(raw_community) {
  true_uniform_bsed <- data.frame(individual_sizes = seq(min(raw_community$individual_sizes),
                                                         max(raw_community$individual_sizes), 
                                                         by = .1), 
                                  individual_species_ids = "notimpt",
                                  stringsAsFactors = F) %>%
    add_energy_sizeclass() %>%
    make_bsed()
  return(true_uniform_bsed)
}

