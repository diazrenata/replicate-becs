

library(replicatebecs)
load(paste0(here::here(), '/bsds.Rdata'))
distribution = bsds$niwot
delta_correction = T
focal_column = "ln_mass"
expected_range = NULL
n_or_i = 'n'