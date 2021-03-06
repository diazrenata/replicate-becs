% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delta-corrected-ks-test.R
\name{zar_ks_test}
\alias{zar_ks_test}
\title{d-Corrected Kolgomorov-Smirnov test}
\usage{
zar_ks_test(distribution, delta_correction = F,
  focal_column = "species_mean_mass", expected_range = NULL,
  n_or_i = "n")
}
\arguments{
\item{distribution}{distribution to compare to uniform}

\item{delta_correction}{T/F use delta correction for small sample sizes}

\item{focal_column}{name of column for the distribution to be evaluated}

\item{expected_range}{vector of expected min and max values for uniform; defaults to min and max of measurement column}

\item{n_or_i}{"n" or "i" see issue on GitHub}
}
\value{
list of signif to p = 0.05, d value, dcrit
}
\description{
delta-corrected KS test from Zar 1999, comparing a focal distribution to a uniform distribution.
}
