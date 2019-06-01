R Notebook
================

``` r
library(dplyr)
library(mclust)
library(replicatebecs)
```

Playing around with Gaussian mixture models in `mclust`.

As I understand it, this should run a GMM with `1:15` modes on a vector of data, allowing for unequal variances (`modelNames = "V"`; use `modelNames = "E"` for equal variances) and calculate BIC for each one, allowing for model selection:

``` r
mclust::Mclust(data, G = c(1:15), modelNames = "V", 
     prior = NULL, 
     control = emControl(), 
     initialization = NULL, 
     warn = mclust.options("warn"), 
     x =  NULL, 
     verbose = interactive())
```

To test this out I'm going to generate some toy data with a known number of modes by sticking together a community.

``` r
set.seed(326)
nspecies <- 5

species_data <- data.frame(species_id = c(1:nspecies), 
                           species_mean_mass = runif(n = nspecies,
                                                     min = 10, max = 150), 
                           species_abundance = ceiling(runif(n = nspecies, 
                                                     min = 20, max = 200))) %>%
  dplyr::mutate(species_variance = .01 * species_mean_mass)

map_abund <- function(X, abund_column) {
  return(rep(X, times = abund_column[X]))
}

toy_isd <- sapply(X = 1:nspecies, FUN = map_abund, abund_column = species_data$species_abundance) %>%
  unlist() %>%
  as.data.frame() %>%
  dplyr::rename(species_id = '.') %>%
  dplyr::left_join(species_data, by = 'species_id') %>%
  dplyr::mutate(individual_mass = rnorm(n = sum(species_data$species_abundance), mean = species_mean_mass,
                                        sd = species_variance))%>%
  dplyr::select(species_id, individual_mass) %>%
  dplyr::rename(individual_sizes = individual_mass) %>%
  replicatebecs::add_energy_sizeclass() %>%
  dplyr::mutate(ln_size = log(individual_sizes), ln_energy = log(individual_energy))

toy_bsed <- toy_isd %>%
  replicatebecs::make_bsed()

plot_bsed(toy_bsed)
```

![](/Users/renatadiaz/Documents/GitHub/replicate-becs/report/toy_gmms_files/figure-markdown_github/make%20toy%20community-1.png)

``` r
isd_plot <- ggplot2::ggplot(data = toy_isd, ggplot2::aes(toy_isd$ln_size, xmin = 0, xmax = 1)) +
    ggplot2::geom_histogram(data = toy_isd, stat = "bin", 
                            binwidth = 0.01,
                            show.legend = NA,
                            inherit.aes = TRUE)  +
    ggplot2::labs(x = "ln(size)", y = "Number of individuals", title = "Individuals size distribution") +
    ggplot2::theme_bw()

ied_plot <- ggplot2::ggplot(data = toy_isd, ggplot2::aes(toy_isd$ln_energy, xmin = 0, xmax = 1)) +
    ggplot2::geom_histogram(data = toy_isd, stat = "bin", 
                            binwidth = 0.01,
                            show.legend = NA,
                            inherit.aes = TRUE) +
    ggplot2::labs(x = "ln(energy)", y = "Number of individuals", title = "Individuals energy distribution") +
    ggplot2::theme_bw()

isd_plot
```

![](/Users/renatadiaz/Documents/GitHub/replicate-becs/report/toy_gmms_files/figure-markdown_github/make%20toy%20community-2.png)

``` r
ied_plot
```

![](/Users/renatadiaz/Documents/GitHub/replicate-becs/report/toy_gmms_files/figure-markdown_github/make%20toy%20community-3.png)

MCLUST on ISD...

``` r
gmm_isd <- mclust::Mclust(toy_isd$ln_size, G = 1:15, modelNames = "V", 
     prior = NULL, 
     control = emControl(), 
     initialization = NULL, 
     warn = mclust.options("warn"), 
     x =  NULL, 
     verbose = interactive())
```

    ## fitting ...
    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |====                                                             |   6%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |============                                                     |  19%
      |                                                                       
      |================                                                 |  25%
      |                                                                       
      |====================                                             |  31%
      |                                                                       
      |========================                                         |  38%
      |                                                                       
      |============================                                     |  44%
      |                                                                       
      |================================                                 |  50%
      |                                                                       
      |=====================================                            |  56%
      |                                                                       
      |=========================================                        |  62%
      |                                                                       
      |=============================================                    |  69%
      |                                                                       
      |=================================================                |  75%
      |                                                                       
      |=====================================================            |  81%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |=============================================================    |  94%
      |                                                                       
      |=================================================================| 100%

``` r
gmm_isd
```

    ## 'Mclust' model object: (V,4) 
    ## 
    ## Available components: 
    ##  [1] "call"           "data"           "modelName"      "n"             
    ##  [5] "d"              "G"              "BIC"            "bic"           
    ##  [9] "loglik"         "df"             "hypvol"         "parameters"    
    ## [13] "z"              "classification" "uncertainty"

``` r
gmm_isd$BIC
```

    ## Bayesian Information Criterion (BIC): 
    ##             V
    ## 1    47.30982
    ## 2   521.52378
    ## 3   779.95205
    ## 4  1716.41966
    ## 5  1697.79549
    ## 6  1705.38522
    ## 7  1696.29591
    ## 8  1675.96088
    ## 9  1651.47253
    ## 10 1632.77850
    ## 11 1614.99344
    ## 12 1597.29650
    ## 13 1594.66320
    ## 14 1576.56298
    ## 15 1560.80508
    ## 
    ## Top 3 models based on the BIC criterion: 
    ##      V,4      V,6      V,5 
    ## 1716.420 1705.385 1697.795

``` r
gmm_isd$parameters$mean
```

    ##        1        2        3        4 
    ## 4.313749 4.413669 4.682659 4.961594

``` r
print(plot(gmm_isd, what = c("density")))
```

![](/Users/renatadiaz/Documents/GitHub/replicate-becs/report/toy_gmms_files/figure-markdown_github/mclust%20on%20isd-1.png)

    ## NULL

Trying it on real data...

``` r
andrews <- replicatebecs::load_paper_data()[[1]] %>%
  replicatebecs::add_energy_sizeclass() %>%
  dplyr::mutate(ln_size = log(individual_sizes), ln_energy = log(individual_energy))


andrews_isd_plot <- ggplot2::ggplot(data = andrews, ggplot2::aes(andrews$ln_size, xmin = 0, xmax = 1)) +
    ggplot2::geom_histogram(data = andrews, stat = "bin", 
                            binwidth = 0.01,
                            show.legend = NA,
                            inherit.aes = TRUE)  +
    ggplot2::labs(x = "ln(size)", y = "Number of individuals", title = "Individuals size distribution") +
    ggplot2::theme_bw()

andrews_isd_plot
```

![](/Users/renatadiaz/Documents/GitHub/replicate-becs/report/toy_gmms_files/figure-markdown_github/mclust%20on%20real%20data-1.png)

``` r
gmm_andrews <- mclust::Mclust(andrews$ln_size, G = 1:15, modelNames = "V", 
     prior = NULL, 
     control = emControl(), 
     initialization = NULL, 
     warn = mclust.options("warn"), 
     x =  NULL, 
     verbose = interactive())
```

    ## fitting ...
    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |====                                                             |   6%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |============                                                     |  19%
      |                                                                       
      |================                                                 |  25%
      |                                                                       
      |====================                                             |  31%
      |                                                                       
      |========================                                         |  38%
      |                                                                       
      |============================                                     |  44%
      |                                                                       
      |================================                                 |  50%
      |                                                                       
      |=====================================                            |  56%
      |                                                                       
      |=========================================                        |  62%
      |                                                                       
      |=============================================                    |  69%
      |                                                                       
      |=================================================                |  75%
      |                                                                       
      |=====================================================            |  81%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |=============================================================    |  94%
      |                                                                       
      |=================================================================| 100%

``` r
gmm_andrews
```

    ## 'Mclust' model object: (V,3) 
    ## 
    ## Available components: 
    ##  [1] "call"           "data"           "modelName"      "n"             
    ##  [5] "d"              "G"              "BIC"            "bic"           
    ##  [9] "loglik"         "df"             "hypvol"         "parameters"    
    ## [13] "z"              "classification" "uncertainty"

``` r
gmm_andrews$BIC
```

    ## Bayesian Information Criterion (BIC): 
    ##            V
    ## 1  -2128.477
    ## 2  -1951.563
    ## 3  -1607.486
    ## 4  -1627.428
    ## 5  -1636.931
    ## 6  -1650.245
    ## 7  -1668.706
    ## 8         NA
    ## 9  -1665.059
    ## 10        NA
    ## 11        NA
    ## 12        NA
    ## 13        NA
    ## 14        NA
    ## 15        NA
    ## 
    ## Top 3 models based on the BIC criterion: 
    ##       V,3       V,4       V,5 
    ## -1607.486 -1627.428 -1636.931

``` r
gmm_andrews$parameters$mean
```

    ##        1        2        3 
    ## 1.466011 2.710192 4.535536

``` r
print(plot(gmm_andrews, what = c("density")))
```

![](/Users/renatadiaz/Documents/GitHub/replicate-becs/report/toy_gmms_files/figure-markdown_github/mclust%20on%20real%20data-2.png)

    ## NULL
