Replication of Ernest (2005)
================
Renata Diaz
5/4/2019

``` r
library(revisitbecs)
library(ggplot2)
```

Import data
===========

``` r
process_raw_data()
```

    ## Loading in data version 1.101.0

    ## [1] TRUE

``` r
data_files <- list.files(path = paste0(here::here(), '/data/paper/processed'), full.names = T)

communities <- list()

for(i in 1:length(data_files)) {
communities[[i]] <- read.csv(data_files[[i]], stringsAsFactors = F)
}

rm(data_files)
rm(i)

ncommunities = length(communities)
```

We need to 1) estimate each individual's energy use (as *m*<sup>3/4</sup> where *m* is mass in g) and 2) assign each individual to a size class. We will divide the communities into size classes of .2 log units.

``` r
community_tables <- list()

for(i in 1:length(communities)){
  community_tables[[i]] <- make_community_table(community = communities[[i]])
}
```

Construct BSEDs for those communities
=====================================

Now we can construct a body size-energy distribution for each community. This is a summary, for each size class, of how much energy all the individuals (regardless of species ID) in that size class use. We will work with these distributions standardized according to the total energy used by the whole community.

``` r
real_bseds <- list()

for(i in 1:ncommunities){
  real_bseds[[i]] <- make_bsed(community_tables[[i]], decimals = 1)
}
```

BSED plots
==========

![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSEDS-1.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSEDS-2.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSEDS-3.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSEDS-4.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSEDS-5.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSEDS-6.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSEDS-7.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSEDS-8.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSEDS-9.png)

Calculate and plot energetic dominance
======================================

``` r
dominance_values <- vector(mode = "numeric")

for(i in 1:ncommunities) {
  these_modes <- energetic_dominance(community_tables[[i]])
  
  these_modes <- these_modes %>%
    dplyr::select(mode_id, e_dominance) %>%
    dplyr::distinct()
  
  dominance_values <- c(dominance_values, these_modes$e_dominance)
}

anyNA(dominance_values)
```

    ## [1] FALSE

``` r
dominance_values <- as.data.frame(dominance_values)

e_dominance_plot <- ggplot(data = dominance_values) + 
  geom_histogram(binwidth = 0.1, aes(x = dominance_values)) +
  xlim(-0.1, 1.1) + 
  theme_bw()

e_dominance_plot
```

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](ernest_2005_replication_files/figure-markdown_github/energetic%20dominance%20for%20real%20communities-1.png)

Compare each real BSED to 10000 bootstraps (DOI 95% interval)
=============================================================

``` r
nsamples = 10000
for(i in 1:ncommunities){ 
  sampled_communities_doi <- replicate(nsamples, boostrap_unif_bsed_doi(communities[[i]]))
  
  real_doi <- doi(real_bseds[[i]]$total_energy_proportional)
  
  p_greater_doi <- length(which(sampled_communities_doi > real_doi)) / nsamples
  
  print(p_greater_doi)
  
}
```

    ## [1] 1
    ## [1] 1
    ## [1] 1
    ## [1] 1
    ## [1] 1
    ## [1] 1
    ## [1] 1
    ## [1] 1
    ## [1] 1

Compare all pairwise communities BSEDs
======================================

``` r
nsamples = 10000

all_pairs_matrix <- combn(1:ncommunities, m = 2)

p_comparison <- vector(length = ncol(all_pairs_matrix), mode = 'numeric')

for(i in 1:ncol(all_pairs_matrix)) {
  
  first = all_pairs_matrix[1, i]
    second = all_pairs_matrix[2, i]
sampled_pair_doi <- replicate(nsamples, boostrap_crosscomm_bseds(communities[[first]], communities[[second]]))


both_bseds <- real_bseds[[first]] %>%
  dplyr::full_join(real_bseds[[second]], by = c("size_class", "size_class_g")) %>%
  dplyr::mutate(total_energy_proportional.x = replace(total_energy_proportional.x, is.na(total_energy_proportional.x), 0),
                total_energy_proportional.y = replace(total_energy_proportional.y, is.na(total_energy_proportional.y), 0))

real_doi <- doi(both_bseds$total_energy_proportional.x,
                both_bseds$total_energy_proportional.y)

p_greater_doi <- length(which(sampled_pair_doi > real_doi)) / nsamples

p_comparison[i] <- p_greater_doi
}

p_comparison
```

    ##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    ## [36] 0

``` r
length(which(p_comparison > 0.05)) / length(p_comparison)
```

    ## [1] 0

Construct BSDs for real communities
===================================

``` r
real_bsds <- list()
for(i in 1:ncommunities) {
  real_bsds[[i]] <- make_bsd(community_tables[[i]], decimals = 2)
}
```

Plot them
=========

``` r
for(i in 1:ncommunities){ 
  this_bsd <- real_bsds[[i]]
  bsd_plot <- ggplot(data = this_bsd, aes(x = size_class, y =  n_species_proportional)) +
    geom_point(data = this_bsd, aes(x = as.factor(size_class_g), y= n_species_proportional)) + 
    theme_bw()
  print(bsd_plot)
  }
```

![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSDs-1.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSDs-2.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSDs-3.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSDs-4.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSDs-5.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSDs-6.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSDs-7.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSDs-8.png)![](ernest_2005_replication_files/figure-markdown_github/plot%20real%20BSDs-9.png)

Compare each real BSD to uniform (d-corrected KS)
=================================================

``` r
for (i in 1:ncommunities) {
  this_bsd <- real_bsds[[i]]
  this_ks <- ks.test(this_bsd$n_species_proportional, punif)
  print(this_ks$p.value)
}
```

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 0.0001251666

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 0.000170163

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 1.833555e-07

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 7.14257e-05

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 6.893074e-05

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 3.221248e-06

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 2.394569e-09

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 4.481607e-08

    ## Warning in ks.test(this_bsd$n_species_proportional, punif): ties should not
    ## be present for the Kolmogorov-Smirnov test

    ## [1] 1.648192e-06

Compare all pairwise communities BSDs (KS)
==========================================

``` r
all_pairs_matrix <- combn(1:ncommunities, m = 2)

ks_p_comparison <- vector(length = ncol(all_pairs_matrix), mode = 'numeric')

for(i in 1:ncol(all_pairs_matrix)) {
  
  first = all_pairs_matrix[1, i]
    second = all_pairs_matrix[2, i]

both_bsds <- real_bsds[[first]] %>%
  dplyr::full_join(real_bsds[[second]], by = c("size_class", "size_class_g")) %>%
  dplyr::mutate(n_species_proportional.x = replace(n_species_proportional.x, is.na(n_species_proportional.x), 0),
                n_species_proportional.y = replace(n_species_proportional.y, is.na(n_species_proportional.y), 0))

ks_comparison <- ks.test(both_bsds$n_species_proportional.x,
                both_bsds$n_species_proportional.y)
ks_p_comparison[i] <- ks_comparison$p.value
}
```

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

    ## Warning in ks.test(both_bsds$n_species_proportional.x,
    ## both_bsds$n_species_proportional.y): cannot compute exact p-value with ties

``` r
ks_p_comparison
```

    ##  [1] 0.517550843 0.210551633 0.205836238 0.248548218 0.046139038
    ##  [6] 0.375211039 0.093531675 0.660386023 0.240209704 0.847488454
    ## [11] 0.569613206 0.125389519 0.181300450 0.240209704 0.617194991
    ## [16] 0.375211039 0.240209704 0.093531675 0.003860908 0.003860908
    ## [21] 0.046348278 0.807924160 0.248548218 0.152784340 0.333784269
    ## [26] 0.415365336 0.076262424 0.660386023 0.152784340 0.375211039
    ## [31] 0.375211039 0.093531675 0.152784340 0.003860908 0.999633292
    ## [36] 0.036631053

``` r
length(which(ks_p_comparison < 0.05))
```

    ## [1] 6
