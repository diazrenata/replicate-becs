## replicate-becs


This is an effort to replicate [Ernest 2005 "Body Size, Energy Use, and Community Structure in Small Mammals"](https://esajournals.onlinelibrary.wiley.com/doi/epdf/10.1890/03-3179). 

Renata Diaz (the author) is Morgan Ernest's PhD student. Dr. Ernest is aware of the project but has not been involved directly with it at any stage. 

The master branch of this repo should stay stable, but I am in the process of generalizing these functions and moving them to a separate package so they can be used for future projects. 

### Motivation

- Why replicate
  - Macroecology could benefit a lot from replication efforts. Highly statistical, potentially very sensitive to modeling assumptions. 
  - Barriers to replication are (in principle) lower: data are often available online; methods are generally computational rather than empirical. We just need to write some code, not re-run an expensive experiment. 
  - Online, version controlled open source projects are relatively new. Most foundational work in macroecology was conducted before coding was the norm, let alone a GitHub project. By bringing them online, we can better
    - prevent data/analysis rot
    - be specific about assumptions, idiosyncracies in data, limitations of past analytical methods that contribute to our foundational knowledge
    - make explicit comparisons of new results to old ones - new data, improved methods
- Why replicate this specifically
  - Considerable work and interest around body size-abundance and body size-energy use 

### Analysis
- For the full analysis, see the [full replication](https://github.com/diazrenata/replicate-becs/blob/master/analysis/ernest2005_replication.md). 

### Overall result
- Versions of the original datasets have come online, but not all of them correspond perfectly to the summary statistics reported in Ernest (2005) (#2). 
- All of the results can be replicated qualitatively, given particular assumptions. The p-values and test statistics do not align perfectly, but the statistical significance (or lack thereof) holds up.
- Some of these assumptions warrant further investigation. This would not have been practical in 2005, but is tractable given modern computing technology.

### Specific assumptions and next steps

- 'Uniform' baselines for BSEDs
  - See #4. 
  - To replicate Ernest 2005, we take a *uniform size-abundance relationship* as the null hypothesis/baseline distribution. This is not the same as a uniform size-energy relationship.
  - Given modern tools, it is relatively straightforward to compare the results under the following assumptions:
    - uniform size-abundance relationship
    - uniform size-energy relationship
- 'Uniform' comparison for BSDs (delta-corrrected KS tests)
  - To replicate the comparison of BSDs to uniform, we must compare the *log of the species mean mass* to a uniform distribution. This is not the same as a uniform species-body size relationship; it is a distribution with fewer large species than small ones. Both the log mass and mass on a linear scale are used to study BSDs.
  - Also, the K-S test as documented in Zar (1999) is somewhat ambiguously defined. (#5)
  - Given modern tools, we can:
    - Use bootstrapping instead of the K-S test to explore alternative hypotheses
    - Compare the results using:
        - uniform species-body size relationship at the log scale
        - uniform species-body size relationship
  - It may also be possible to test for multimodality using Gaussian mixture models (see method in Thibault et al 2011). 
- We can also use bootstrapping to compare BSDs to each other, instead of Kolmogorov-Smirnov tests.
- When making multiple comparisons between communities, we are more interested in how often communities are similar or distinguishable from each other than in whether a specific comparison has a p value of .05. It would be appropriate to use a false discovery rate correction to account for making many pairwise comparisons. 
