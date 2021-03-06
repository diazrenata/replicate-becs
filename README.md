[![Build Status](https://travis-ci.com/diazrenata/replicate-becs.svg?branch=master)](https://travis-ci.com/diazrenata/replicate-becs) [![codecov](https://codecov.io/gh/diazrenata/replicate-becs/branch/master/graph/badge.svg)](https://codecov.io/gh/diazrenata/replicate-becs)

## replicate-becs


This is an effort to replicate [Ernest 2005 "Body Size, Energy Use, and Community Structure in Small Mammals"](https://esajournals.onlinelibrary.wiley.com/doi/epdf/10.1890/03-3179). 

Renata Diaz (the author) is Morgan Ernest's PhD student. Dr. Ernest is aware of the project but has not been involved directly with it at any stage. 

The master branch of this repo should stay stable, but I (Renata) am in the process of generalizing these functions and moving them to a separate package so they can be used for future projects. 

### Motivation

- Why replicate
  - Macroecology could benefit a lot from replication efforts. Highly statistical, potentially very sensitive to modeling assumptions. 
  - Barriers to replication are (in principle) lower: data are often available online; methods are generally computational rather than empirical. We just need to write some code, not re-run an expensive experiment. 
  - Online, version controlled open source projects are relatively new. Most foundational work in macroecology was conducted before coding was the norm, let GitHub projects. By bringing them online, we can better
    - prevent data/analysis rot
    - support high standards of replicable, open science, from the roots of the field on up
    - be specific about assumptions, idiosyncracies in data, limitations of past analytical methods that contribute to our foundational knowledge
    - make explicit comparisons of new results to old ones - new data, improved methods
- Why replicate this specifically
  - Considerable work and interest around body size-abundance and body size-energy use, but we still don't know very much about these relationships in terrestrial animal communities (but see Thibault, Baiser/other smammal paper). 
  - Challenges: lack of data; need for *precision* about 
    - which relationships we can explore with the data we have
    - what assumptions and null models we are bringing to a project
    - which relationships we are learning about in a given analysis (the language is often confusing - many similar terms).
  - Fundamental questions and debates are still unresolved at the community level:
    - Species-body size relationships (size vs. no of species) 
      - What do empirical communities look like? Basic documentation.
      - "Clumped", or random?
      - Multimodal? Consistencies across communities? Idiosyncratic?
    - Body size-abundance relationships *at the community scale* (size vs. no of individuals)
    - Body size-energy use relationships *at the community scale* (size vs. total energy use)
      - Does energetic equivalence emerge?
  - Ernest (2005) was an early analysis of these relationships in small mammal communities, using predominately desert rodent communities. 
  - Thanks to NEON and other efforts, we now have a much larger and more diverse collection of small mammal community data we can use to explore these questions more generally and more rigorously. 
  - Replicating Ernest (2005) is a first step towards a broader and more computationally intensive analysis of small mammal community distributions (Diaz in prep....)
    - brings a pre-github analysis into the github era
    - add context to our existing knowledge base: what are the assumptions, what are the specific results
    - track changes in analysis methods & results; understand and resolve conflicting results if they occur. 

### Analysis
- For the full analysis, see the [full replication](https://github.com/diazrenata/replicate-becs/blob/master/report/ernest_2005_replication.md). 

### Overall result
- Versions of the original datasets have come online, but not all of them correspond perfectly to the summary statistics reported in Ernest (2005) (#2). 
- All of the results can be replicated qualitatively, given particular assumptions. The p-values and test statistics do not align perfectly, but the statistical significance (or lack thereof) holds up.

### Specific assumptions and next steps

- 'Uniform' baselines for BSEDs
  - To replicate Ernest 2005, we take a uniform size-abundance relationship at the linear (not log transformed) scale as the null hypothesis/baseline distribution.
- 'Uniform' comparison for BSDs (delta-corrrected KS tests)
  - To replicate the comparison of BSDs to uniform, we must compare the *log of the species mean mass* to a uniform distribution. This is not the same as a uniform species-body size relationship; it is a distribution with fewer large species than small ones. Both the log mass and mass on a linear scale are standard in studies of BSDs. 
  - Also, the K-S test as documented in Zar (1999) is somewhat ambiguously defined. (#5)
