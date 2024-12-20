
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EnvSca

<!-- badges: start -->

[![R-CMD-check](https://github.com/mohit-chhaparia/EnvSca/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mohit-chhaparia/EnvSca/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The R package “EnvSca” provides functions for classification of
categorical time-series under the supervised learning paradigm as
proposed by the authors in the below linked paper. To construct
meaningful features for categorical time-series classification, the
authors considered two relevant quantities: the spectral envelope and
its corresponding set of optimal scalings. These quantities characterize
oscillatory patterns in a categorical time-series as the largest
possible power at each frequency, or spectral envelope, obtained by
assigning numerical values, or scalings, to categories that optimally
emphasize oscillations at each frequency. This procedure combines these
two quantities to produce a feature-based classifier that can be used to
accurately determine group membership for categorical time-series. For
reference, see the following:

[Li, Zeda, Scott A. Bruce, and Tian Cai. “Interpretable Classification
of Categorical Time Series using the Spectral Envelope and Optimal
Scalings.” Journal of Machine Learning Research 23, no. 299 (2022):
1-31.](https://jmlr.org/papers/v23/21-0369.html)

## Installation

You can install the development version of EnvSca from
[GitHub](https://github.com/mohit-chhaparia/EnvSca)

``` r
# install.packages("devtools")
devtools::install_github("mohit-chhaparia/EnvSca")
# or
devtools::install_github("mohit-chhaparia/EnvSca", build_vignettes=TRUE)
```
