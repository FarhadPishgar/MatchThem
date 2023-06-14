# MatchThem <img src="man/figure/logo.png" align="right" width="150" />

<!-- badges: start -->
#### Matching and Weighting Multiply Imputed Datasets
<!-- badges: end -->

[![](https://img.shields.io/badge/CRAN%20version-1.1.0-success.svg?color=informational&style=for-the-badge)](https://cran.r-project.org/package=MatchThem)
[![](https://img.shields.io/badge/github%20version-1.1.0-success.svg?color=informational&style=for-the-badge)](https://github.com/FarhadPishgar/MatchThem)

## Introduction

One of the significant challenges in matching procedures is the occurrence of missing data on the covariates. Matching involves comparing the values of covariates for units in control and treated subgroups, or relying on predictions from a logistic regression model. When there are missing values in the covariates within the model, it becomes impossible to make a valid comparison or generate accurate predictions for that unit. To tackle this issue, several solutions have been proposed, including complete-case analysis. However, these approaches have their flaws and limitations. As a result, the adoption of algorithms for multiply imputing the missing data is gaining popularity as an alternative.

The [`mice`](https://cran.r-project.org/package=mice) and [`Amelia`](https://cran.r-project.org/package=Amelia) packages are recognized statistical tools for imputing missing data within the R. In combination with these packages, the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package streamlines the matching and weighting processes for multiply imputed datasets. It facilitates the credible implementation of matching and weighting approaches and methods in practical applications.

## Installation

The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package can be installed from the Comprehensive R Archive Network (CRAN) repository:

``` r
install.packages("MatchThem")
```

And, the latest version of the package can be installed from GitHub:

``` r
devtools::install_github(repo = "FarhadPishgar/MatchThem")
```

## Suggested Workflow

Implementing algorithms for multiple imputation of missing data, as well as the matching or weighting procedures, may appear complex at first. To simplify this process, a suggested workflow has been designed, consisting of five steps. For more detailed information, please refer to the package's [cheat sheet](https://cran.r-project.org/package=MatchThem/vignettes/cheatsheet.pdf) or [vignette](https://cran.r-project.org/package=MatchThem/vignettes/vignette.pdf).

1. **Multiply Imputing of Missing Data in the Dataset**: [`mice`](https://cran.r-project.org/package=mice) and [`Amelia`](https://cran.r-project.org/package=Amelia) packages are recommended for performing multiple imputation of missing data in the dataset.
2. **Matching or Weighting the Multiply Imputed Datasets**: The matching procedure for selecting matched units from the control and treated subgroups of each imputed dataset or the weighting procedure can be accomplished using the `matchthem()` or `weightthem()` functions provided by the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package.
3. **Assessing Balance on Matched or Weighted Datasets**: To evaluate the balance of all covariates in the multiply imputed datasets after matching or weighting, the [`cobalt`](https://cran.r-project.org/package=cobalt) package can be employed. It provides tools and functions specifically designed for assessing the extent of balance.
4. **Analyzing the Matched or Weighted Datasets**: To estimate causal effects in each matched or weighted dataset, the `with()` function from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be utilized. This function provides the necessary tools for conducting the analysis on the datasets.
5. **Pooling the Causal Effect Estimates**: To combine the causal effect estimates obtained from analyzing each dataset, the `pool()` function from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be employed. This function facilitates the pooling of the estimates to obtain an overall estimate of the causal effects.

## Acknowledgments
The logo for this package, [a trip to the Arctic](https://dribbble.com/shots/1652911-A-trip-to-the-Arctic), was designed by Max Josino. You can view and explore more of his work on his [website](http://maxjosino.co/) and [Dribble](https://dribbble.com/maxjosino) profile. We sincerely thank Max Josino for his kind contribution. This package relies on the functionality provided by the [`mice`](https://cran.r-project.org/package=mice), [`MatchIt`](https://cran.r-project.org/package=MatchIt), and [`WeightIt`](https://cran.r-project.org/package=WeightIt) packages.

## Authors
[![](https://img.shields.io/badge/Farhad%20Pishgar-success.svg?color=informational&style=for-the-badge)](https://twitter.com/FarhadPishgar)
[![](https://img.shields.io/badge/Noah%20Greifer-success.svg?color=informational&style=for-the-badge)](https://twitter.com/Noah_Greifer)
[![](https://img.shields.io/badge/Clémence%20Leyrat-success.svg?color=informational&style=for-the-badge)](https://twitter.com/LeyClem)
[![](https://img.shields.io/badge/Elizabeth%20Stuart-success.svg?color=informational&style=for-the-badge)](https://twitter.com/LizStuartdc)
