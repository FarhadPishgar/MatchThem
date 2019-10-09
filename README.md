# MatchThem <img src="man/figure/logo.png" align="right" width="150" />

<!-- badges: start -->
#### Matching Multiply Imputed Datasets
<!-- badges: end -->

[![](https://img.shields.io/badge/CRAN%20version-0.8.1-success.svg?color=informational&style=for-the-badge)](https://cran.r-project.org/package=MatchThem)
[![](https://img.shields.io/badge/github%20version-0.8.1-success.svg?color=informational&style=for-the-badge)](https://github.com/FarhadPishgar/MatchThem)

## Introduction

One of the major issues in the propensity score matching procedures is the presence of missing data on the treatment indicator or covariates since matching relies on the predictions from a logistic regression model and with missing information in the variables within the model, the predictions cannot be made for that observation. There are a couple of solutions to address this problem (including the complete-case analysis and the missingness pattern approach) and despite these standard approaches, adopting algorithms to multiply impute the missing data is growing as a popular alternative.

Matching of control and treatment observations based on the propensity score in multiply imputed datasets can be achieved through different approaches:

1. **The within (match-then-pool) approach**: In this approach, matching is done on each imputed dataset, using the observed and imputed covariate values, and the treatment effect sizes obtained from analyzing the matched datasets are pooled together (please see the article by [Leyrat et al.](https://www.ncbi.nlm.nih.gov/pubmed/28573919) for more details).
2. **The across (pool-then-match) approach**: In this approach, the calculated propensity scores for each observation across the imputed datasets are pooled and using this pooled measure, matching is done on the imputed datasets. The matched datasets are analyzed and the treatment effect sizes obtained from these analyses are pooled together (this method is shown to produce inconsistent estimates of the treatment effect size, please see the article by [Mitra et al.](https://www.ncbi.nlm.nih.gov/pubmed/22687877) for more details).

The [`mice`](https://cran.r-project.org/package=mice) and [`Amelia`](https://cran.r-project.org/package=Amelia) packages are widely accepted statistical tools for imputing the ignorable missing data in the R platform. The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package simplifies the process of matching the imputed datasets obtained using these packages and enables credible adoption of the two matching approaches and several matching methods in practice. This package enables parametric models for causal inference to provide unbiased estimates through selecting matched observations from the control and treatment groups, analyzing the matched datasets, and pooling the obtained results on imputed datasets using Rubin’s rules.

## Installation

The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package can be installed from the Comprehensive R Archive Network (CRAN) repository as follows:

``` r
install.packages("MatchThem")
```

The latest (though unstable) version of the package can be installed from GitHub as follows:

``` r
devtools::install_github(repo = "FarhadPishgar/MatchThem")
```

## Suggested Workflow

### Overview

Adopting algorithms to multiply impute the missing data, before the matching procedure, and the matching procedure itself may seem to be complicated tasks. This suggested workflow tries to simplify this process into a few steps:

1. **Imputing the Missing Data in the Dataset**: Functions of the [`mice`](https://cran.r-project.org/package=mice) and [`Amelia`](https://cran.r-project.org/package=Amelia) packages can be used to multiply impute the missing data in the dataset (the [`Amelia`](https://cran.r-project.org/package=Amelia) package is designed to impute missing data in a single cross-sectional dataset or in a time-series dataset, although it may work with the latter, the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package only supports the former data type).
2. **Matching the Imputed Datasets**: The `matchthem()` function from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be used to select matched observations from control and treatment groups of each imputed dataset (don't forget to check the extent of the balance in covariates after matching. You can use the [`cobalt`](https://cran.r-project.org/package=cobalt) package for this purpose, which is now compatible with the `mimids` and `wimids` objects, as well as, the tools provided in the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package, itself).
3. **Analyzing the Matched Datasets**: The `with()` function from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be used to estimate treatment effect size by analyzing the matched datasets.
4. **Pooling the Treatment Effect Size Estimates**: The `pool()` function from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be used to pool the obtained treatment effect estimates from the previous step using Rubin’s rules.

### Imputing the Missing Data in the Dataset

The [`mice`](https://cran.r-project.org/package=mice) and [`Amelia`](https://cran.r-project.org/package=Amelia) packages and their main functions provide the necessary tools for multiply imputing ignorable missing data in a dataset (several points should be considered before choosing the appropriate method for the imputation procedure, please see these packages reference manuals for details).

### Matching the Imputed Datasets

The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package and its main function, `matchthem()`, provides the essential tools for selecting matched observations from control and treatment groups of imputed datasets. Currently, two matching approaches (match-then-pool and pool-then-match matching approaches) and two matching methods (nearest neighbor and exact matching methods) are available (within each of these approaches and methods, the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package offers a variety of options).

The `matchthem()` function requires the imputed datasets not to have any missing data (even in variables not included in the matching model).

The output of the `matchthem()` function will be saved in an object of the `mimids` class. The `plot()`, `print()`, and `summary()` functions can be used to review detailed descriptions of these objects. Moreover, `matchthem.data()` function can be used to extract the matched datasets in these objects.

### Analyzing the Matched Datasets

The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package and one of its functions, `with()`, provides an easy way to analyze each matched imputed dataset.

The output of the `with()` function will be saved in an object of the `mira` class. The `print()` and `summary()` functions can be used to review detailed descriptions of these objects.

### Pooling the Treatment Effect Size Estimates
The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package and one of its functions, `pool()`, provides the tools to easily pool the obtained treatment effect estimates from complete data analyses according to Rubin’s rules.

The output of the `pool()` function will be saved in an object of the `mipo` class. The `print()` and `summary()` functions can be used to review detailed descriptions of these objects.

## Acknowledgments
The logo for this package, [a trip to the Arctic](https://dribbble.com/shots/1652911-A-trip-to-the-Arctic), was designed and kindly provided by Max Josino (check his [website](http://maxjosino.co/) and [Dribble](https://dribbble.com/maxjosino) to see his beautiful works).

I would like to thank the CRAN team members for their comments and technical support. This package relies on the [`Amelia`](https://cran.r-project.org/package=Amelia), [`MatchIt`](https://cran.r-project.org/package=MatchIt), [`mice`](https://cran.r-project.org/package=mice), and [`WeightIt`](https://cran.r-project.org/package=WeightIt) packages. Please cite their reference manuals and vignettes in your work besides citing reference manual and vignette of this package.

## Authors
Farhad Pishgar

[![](https://img.shields.io/twitter/follow/FarhadPishgar.svg?color=informational&style=for-the-badge)](https://twitter.com/FarhadPishgar)

Clémence Leyrat

[![](https://img.shields.io/twitter/follow/LeyClem.svg?color=informational&style=for-the-badge)](https://twitter.com/LeyClem)
