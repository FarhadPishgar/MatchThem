# MatchThem <img src="man/figure/logo.png" align="right" width="150" />

<!-- badges: start -->
#### Matching and Weighting Multiply Imputed Datasets
<!-- badges: end -->

[![](https://img.shields.io/badge/CRAN%20version-0.8.1-success.svg?color=informational&style=for-the-badge)](https://cran.r-project.org/package=MatchThem)
[![](https://img.shields.io/badge/github%20version-0.8.2-success.svg?color=informational&style=for-the-badge)](https://github.com/FarhadPishgar/MatchThem)

## Introduction

One of the major issues in the matching procedures is the presence of missing data on the covariates or outcome indicator since matching requires comparing the values of covariates for units in control and treated subgroups or relies on the predictions from a logistic regression model and with missing values in the variables within the model, the comparison or predictions cannot be done for that unit. There are a couple of solutions to address this problem (including the complete-case analysis and the missingness pattern approach) and with flaws and limitations in these standard approaches, adopting algorithms to multiply impute the missing data is growing as a popular alternative.

Matching of control and treated units in multiply imputed datasets can be achieved through different approaches:

1. **The within (match-then-pool) approach**: In this approach, matching is done on each imputed dataset, using the observed and imputed covariate values, and the causal effects obtained from analyzing the matched datasets are pooled together (please see the article by [Leyrat et al.](https://www.ncbi.nlm.nih.gov/pubmed/28573919) for more details).
2. **The across (pool-then-match) approach**: In this approach, the calculated propensity scores for each unit across the imputed datasets are pooled and using this pooled measure, matching is done in the imputed datasets. The matched datasets are analyzed and the causal effects obtained from these analyses are pooled together (this approach is shown to produce biased estimates of the pooled causal effect, please see the article by [Mitra et al.](https://www.ncbi.nlm.nih.gov/pubmed/22687877) for more details).

The [`mice`](https://cran.r-project.org/package=mice) and [`Amelia`](https://cran.r-project.org/package=Amelia) packages are widely accepted statistical tools for imputing the ignorable missing data in the R platform. The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package simplifies the process of matching the imputed datasets obtained using these packages and enables credible adoption of the two matching approaches and several matching methods in practice. This package enables parametric models for causal inference to provide unbiased estimates through selecting matched units from the control and treated subgroups, analyzing the matched datasets, and pooling the obtained results on imputed datasets using Rubin’s rules.

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

Adopting algorithms to multiply impute the missing data, before the matching procedure, and the matching procedure itself may seem to be complicated tasks. This suggested workflow tries to map out this process into a few steps:

1. **Imputing the Missing Data in the Dataset**: The [`mice`](https://cran.r-project.org/package=mice) and [`Amelia`](https://cran.r-project.org/package=Amelia) packages can be used to multiply impute the missing data in the dataset (the [`Amelia`](https://cran.r-project.org/package=Amelia) package is designed to impute missing data in a single cross-sectional dataset or in a time-series dataset, the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package only supports the former data type).
2. **Matching the Imputed Datasets**: The `matchthem()` command from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be used to select matched units from control and treated subgroups of each imputed dataset.
3. **Assessing Balance on the Matched Datasets**: The [`cobalt`](https://cran.r-project.org/package=cobalt) package should be used to assess the extent of the balance for all covariates in the imputed datasets after matching.
4. **Analyzing the Matched Datasets**: The `with()` command from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be used to estimate causal effect by analyzing the matched datasets.
5. **Pooling the Causal Effect Estimates**: The `pool()` command from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be used to pool the obtained causal effect estimates from the previous step using Rubin’s rules.

### Imputing the Missing Data in the Dataset

The [`mice`](https://cran.r-project.org/package=mice) and [`Amelia`](https://cran.r-project.org/package=Amelia) packages and their main functions provide the necessary tools for multiply imputing ignorable missing data in a dataset (several points should be considered before choosing the appropriate method for the imputation procedure, please see these packages reference manuals for details).

### Matching the Imputed Datasets

The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package and its main function, `matchthem()`, provides the essential tools for selecting matched units from control and treated subgroups of imputed datasets. Currently, two matching approaches (within and across matching approaches) and several matching methods (nearest neighbor, exact, full, genetic, subclassification, coarsened exact, and optimal matching methods) are available (within each of these approaches and methods, the `matchthem()` offers a variety of options).

The output of the `matchthem()` command will be saved in an object of the `mimids` class. The `plot()`, `print()`, and `summary()` can be used to review detailed descriptions of these objects. Moreover, `complete()` can be used to extract the matched datasets in these objects.

### Assessing Balance on the Matched Datasets

Function from the [`cobalt`](https://cran.r-project.org/package=cobalt) package should be used to assess average and maximum of the (absolute) standardized mean differences for all covariates as a measure of the extent of the balance in the imputed datasets after matching (please see this package reference manuals for details).

### Analyzing the Matched Datasets

The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package and one of its functions, `with()`, provides an easy-to-use tool for analyzing each matched dataset.

The output of the `with()` command will be saved in an object of the `mira` class. The `print()` and `summary()` can be used to review detailed descriptions of these objects.

### Pooling the Causal Effect Estimates
The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package and one of its functions, `pool()`, can be used to pool the obtained causal effect estimates from data analyses according to Rubin’s rules.

The output of the `pool()` command will be saved in an object of the `mipo` class. The `print()` and `summary()` can be used to review detailed descriptions of these objects.

## Acknowledgments
The logo for this package, [a trip to the Arctic](https://dribbble.com/shots/1652911-A-trip-to-the-Arctic), was designed and kindly provided by Max Josino (check his [website](http://maxjosino.co/) and [Dribble](https://dribbble.com/maxjosino) to see his works).

We would like to thank the CRAN team members for their technical support and comments on the package performance. This package relies on the [`MatchIt`](https://cran.r-project.org/package=MatchIt), [`mice`](https://cran.r-project.org/package=mice), and [`WeightIt`](https://cran.r-project.org/package=WeightIt) packages. Please cite their reference manuals and vignettes in your work besides citing reference manual and vignette of this package.

## Authors
Farhad Pishgar

[![](https://img.shields.io/twitter/follow/FarhadPishgar.svg?color=informational&style=for-the-badge)](https://twitter.com/FarhadPishgar)

Noah Greifer

[![](https://img.shields.io/github/followers/ngreifer.svg?style=for-the-badge&color=informational&label=follow%20@NGreifer)](https://github.com/NGreifer)

Clémence Leyrat

[![](https://img.shields.io/twitter/follow/LeyClem.svg?color=informational&style=for-the-badge)](https://twitter.com/LeyClem)
