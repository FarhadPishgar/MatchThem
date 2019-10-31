# MatchThem <img src="man/figure/logo.png" align="right" width="150" />

<!-- badges: start -->
#### Matching and Weighting Multiply Imputed Datasets
<!-- badges: end -->

[![](https://img.shields.io/badge/CRAN%20version-0.9.0-success.svg?color=informational&style=for-the-badge)](https://cran.r-project.org/package=MatchThem)
[![](https://img.shields.io/badge/github%20version-0.9.0-success.svg?color=informational&style=for-the-badge)](https://github.com/FarhadPishgar/MatchThem)

## Introduction

One of the major issues in the matching procedures is the presence of missing data on the covariates or outcome indicator since matching requires comparing the values of covariates for units in control and treated subgroups or relies on the predictions from a logistic regression model, and with missing values in the covariates within the model, the comparison or predictions cannot be done for that unit. There are a couple of solutions to address this problem (including the complete-case analysis) and with flaws and limitations in these approaches, adopting algorithms to multiply impute the missing data is growing as a popular alternative.

The [`mice`](https://cran.r-project.org/package=mice) and [`Amelia`](https://cran.r-project.org/package=Amelia) packages are widely accepted statistical tools for imputing the ignorable missing data in the R platform. The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package simplifies the process of matching the imputed datasets by these packages and enables credible adoption of the two matching approaches (within and across) and several matching methods in practice.

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

Adopting algorithms to multiply impute the missing data, before the matching procedure, and the matching procedure itself may seem to be complicated tasks. This suggested workflow tries to map out this process into five steps (please see the package [cheat sheet](inst/doc/cheatsheet.pdf) for more details):

1. **Imputing the Missing Data in the Dataset**: [`mice`](https://cran.r-project.org/package=mice) and [`Amelia`](https://cran.r-project.org/package=Amelia) packages should be used to multiply impute the missing data in the dataset.
2. **Matching the Imputed Datasets**: `matchthem()` from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be used to select matched units from control and treated subgroups of each imputed dataset.
3. **Assessing Balance on the Matched Datasets**: [`cobalt`](https://cran.r-project.org/package=cobalt) package should be used to assess the extent of balance for all covariates in the imputed datasets after matching.
4. **Analyzing the Matched Datasets**: `with()` from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be used to estimate causal effects in each matched dataset.
5. **Pooling the Causal Effect Estimates**: `pool()` from the [`MatchThem`](https://cran.r-project.org/package=MatchThem) package should be used to pool the obtained causal effect estimates from analyzing each dataset.

## Acknowledgments
The logo for this package, [a trip to the Arctic](https://dribbble.com/shots/1652911-A-trip-to-the-Arctic), was designed and kindly provided by Max Josino (please see his [website](http://maxjosino.co/) and [Dribble](https://dribbble.com/maxjosino) to see his works).

We would like to thank the CRAN team members for their technical support and comments on the package performance. This package relies on the [`MatchIt`](https://cran.r-project.org/package=MatchIt), [`mice`](https://cran.r-project.org/package=mice), and [`WeightIt`](https://cran.r-project.org/package=WeightIt) packages. Please cite their reference manuals and vignettes in your work besides citing the reference manual and vignette of this package.

## Authors
Farhad Pishgar

[![](https://img.shields.io/twitter/follow/FarhadPishgar.svg?color=informational&style=for-the-badge)](https://twitter.com/FarhadPishgar)

Noah Greifer

[![](https://img.shields.io/github/followers/ngreifer.svg?style=for-the-badge&color=informational&label=follow%20@NGreifer)](https://github.com/NGreifer)

Clémence Leyrat

[![](https://img.shields.io/twitter/follow/LeyClem.svg?color=informational&style=for-the-badge)](https://twitter.com/LeyClem)
