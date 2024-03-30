# MatchThem <img src="man/figure/logo.png" align="right" width="150" />

<!-- badges: start -->
#### Matching and Weighting Multiply Imputed Datasets
<!-- badges: end -->

[![](https://img.shields.io/badge/CRAN%20version-1.2.1-success.svg?color=informational&style=for-the-badge)](https://cran.r-project.org/package=MatchThem)
[![](https://img.shields.io/badge/github%20version-1.2.1-success.svg?color=informational&style=for-the-badge)](https://github.com/FarhadPishgar/MatchThem)

## What's New

### Version 1.2.1

Support for using `glm_weightit()` in the outcome model after weighting using `weightthem()` is added.

### Version 1.2.0

New functions `as.mimids()` and `as.wimids()` are now available; these take a list of `matchit` or `weightit` objects fit to multiply imputed data and transform them into a `mimids` or `wimids` object, respectively, for use in balance assessment and to take advantage of `MatchThem`'s other capabilities.

### Version 1.1.0

In this update, the `matchthem()` function has been enhanced to include support for the `quick` and `cardinality` matching methods. These additional methods offer users more options for their matching analyses, catering to a wider range of use cases and data characteristics. Alongside the matching method updates, this release also incorporates improved documentation and inclusion of a newly introduced weighting approach by Nguyen and Stuart, known as averaging probability weights (`apw`, please see their paper [here](https://arxiv.org/abs/2301.07066)).

### Version 1.0.2

This update is dedicated to enhancing the documentation, providing clearer explanations, examples, and instructions to improve the usability and understanding of the package. The focus is on ensuring that users have comprehensive and detailed documentation to make the most out of the package's features and functionality.

### Version 1.0.1

This update is primarily focused on enhancing the documentation and addressing minor bugs in order to improve the overall performance and user experience of the package. The necessary fixes have been implemented to ensure smoother operation and to address any reported issues.

### Version 1.0.0

This update brings significant improvements to the documentation and introduces several new features: 1. The `mimira` and `mimipo` objects, which are the output of `with()` and `pool()` functions respectively, now inherit from the [`mice`](https://cran.r-project.org/package=mice) classes `mira` and `mipo`. This allows for seamless integration with existing [`mice`](https://cran.r-project.org/package=mice) methods, 2. When using `coxph()` with `with()`, the update ensures that the robust standard errors are correctly applied, 3. A new cluster argument has been added to `with.mimids()` function. This argument controls whether cluster-robust standard errors should be used to account for pair membership when the model is a `svyglm()`-type model from the [`survey`](https://cran.r-project.org/package=survey) package. By default, pair membership is included when present and there are 20 or more unique subclasses (pairs), 4. The `cbind()` methods have been documented and exported, 5. The `mimids` and `wimids` objects have been optimized to reduce their size. They now only contain the supplied `mids` object and the outputs from `matchit()` or `weightit()`, and 6. A new `trim()` function has been added to trim estimated weights, utilizing `WeightIt::trim()` with the same syntax (credits go to Nicolas for this contribution).

These updates enhance the functionality, flexibility, and efficiency of the package, providing users with an improved experience.

### Version 0.9.3

This update focuses on resolving a few bugs, resulting in improved stability and functionality of the package. The necessary fixes have been implemented to address these issues and enhance the overall performance.

### Version 0.9.2

This update includes a modification to the `complete()` function in order to avoid any potential conflicts with the [`tidyr`](https://cran.r-project.org/package=tidyr) package. The updated definition of the `complete()` function ensures smooth compatibility and eliminates any name clashes that might have occurred.

### Version 0.9.1

This update focuses on enhancing documentation and addressing minor bugs, resulting in improved overall performance and user experience.

### Version 0.9.0

This update focuses on enhancing documentation and implementing compatibility for robust estimation of standard errors. Specifically, the package now supports compatibility with the `svyglm()` and `svycoxph()` functions from the [`survey`](https://cran.r-project.org/package=survey) package, allowing them to be used as expressions within the `with()` function. Additionally, new matching and weighting methods have been introduced, such as the `full`, `genetic`, and `cem` matching methods, as well as the `ebal` and `optweight` weighting methods.

Furthermore, the package now includes the `complete()` function, which replaces the previous `matchthem.data()` and `weightthem.data()` functions. This new function provides improved functionality and convenience for completing the required data operations within the package.

### Version 0.8.2

This update focuses on enhancing the documentation and addressing minor bugs to improve the overall quality of the package. It includes necessary refinements and fixes to ensure a more polished and seamless user experience.

### Version 0.8.1

The [`MatchThem`](https://cran.r-project.org/package=MatchThem) package has been released and can now be accessed on [Github](https://github.com/FarhadPishgar/MatchThem) and [Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/package=MatchThem).

## Authors
[![](https://img.shields.io/badge/Farhad%20Pishgar-success.svg?color=informational&style=for-the-badge)](https://twitter.com/FarhadPishgar)
[![](https://img.shields.io/badge/Noah%20Greifer-success.svg?color=informational&style=for-the-badge)](https://twitter.com/Noah_Greifer)
[![](https://img.shields.io/badge/Clémence%20Leyrat-success.svg?color=informational&style=for-the-badge)](https://twitter.com/LeyClem)
[![](https://img.shields.io/badge/Elizabeth%20Stuart-success.svg?color=informational&style=for-the-badge)](https://twitter.com/LizStuartdc)
