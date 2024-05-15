<br />

<img align="right" width="185" src='man/figures/remify-logo.svg'>

## **remify** 

### _Processing and Transforming Relational Event History Data_

<!-- badges: start -->
[![github-repo-status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-package-version](https://img.shields.io/github/r-package/v/TilburgNetworkGroup/remify)](https://www.github.com/TilburgNetworkGroup/remify)
[![CRAN-release](https://www.r-pkg.org/badges/version/remify)](https://cran.r-project.org/package=remify)
[![R-CMD-check](https://github.com/TilburgNetworkGroup/remify/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/TilburgNetworkGroup/remify/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/TilburgNetworkGroup/remify/branch/master/graph/badge.svg?token=BDG8F1672B)](https://codecov.io/gh/TilburgNetworkGroup/remify)
[![grand-total-downloads](http://cranlogs.r-pkg.org/badges/grand-total/remify)](https://cran.r-project.org/package=remify)
<!-- badges: start -->

<br />

The aim of the `remify` package is twofold:

 * processing REH data and arranging them in a new structure (`remify`) that is used by the packages in [`remverse`](https://github.com/TilburgNetworkGroup/remverse);
 * transforming REH data from a `remify` structure to other formats used in other R packages.
 
The two main functions `remify()` and `rehshape()` perform respectively the processing and the conversion of the REH data.

<br />

### Installation
Install the package in R from CRAN:

```r
install.packages("remify")
library(remify)
```

### Vignettes
In order to provide a thorough explanation over the two main functions, both of them have a vignette where inputs, outputs, attributes and methods are described in detail.


List all the vignettes available with the installed version of `remify`

```r
vignette(package = "remify") 
```

Open the vignette of a specific topic

```r
vignette(topic = "remify", package = "remify") # or simply vignette("remify") 
```
<br />

### Problems while remify-ing?

Should you encounter errors while using the package, should you have questions that are not answered in the Vignettes, or for reporting any kind of malfunction of the package, you can open an issue [here](https://github.com/TilburgNetworkGroup/remify/issues). 

When opening an issue, please, use a descriptive title that clearly states the issue, be as thorough as possible when describing the issue, provide code snippets that can reproduce the issue.
<br />

### Funding
The funder of this work is the ERC and the ERC project number is 758791.
