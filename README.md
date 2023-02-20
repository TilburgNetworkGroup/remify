# remify (Version: 2.0.0)

[![R-CMD-check](https://github.com/TilburgNetworkGroup/remify/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/TilburgNetworkGroup/remify/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/TilburgNetworkGroup/remify/branch/master/graph/badge.svg?token=BDG8F1672B)](https://codecov.io/gh/TilburgNetworkGroup/remify)

### A package for pre-processing the structure of REH data


### Table of contents
- [About the package](#about-the-package)
- [Programming Languages](#programming-languages)
- [Installing the package](#installing-the-package)
- [Vignettes](#vignettes)
- [Funding](#funding)
- [Author](#author)


### About the package
The aim of the `remify` package is twofold:
 * processing REH data and arranging them in a new structure (`reh`) that is used by the packages in `remverse`;
 * transforming REH data from other formats or other sources to a `reh` structure (or vice versa).
 
The two main functions `reh()` and `rehshape()` perform respectively the processing and the conversion of the REH data.

### Programming Languages
The package contains code written in:
* R (>= 4.2.0)
* Rcpp (>= 1.0.8.3) and RcppArmadillo (>= 0.11)
* C++14

_n.b._ for compilation purposes, the package needs RTools (for R >= 4.2.0) installed.
	
### Installing the package
To install the package in R using `devtools`:

```
library(devtools)
devtools::install_github(repo = "TilburgNetworkGroup/remify", build_vignettes = TRUE)

# load the package
library(remify)
```

### Vignettes
In order to provide a thorough explanation over the two main functions, both of them have a vignette where inputs, outputs, attributes and methods are described in detail.

(_Note_: currently only the vignette for `reh()` is available)

```
vignette(package = "remify") # returns all the vignettes available with the current version of the package

vignette(topic = "reh", package = "remify") # or simply vignette("reh") opens the vignette for the processing function reh()
```

### Funding
The funder of this work is the ERC and the ERC project number is 758791.

### Author
Giuseppe Arena, Tilburg University (Tilburg, The Netherlands). (g.arena@tilburguniversity.edu)
