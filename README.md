# remify <img align="right" width="185" src='man/figures/remify-logo.svg'>

[![github-repo-status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-package-version](https://img.shields.io/github/r-package/v/TilburgNetworkGroup/remify)](https://www.github.com/TilburgNetworkGroup/remify)
[![R-CMD-check](https://github.com/TilburgNetworkGroup/remify/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/TilburgNetworkGroup/remify/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/TilburgNetworkGroup/remify/branch/master/graph/badge.svg?token=BDG8F1672B)](https://codecov.io/gh/TilburgNetworkGroup/remify)

## Processing and transforming relational event history data

The aim of the `remify` package is twofold:
 * processing REH data and arranging them in a new structure (`remify`) that is used by the packages in [`remverse`](https://github.com/TilburgNetworkGroup/remverse);
 * transforming REH data from a `remify` structure to other formats used in other R packages.
 
The two main functions `remify()` and `rehshape()` perform respectively the processing and the conversion of the REH data.

	
## Installation
Install the package in R using `devtools` or `remotes`:

```
# via `devtools`
devtools::install_github(repo = "TilburgNetworkGroup/remify", build_vignettes = TRUE)

# via `remotes`
remotes::install_github(repo = "TilburgNetworkGroup/remify", build_vignettes = TRUE)
```

## Vignettes
In order to provide a thorough explanation over the two main functions, both of them have a vignette where inputs, outputs, attributes and methods are described in detail.


List all the vignettes available with the installed version of `remify`

```
vignette(package = "remify") 
```

Open the vignette of a specific topic

```
vignette(topic = "remify", package = "remify") # or simply vignette("remify") 
```

## Author
Giuseppe Arena, Tilburg University (Tilburg, The Netherlands). (g.arena@tilburguniversity.edu)

## Funding
The funder of this work is the ERC and the ERC project number is 758791.
