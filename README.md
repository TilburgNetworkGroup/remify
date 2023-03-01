# remify <img align="right" width="135" src='man/figures/remify-logo.png'>

[![github-repo-status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-package-version](https://img.shields.io/github/r-package/v/TilburgNetworkGroup/remify)](https://www.github.com/TilburgNetworkGroup/remify)
[![R-CMD-check](https://github.com/TilburgNetworkGroup/remify/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/TilburgNetworkGroup/remify/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/TilburgNetworkGroup/remify/graph/badge.svg?token=BDG8F1672B)](https://codecov.io/gh/TilburgNetworkGroup/remify)

## A package for pre-processing the structure of REH data

The aim of the `remify` package is twofold:
 * processing REH data and arranging them in a new structure (`reh`) that is used by the packages in `remverse`;
 * transforming REH data from other formats or other sources to a `reh` structure (or vice versa).
 
The two main functions `reh()` and `rehshape()` perform respectively the processing and the conversion of the REH data.

	
## Installation
Install the package in R using `devtools`:

```
devtools::install_github(repo = "TilburgNetworkGroup/remify", build_vignettes = TRUE)
```

## Vignettes
In order to provide a thorough explanation over the two main functions, both of them have a vignette where inputs, outputs, attributes and methods are described in detail.

(_Note_: currently only the vignette for `reh()` is available)

List all the vignettes available with the installed version of `remify`
```
vignette(package = "remify") 
```

Open the vignette of a specific topic

```
vignette(topic = "reh", package = "remify") # or simply vignette("reh") 
```

## Author
Giuseppe Arena, Tilburg University (Tilburg, The Netherlands). (g.arena@tilburguniversity.edu)

## Funding
The funder of this work is the ERC and the ERC project number is 758791.
