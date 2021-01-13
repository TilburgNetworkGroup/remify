# remify (Version: 1.1.0)

### Table of contents
* [About the package](#about-the-package)
* [Programming Languages](#programming-languages)
* [Installing the package](#installing-the-package)
* [Vignettes](#Vignettes)

### About the package
The aim of the `remify` package is twofold:
 * processing REH data and arranging them in a new structure (`reh`) that is used by the packages of `remverse`;
 * transforming REH data from other formats or other sources to a `reh` structure (or vice versa).
The two main functions `reh()` and `remify()` perform respectively the processing and the conversion of the REH data.

### Programming Languages
The package contains code written in:
* R (>= 4.0.0)
* Rcpp (>= 1.0.4.6) and RcppArmadillo (>= 0.9.860.2.0)
* C++11 (Compiler Version: GCC-8.1.0)
	
### Installing the package
To install the package in R using `devtools`:

```
library(devtools)
devtools::install_github(repo = "TilburgNetworkGroup/remify", build_vignettes = TRUE)

# load the package
library(remify)
```

### Vignettes
In order to provide as much information as possible over the use of the two functions, both function has a vignette where inputs, outputs, attributes and methods are explained.
(Note: currently only the vignette for `reh()` is available)
```
vignette(package = "remify") # returns all the vignette available with the current version of the package
vignette(topic = "reh", package = "remify") # or simply vignette("reh") opens the vignette for the processing function reh()
```

