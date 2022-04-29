# remify (Version: 2.0.0)

### Table of contents
- [remify (Version: 2.0.0)](#remify-version-200)
    - [Table of contents](#table-of-contents)
    - [About the package](#about-the-package)
    - [Programming Languages](#programming-languages)
    - [Installing the package](#installing-the-package)
    - [Vignettes](#vignettes)

### About the package
The aim of the `remify` package is twofold:
 * processing REH data and arranging them in a new structure (`reh`) that is used by the packages in `remverse`;
 * transforming REH data from other formats or other sources to a `reh` structure (or vice versa).
 
The two main functions `reh()` and `remify()` perform respectively the processing and the conversion of the REH data.

### Programming Languages
The package contains code written in:
* R (>= 4.0.0)
* Rcpp (>= 1.0.8.3) and RcppArmadillo (>= 0.11)
* C++14
	
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

