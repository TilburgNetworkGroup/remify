# remify (Version: 1.1.0)
# 
### Table of contents
* [About the package](#about-the-package)
* [Programming Languages](#programming-languages)
* [Installing the package](#installing-the-package)
* [How to process data](#how-to-process-data)

### About the package
The `remify` package transforms REH data from/to other formats or other sources to a REH structure that is suitable for the packages in `remverse`


### Programming Languages
The package contains code written in:
* R (>= 4.0.0)
* Rcpp (>= 1.0.4.6) and RcppArmadillo (>= 0.9.860.2.0)
* C++11 (Compiler Version: GCC-8.1.0)
	
### Installing the package
To install the package in R using `devtools`:

```
library(devtools)
install_github("TilburgNetworkGroup/remify")

# load the package
library(remify)
```

### How to process data
Short tutorial about supplying arguments to the function `remify::reh()` and get the preprocessed data as output (note: the initial check of inputs is not yet part of the function as well as attributes and methods are not yet implemented in this version of the package).
```
library(remify)
load(file.choose()) # load edgelist object: it must be a data.frame with columnames [time,actor1,actor2,type,weight] 
(it is not necessary to define a type or a weight column but if they are present in the edgelist they must be named as 
mentioned above, whereas it is compulsory to supply [time,actor1,actor2])

reh_processed <- remify::reh(edgelist = edgelist, # edgelist to process
                covariates = list(default = NULL), # `covariates` can be left unspecified 
                                                      (this argument will be removed with the next update)
                actors = NULL, # vector of actor names (can be also left NULL)
                types = NULL,  # vector of type names (can be also left NULL)
                directed = TRUE, # are events directed?
                ordinal = FALSE, # 
                origin = NULL, # t_0 if exists, otherwise (when NULL) the day/second/1unit earlier that t_1 is chosen
                omit_dyad = list(default=NULL)) # list of lists where each element contains two objects `dyad` (specifying 
                                                  the dyad to remove from the riskset) `time` (vector of time points when 
                                                  to remove the dyads in `dyad`)
                
```
##### Example of `omit_dyad` list
Let's say we want to alter (reduce) the riskset following to two changes that apply on different time intervals:
1. two actors `Anne` and `Will` that couldn't interact with anybody else after a specific time point, t_s (until the last observed time point, t_M);
2. an event type `cooperation` that was no more feasible since t_r (until t_M).

An example of input list `omit_dyad` that we call here `exclude_from_riskset` is the following 
```
exclude_from_riskset <- list() # create an empty list

 # first change : Anne and Will
exclude_from_riskset[[1]] <- list(time = c(t_s, ..., t_M), dyad = see table below)
```

The `data.frame` in `dyad` for the first change will be the one below

actor1|actor2|type| 
:---:|:---:|:---:|
Anne|`<NA>`|`<NA>`|
`<NA>`|Anne|`<NA>`|
Will|`<NA>`|`<NA>`|
`<NA>`|Will|`<NA>`| 

The `data.frame` above will give instruction to the function about removing Anne and Will for all the event types and both when they are sender or receiver (the `<NA>` values mean that all the actors/types are considered). 
For the second case we do the same.
```
exclude_from_riskset[[2]] <- list(time = c(t_r, ..., t_M), dyad = see table below)
```

In this case the `data.frame` in `dyad` will be the following

actor1|actor2|type| 
:---:|:---:|:---:|
`<NA>`|`<NA>`|cooperation|

Given that we want to remove all the possible combinations of `(actor1,actor2)` having type `cooperation`, we left them unspecified.

To sum up, every time one field (actor1,actor2,type) is left unspecified, the omission from the riskset applies to all the values of that field.
