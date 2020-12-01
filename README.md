# remify (Version: 1.1.0)

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
Short tutorial about supplying arguments to the function `remify::reh()` and obtaining the preprocessed data as output (note: the initial check of inputs is not yet part of the function as well as attributes and methods are not yet implemented in this version of the package).

The edgelist must be a `data.frame` with colum names `[time,actor1,actor2,type,weight]` (Note: it is not necessary to supply a type or a weight column but if they are present in the edgelist they must be named `type` and `weight`, whereas it is compulsory to supply `[time,actor1,actor2]`).
```
load(file.choose()) # load edgelist object
```

Before running the function `remify::reh()`, we briefly go through the input arguments:

- `edgelist` = edgelist to process;
- `covariates` = covariates are not actually processed in this function, they will be processed by `remstats`. This argument will be removed with the next update;
- `actors` = vector of actor names (if left unspecified it will find them from the `edgelist`);
- `types`= vector of type names (if left unspecified it will find them from the `edgelist`);
- `directed` = logical TRUE/FALSE, are events directed? (if FALSE, dyads `[actor1,actor2]` will be sorted according to their names;
- `ordinal` = logical TRUE/FALSE, does only the order of event matter? if TRUE, then the ordinal likelihood (Cox model with proportional hazards) will be used;
- `origin` = t_0 if exists, otherwise (when NULL) the day/second/1unit earlier that t_1 is chosen;
- `omit_dyad`: list of lists where each element contains two objects: `dyad` (specifying the dyad to remove from the riskset) and `time` (vector of time points when to remove the dyads in `dyad`). For instance, we want to alter (reduce) the riskset according to two changes that apply on different time intervals:
1. two actors `Anne` and `Will` that couldn't interact with anybody else after a specific time point, t_s (until the last observed time point, t_M);
2. an event type `cooperation` that was no more feasible since t_r (until t_M).

An example of input list `omit_dyad` that we call here `exclude_from_riskset` is the following 
```
exclude_from_riskset <- list() # create an empty list

# first change : Anne and Will
exclude_from_riskset[[1]] <- list(time = c(t_s, ..., t_M), dyad = see table below)
```

The `data.frame` in `dyad` for the change in 1. will be the one below

actor1|actor2|type| 
:---:|:---:|:---:|
Anne|`<NA>`|`<NA>`|
`<NA>`|Anne|`<NA>`|
Will|`<NA>`|`<NA>`|
`<NA>`|Will|`<NA>`| 

The `data.frame` above will give instruction such that it will make the function remove all the events where Anne and Will are senders or receivers (the `<NA>` values mean that all the actors/types are considered). 

For the change in 2. the inpuy will be the following
```
# second change : cooperation
exclude_from_riskset[[2]] <- list(time = c(t_r, ..., t_M), dyad = see table below)
```

In this case the `data.frame` in `dyad` will be

actor1|actor2|type| 
:---:|:---:|:---:|
`<NA>`|`<NA>`|cooperation|

Given that we need to remove all the possible combinations of `(actor1,actor2)` having type `cooperation`, we left both `actor1` and `actor2` unspecified. To sum up, every time one field (`actor1`,`actor2`,`type`) is left undefined, the omission from the riskset applies to all the values of that field.

Finally, the function is run
```
reh_processed <- remify::reh(edgelist = edgelist,
                covariates = list(default = NULL), 
                actors = NULL, 
                types = NULL,  
                directed = TRUE, 
                ordinal = FALSE, 
                origin = NULL, 
                omit_dyad = list(default=NULL)) 
                
```
