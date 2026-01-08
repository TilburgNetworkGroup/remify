# Transform processed remify objects to different formats

A function that transforms a `remify` object into one of the possible
formats that suit external packages. The function can convert, at the
moment, the data structure from an object of class `remify` to a data
structure required by the function `relevent::rem()` or by the function
`relevent::rem.dyad()` from the
'[relevent](https://CRAN.R-project.org/package=relevent)' package
(Butts, C.T. 2023).

## Usage

``` r
rehshape(
  data,
  output_format = c("relevent-rem", "relevent-rem.dyad"),
  ncores = 1L,
  optional_arguments = NULL
)
```

## Arguments

- data:

  an object of class 'remify' (see function
  [`remify::remify()`](https://tilburgnetworkgroup.github.io/remify/reference/remify.md)).

- output_format:

  a character indicating the output format which the input data has to
  be converted to. It can assume two values: `"relevent-rem"` ,
  `"relevent-rem.dyad"`. Default value is `"relevent-rem"`.

- ncores:

  number of cores used to parallelize internal algorithms

- optional_arguments:

  vector of arguments names from relevent::rem or relevent::rem.dyad()
  that the user might want to process and have in the output object of
  rehshape (e.g., the pre-computed structures required by
  relevent::rem.dyad)

## Value

an object of class specified in the `output_format` argument. The output
class object 'relevent-rem' contains a list of objects named after the
arguments of the function `relevent::rem()`: 'eventlist' (mandatory),
'supplist' (optional), 'timing'(mandatory). The output class object
'relevent-rem.dyad' contains a list of objects named after the arguments
of the function `relevent::rem.dyad()`: 'edgelist' (mandatory), 'n'
(mandatory), 'ordinal'(optional).

## Examples

``` r
# processing the random network 'randomREH'
library(remify)
data(randomREH)
reh <- remify(edgelist = randomREH$edgelist,
              model = "tie",
              riskset = "manual",
              omit_dyad = randomREH$omit_dyad)

# convert 'remify' object to output_format = "relevent-rem"
relevent_rem_obj <- rehshape(data = reh, output_format = "relevent-rem")

str(relevent_rem_obj) 
#> List of 3
#>  $ eventlist:'data.frame':   9915 obs. of  2 variables:
#>   ..$ dyad: num [1:9915] 182 464 963 4 733 ...
#>   ..$ time: POSIXct[1:9915], format: "2020-03-05 01:47:08" "2020-03-05 01:50:18" ...
#>  $ supplist : logi [1:9915, 1:1140] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  $ timing   : chr "interval"
#>  - attr(*, "class")= chr "relevent-rem"

# convert 'remify' object to output_format = "relevent-rem.dyad"
relevent_rem.dyad_obj <- rehshape(data = reh, output_format = "relevent-rem.dyad")

summary(relevent_rem.dyad_obj)
#>          Length Class      Mode   
#> edgelist 3      data.frame list   
#> n        1      -none-     numeric
#> ordinal  1      -none-     logical
```
