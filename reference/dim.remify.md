# dim.remify

A function that returns the dimension of the temporal network.

## Usage

``` r
# S3 method for class 'remify'
dim(x)
```

## Arguments

- x:

  a `remify` object.

## Value

vector of dimensions of the processed event sequence.

## Examples

``` r
# processing the random network 'randomREHsmall'
library(remify)
data(randomREHsmall)
reh <- remify(edgelist = randomREHsmall$edgelist,
              model = "tie")

# dimensions of the processed 'remify' object
dim(reh)
#> events actors  dyads 
#>    586      5     20 
```
