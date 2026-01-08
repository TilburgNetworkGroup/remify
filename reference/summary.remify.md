# summary.remify

A function that returns a easy-to-read summary of the main
characteristics as to the processed relational event sequence.

## Usage

``` r
# S3 method for class 'remify'
summary(object, ...)
```

## Arguments

- object:

  a `remify` object.

- ...:

  other arguments.

## Value

prints out the main characteristics of the processed relational event
sequence.

## Examples

``` r
# processing the random network 'randomREHsmall'
library(remify)
data(randomREHsmall)
reh <- remify(edgelist = randomREHsmall$edgelist,
              model = "tie")

# printing a summary of the processed 'remify' object
summary(reh)
#> Relational Event Network
#> (processed for tie-oriented modeling):
#>  > events = 586
#>  > actors = 5
#>  > riskset = full
#>  > directed = TRUE
#>  > ordinal = FALSE
#>  > weighted = FALSE
#>  > time length ~ 79 days
#>  > interevent time 
#>       >> minimum ~ 1 seconds
#>       >> maximum ~ 85906.6565 seconds
```
