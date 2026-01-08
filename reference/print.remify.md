# print.remify

print a summary of the event history.

## Usage

``` r
# S3 method for class 'remify'
print(x, ...)
```

## Arguments

- x:

  a `remify` object.

- ...:

  further arguments.

## Value

displays the same information provided by the summary method.

## Examples

``` r
 
# processing the random network 'randomREHsmall'
library(remify)
data(randomREHsmall)
reh <- remify(edgelist = randomREHsmall$edgelist,
              model = "tie")

# printing a summary of the processed 'remify' object
print(reh)
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
