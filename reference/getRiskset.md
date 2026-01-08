# getRiskset

This function returns the processed risk set changes specified by the
input \`omit_dyad\`. In such a matrix: value 1 refers to the dyads in
the risk set, and 0 otherwise (dyads excluded from the risk set). All
the possible risk set modifications are described by row, and the
columns identify the dyads. Note: This matrix is the output given by
processing the input \`omit_dyad\`, and the number of rows might be
equal to or higher than the number of objects in \`omit_dyad\`. This
might happen because more than one modification of the risk set defined
in the input could overlap over time with others. For more details about
how the risk set is processed, see
[`vignette(package="remify",topic="riskset")`](https://tilburgnetworkgroup.github.io/remify/articles/riskset.md).

## Usage

``` r
getRiskset(x)

# S3 method for class 'remify'
getRiskset(x)
```

## Arguments

- x:

  a `remify` object.

## Value

list of objects describing the processed the risk set.

## Methods (by class)

- `getRiskset(remify)`: manual riskset object

## Examples

``` r
# processing the random network 'randomREH'
library(remify)
data(randomREH)
reh <- remify(edgelist = randomREH$edgelist,
              model = "tie",
              riskset = "manual",
              omit_dyad = randomREH$omit_dyad)

# structure of the processed risk set
str(getRiskset(reh))
#> List of 1
#>  $ riskset: int [1:2, 1:1140] 1 1 1 1 1 1 1 1 1 1 ...
```
