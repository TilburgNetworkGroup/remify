# getTypeID

A function that given a vector of type names returns the corresponding
vector of ID's.

## Usage

``` r
getTypeID(x, typeName = NULL)

# S3 method for class 'remify'
getTypeID(x, typeName = NULL)
```

## Arguments

- x:

  a `remify` object.

- typeName:

  a vector of type names. The same names in the input edgelist.

## Value

type ID as integer value.

## Methods (by class)

- `getTypeID(remify)`: return type's ID from type's name

## Examples

``` r
# processing the random network 'randomREH'
library(remify)
data(randomREH)
reh <- remify(edgelist = randomREH$edgelist,
              model = "tie",
              riskset = "manual",
              omit_dyad = randomREH$omit_dyad)

# find type ID from the type name
getTypeID(x = reh, typeName = c("conflict","cooperation"))
#> [1] 2 3
```
