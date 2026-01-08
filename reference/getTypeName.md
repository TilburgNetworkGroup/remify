# getTypeName

A function that given a vector of type ID's returns the corresponding
vector of type (input) names.

## Usage

``` r
getTypeName(x, typeID = NULL)

# S3 method for class 'remify'
getTypeName(x, typeID = NULL)
```

## Arguments

- x:

  a `remify` object.

- typeID:

  a vector of type ID's. The ID value can range between `1` and `C`
  (number of event types in the network).

## Value

character vector of types' names.

## Methods (by class)

- `getTypeName(remify)`: return type's name from type's ID

## Examples

``` r
# processing the random network 'randomREH'
library(remify)
data(randomREH)
reh <- remify(edgelist = randomREH$edgelist,
              model = "tie",
              riskset = "manual",
              omit_dyad = randomREH$omit_dyad)

# find type name from type ID
getTypeName(x = reh, typeID = c(1,3))
#> [1] "competition" "cooperation"
```
