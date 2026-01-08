# getActorName

A function that given a vector of actor ID's returns the corresponding
vector of actor (input) names.

## Usage

``` r
getActorName(x, actorID = NULL)

# S3 method for class 'remify'
getActorName(x, actorID = NULL)
```

## Arguments

- x:

  a `remify` object.

- actorID:

  a vector of actor ID's. The ID value can range between `1` and `N`
  (number of actors in the network).

## Value

character vector of actors' names.

## Methods (by class)

- `getActorName(remify)`: return actor's name from actor's ID

## Examples

``` r
# processing the random network 'randomREH'
library(remify)
data(randomREH)
reh <- remify(edgelist = randomREH$edgelist,
              model = "tie",
              riskset = "manual",
              omit_dyad = randomREH$omit_dyad)

# find actor name from actor ID
getActorName(x = reh, actorID = c(1,2,8,12))
#> [1] "Alexander" "Andrey"    "Francesca" "Kiffani"  
```
