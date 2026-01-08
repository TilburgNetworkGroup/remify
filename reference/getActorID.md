# getActorID

A function that given a vector of actor names returns the corresponding
vector of ID's.

## Usage

``` r
getActorID(x, actorName = NULL)

# S3 method for class 'remify'
getActorID(x, actorName = NULL)
```

## Arguments

- x:

  a `remify` object.

- actorName:

  a vector of actor names. The same names in the input edgelist.

## Value

actor ID as integer value.

## Methods (by class)

- `getActorID(remify)`: return actor's ID from actor's name

## Examples

``` r
# processing the random network 'randomREH'
library(remify)
data(randomREH)
reh <- remify(edgelist = randomREH$edgelist,
              model = "tie",
              riskset = "manual",
              omit_dyad = randomREH$omit_dyad)

# find actor ID from the actor name
getActorID(x = reh, actorName = c("Francesca","Kayla"))
#> [1]  8 10
```
