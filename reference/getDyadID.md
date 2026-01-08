# getDyadID

A function that given a vector of names as to actor1, actor2 and type
returns the corresponding dyad ID. The names to supply are the original
input names of the edgelist before the processing via the function
[`remify::remify()`](https://tilburgnetworkgroup.github.io/remify/reference/remify.md).

## Usage

``` r
getDyadID(x, actor1, actor2, type)

# S3 method for class 'remify'
getDyadID(x, actor1, actor2, type)
```

## Arguments

- x:

  a `remify` object.

- actor1:

  \[character\] name of actor1.

- actor2:

  \[character\] name of actor2.

- type:

  \[character\] name of type.

## Value

dyad ID as integer value.

## Methods (by class)

- `getDyadID(remify)`: return dyad's ID from dyad's composition

## Examples

``` r
# processing the random network 'randomREH'
library(remify)
data(randomREH)
reh <- remify(edgelist = randomREH$edgelist,
              model = "tie",
              riskset = "manual",
              omit_dyad = randomREH$omit_dyad)

# find dyad ID from dyad composition (names of actor1, actor2 and type)
getDyadID(x = reh, actor1 = "Francesca", actor2 = "Kayla", type = "conflict")
#> [1] 522
```
