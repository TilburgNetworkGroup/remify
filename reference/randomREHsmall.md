# Random Relational Event History (small)

A subset from the randomly generated sequence of relational events
\`randomREH\`, with 5 actors and 586 events (without event types).

## Usage

``` r
randomREHsmall
```

## Format

`data(randomREHsmall)` will load a list containing following objects:

- `edgelist`:

  a `data.frame` that contains the random sequence of events. Columns of
  the edgelist are:

  `time`

  :   the timestamp indicating the time at which each event occurred;

  `actor1`

  :   the name of the actor that generated the relational event;

  `actor2`

  :   the name of the actor that received the relational event;

- `actors`:

  names of actors interacting in the dynamic network.

- `origin`:

  starting time point (`t_0`) prior to the first observed event (`t_1`),
  the class of this object must be the same as the one of the time
  column in the edgelist.

- `omit_dyad`:

  a list where each element describes an alteration of the riskset which
  takes place at specific time points and for certain actors and/or
  types.

## Examples

``` r
data(randomREHsmall)

# actors names
randomREHsmall$actors
#> [1] "Colton"    "Lexy"      "Francesca" "Richard"   "Kayla"    

# types names
randomREHsmall$types
#> NULL


# run the preprocessing function reh() by supplying the loaded objects.
small_edgelist_reh <- remify(edgelist = randomREHsmall$edgelist,
                    actors = randomREHsmall$actors,
                    directed = TRUE,
                    ordinal = FALSE,
                    origin = randomREHsmall$origin,
                    omit_dyad = randomREHsmall$omit_dyad,
                    model = "tie")

# `small_edgelist_reh` is an object of class `reh`
class(small_edgelist_reh)
#> [1] "remify"

# names of objects inside `small_edgelist_reh`
names(small_edgelist_reh)
#> [1] "M"              "N"              "C"              "D"             
#> [5] "intereventTime" "edgelist"      
```
