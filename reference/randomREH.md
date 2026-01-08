# Random Relational Event History

A randomly generated sequence of relational events with 20 actors and
9915 events. Each event type is associated to one of the three following
sentiments: *conflict*, *competition* and *cooperation*.

## Usage

``` r
randomREH
```

## Format

`data(randomREH)` will load a list containing following objects:

- `edgelist`:

  a `data.frame` that contains the random sequence of events. Columns of
  the edgelist are:

  `time`

  :   the timestamp indicating the time at which each event occurred;

  `actor1`

  :   the name of the actor that generated the relational event;

  `actor2`

  :   the name of the actor that received the relational event;

  `type`

  :   the type of the relational event.

- `actors`:

  names of actors interacting in the dynamic network.

- `types`:

  names of event types observed in the network and describing the
  sentiment of the interaction (*conflict*, *competition* and
  *cooperation*).

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
data(randomREH)

# actors names
randomREH$actors
#>  [1] "Crystal"   "Colton"    "Lexy"      "Kelsey"    "Michaela"  "Zackary"  
#>  [7] "Richard"   "Maya"      "Wyatt"     "Kiffani"   "Alexander" "Kayla"    
#> [13] "Derek"     "Justin"    "Andrey"    "Francesca" "Megan"     "Mckenna"  
#> [19] "Charles"   "Breanna"  

# types names
randomREH$types
#> [1] "conflict"    "competition" "cooperation"

# looking into the first modification of the riskset: omit_dyad[[1]]
## the data.frame `dyad` specifies which dyads will be omitted from the riskset 
## (all the dyads that expressed a `conflict` between actor won't be part of the riskset):
randomREH$omit_dyad[[1]]$dyad 
#>   actor1 actor2     type
#> 1     NA     NA conflict

## the vector `time` specifies the time points when this exclusion takes place 
head(randomREH$omit_dyad[[1]]$time) # (printing out only the first 10 time points)
#> [1] "2020-05-07 20:42:38 UTC" "2020-05-23 21:46:41 UTC"

# run the preprocessing function reh() by supplying the loaded objects.
edgelist_reh <- remify(edgelist = randomREH$edgelist,
                    actors = randomREH$actors,
                    types = randomREH$types, 
                    directed = TRUE,
                    ordinal = FALSE,
                    origin = randomREH$origin,
                    omit_dyad = randomREH$omit_dyad,
                    model = "tie")

# `edgelist_reh` is an object of class `reh`
class(edgelist_reh)
#> [1] "remify"

# names of objects inside `edgelist_reh`
names(edgelist_reh)
#> [1] "M"              "N"              "C"              "D"             
#> [5] "intereventTime" "edgelist"       "omit_dyad"     
```
