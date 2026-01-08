# Process a Relational Event History

A function that processes raw relational event history data and returns
a S3 object of class 'remify' which is used as input in other functions
inside 'remverse'.

## Usage

``` r
remify(
  edgelist,
  directed = TRUE,
  ordinal = FALSE,
  model = c("tie", "actor"),
  actors = NULL,
  types = NULL,
  riskset = c("full", "active", "manual"),
  origin = NULL,
  omit_dyad = NULL,
  ncores = 1L
)
```

## Arguments

- edgelist:

  the relational event history. An object of class
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) with first
  three columns corresponding to time, and actors forming the dyad. The
  first three columns will be re-named "time", "actor1", "actor2"
  (where, for directed networks, "actor1" corresponds to the sender and
  "actor2" to the receiver of the relational event). Optional columns
  that can be supplied are: \`type\` and \`weight\`. If one or both
  exist in `edgelist`, they have to be named accordingly.

- directed:

  logical value indicating whether events are directed (`TRUE`) or
  undirected (`FALSE`). (default value is `TRUE`)

- ordinal:

  logical value indicating whether only the order of events matters in
  the model (`TRUE`) or also the waiting time must be considered in the
  model (`FALSE`). (default value is `FALSE`)

- model:

  can be "tie" or "actor" oriented modeling. This argument plays a
  fundamental role when `omit_dyad` is supplied. Indeed, when
  actor-oriented modeling, the dynamic risk set will consist of two risk
  sets objects (senders' and dyads' risk sets). In the tie-oriented
  model the function will return a dynamic risk set referred at a
  dyad-level.

- actors:

  \[*optional*\] character vector of actors' names that may be observed
  interacting in the network. If `NULL` (default), actors' names will be
  taken from the input edgelist.

- types:

  \[*optional*\] character vector of event types that may occur in the
  network. If `NULL` (default), types' names will be taken from the
  input edgelist.

- riskset:

  \[*optional*\] character value indicating the type of risk set to
  process: `riskset = "full"` (default) consists of all the possible
  dyadic events given the number of actors (and the number of event
  types) and it mantains the same structure over time.
  `riskset = "active"` considers at risk only the observed dyads and it
  mantains the same structure over time. `riskset = "manual"`, allows
  the risk set to have a structure that is user-defined, and it is based
  on the instructions supplied via the argument `omit_dyad`. This type
  of risk set allows for time-varying risk set, in which, for instance,
  subset of actors can interact only at specific time windows, or events
  of a specific type (sentiment) can't be observed within time intervals
  that are defined by the user.

- origin:

  \[*optional*\] starting time point of the observaton period (default
  is `NULL`). If it is supplied, it must have the same class of the
  \`time\` column in the input `edgelist`.

- omit_dyad:

  \[*optional*\] list of lists. Each list refers to one risk set
  modification and must have two objects: a first object named \`time\`,
  that is a vector of two values defining the first and last time point
  of the time window where to apply the change to the risk set and a
  second object, named \`dyad\`, which is a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html) where dyads to
  be removed are supplied in the format `actor1,actor2,type` (by row).
  The `NA` value can be used to remove multiple objects from the risk
  set at once with one risk set modification list (see Details).

- ncores:

  \[*optional*\] number of cores used in the parallelization of the
  processing functions. (default is `1`).

## Value

'remify' S3 object, list of: number of events (\`M\`), number of actors
(\`N\`), number of event types (if present, \`C\`), number of dyads
(\`D\`, and also \`activeD\` if \`riskset="active"\`), vector of
inter-event times (waiting times between two subsequent events),
processed input edgelist as \`data.frame\`, processed \`omit_dyad\`
object as \`list\`. The function returns also several attributes that
make efficient the processing of the data for future analysis. For more
details about the function, input arguments, output, attributes and
methods, please read
[`vignette(package="remify",topic="remify")`](https://tilburgnetworkgroup.github.io/remify/articles/remify.md).

## Details

In `omit_dyad`, the `NA` value can be used to remove multiple objects
from the risk set at once with one risk set modification list. For
example, to remove all events with sender equal to actor “A” add a list
with two objects `time = c(NA, NA)` and
`dyad = data.frame(actor1 = A, actor2 = NA, type = NA)` to the
`omit_dyad` list. For more details about

## Examples

``` r
# load package and random network 'randomREH'
library(remify)
data(randomREH)

# first events in the sequence
head(randomREH$edgelist)
#>                  time    actor1  actor2        type
#> 1 2020-03-05 01:47:08     Kayla Kiffani competition
#> 2 2020-03-05 01:50:18    Colton  Justin    conflict
#> 3 2020-03-05 02:30:26    Kelsey    Maya cooperation
#> 4 2020-03-05 02:38:50 Alexander  Colton competition
#> 5 2020-03-05 02:56:16     Wyatt  Kelsey    conflict
#> 6 2020-03-05 03:06:45     Derek Breanna competition

# actor's names
randomREH$actors
#>  [1] "Crystal"   "Colton"    "Lexy"      "Kelsey"    "Michaela"  "Zackary"  
#>  [7] "Richard"   "Maya"      "Wyatt"     "Kiffani"   "Alexander" "Kayla"    
#> [13] "Derek"     "Justin"    "Andrey"    "Francesca" "Megan"     "Mckenna"  
#> [19] "Charles"   "Breanna"  

# event type's names
randomREH$types
#> [1] "conflict"    "competition" "cooperation"

# start time of the study (origin)
randomREH$origin
#> [1] "2020-03-05 01:32:53 UTC"

# list of changes of the risk set: each one is a list of:
# 'time' (indicating the time window where to apply the risk set reduction)
# 'dyad' (a data.frame describing the dyads to remove from the risk set 
# during the time window specified in 'time')
str(randomREH$omit_dyad)
#> List of 2
#>  $ :List of 2
#>   ..$ time: POSIXct[1:2], format: "2020-05-07 20:42:38" "2020-05-23 21:46:41"
#>   ..$ dyad:'data.frame': 1 obs. of  3 variables:
#>   .. ..$ actor1: logi NA
#>   .. ..$ actor2: logi NA
#>   .. ..$ type  : chr "conflict"
#>  $ :List of 2
#>   ..$ time: POSIXct[1:2], format: "2020-05-19 23:30:09" "2020-05-23 21:46:41"
#>   ..$ dyad:'data.frame': 4 obs. of  3 variables:
#>   .. ..$ actor1: chr [1:4] "Michaela" NA "Zackary" NA
#>   .. ..$ actor2: chr [1:4] NA "Michaela" NA "Zackary"
#>   .. ..$ type  : logi [1:4] NA NA NA NA

# -------------------------------------- #
#  processing for tie-oriented modeling  #
# -------------------------------------- #

tie_randomREH <- remify(edgelist = randomREH$edgelist,
       directed = TRUE,
       ordinal = FALSE,
       model = "tie",
       actors = randomREH$actors,
       types = randomREH$types,
       riskset = "manual",
       origin = randomREH$origin,
       omit_dyad = randomREH$omit_dyad)

# summary
summary(tie_randomREH)
#> Relational Event Network
#> (processed for tie-oriented modeling):
#>  > events = 9915
#>  > actors = 20
#>  > (event) types = 3
#>  > riskset = manual
#>  > directed = TRUE
#>  > ordinal = FALSE
#>  > weighted = FALSE
#>  > time length ~ 80 days
#>  > interevent time 
#>       >> minimum ~ 0.0011 seconds
#>       >> maximum ~ 5811.4011 seconds

# dimensions of the processed network
dim(tie_randomREH)
#> events actors  types  dyads 
#>   9915     20      3   1140 

# Which ID is assigned to the actors with names "Francesca" and "Kayla"?
getActorID(x = tie_randomREH, actorName = c("Francesca","Kayla"))
#> [1]  8 10

# Which ID is assigned to the event type "conflict"?
getTypeID(x = tie_randomREH, typeName = "conflict")
#> [1] 2

# Find dyad composition (names of actor1, actor2 and type) from the dyad ID: c(1,380,760,1140)
getDyad(x = tie_randomREH, dyadID = c(1,380,760,1140))
#>   dyadID    actor1 actor2        type
#> 1      1 Alexander Andrey competition
#> 2    380   Zackary  Wyatt competition
#> 3    760   Zackary  Wyatt    conflict
#> 4   1140   Zackary  Wyatt cooperation

# visualize descriptive measures of relational event data
# plot(x = tie_randomREH)

# -------------------------------------- #
# processing for actor-oriented modeling #
# -------------------------------------- #

# loading network 'randomREHsmall'
data(randomREHsmall)

# processing small random network
actor_randomREH <- remify(edgelist = randomREHsmall$edgelist,
       directed = TRUE,
       ordinal = FALSE,
       model = "actor",
       actors = randomREHsmall$actors,
       origin = randomREHsmall$origin)
       
# summary
summary(actor_randomREH)
#> Relational Event Network
#> (processed for actor-oriented modeling):
#>  > events = 586
#>  > actors = 5
#>  > riskset = full
#>  > directed = TRUE
#>  > ordinal = FALSE
#>  > weighted = FALSE
#>  > time length ~ 80 days
#>  > interevent time 
#>       >> minimum ~ 42.1258 seconds
#>       >> maximum ~ 85906.6565 seconds

# dimensions of the processed network
dim(actor_randomREH)
#> events actors  dyads 
#>    586      5     20 

# ------------------------------------ #
# for more information about remify()  #
# check: vignette(package="remify")    #
# ------------------------------------ #
 
```
