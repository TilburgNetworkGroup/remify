## tests on warning messages for remify::remify() ##


# when there is at least one self-event in the event sequence
reh_loc <- randomREH
reh_loc$edgelist$actor1[10] <- reh_loc$edgelist$actor2[10] 
expect_warning(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie"),
  "Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
  fixed=TRUE
)

# agument 'model' set to default
reh_loc <- randomREH
expect_warning(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad),
  "`model` set to `tie` by default",
  fixed = TRUE
)

# 'edgelist' contains missing data
reh_loc <- randomREH
reh_loc$edgelist[1,c(2,4)] <- c(NA,NA)
expect_warning(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie"),
  "`edgelist` contains missing data: incomplete events are dropped.",
  fixed = TRUE
)

# `time` column is not sorted
reh_loc <- randomREH
reh_loc$edgelist$time[1:100] <- reh_loc$edgelist$time[c(100:1)]
expect_warning(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "tie"),
"Warning: the `time` variable is not sorted. Sorting will be forced.",
fixed=TRUE
)

# unsorted time column when ordinal  = TRUE
## tests on ordinal = TRUE ##
reh_loc_ordinal <- randomREH$edgelist
reh_loc_ordinal$time  <- rep(1:(9915/5),each=5)[c(10:1,11:9915)]
expect_warning(remify(edgelist = reh_loc_ordinal, ordinal = TRUE, model = "tie"),
"Warning: the `time` variable is not sorted. Sorting will be forced.",
fixed=TRUE
)

# first `time` value and `origin` are the same
reh_loc <- randomREH
reh_loc$origin <- reh_loc$edgelist$time[1]
expect_warning(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "tie"),
"Warning: value supplied as `origin` is greater or equal than the first time point. `origin` is then automatically set either to one day/second/time unit before the first time point.",
fixed=TRUE
)

# one or more actors in `omit_dyad` are not found in `edgelist`
reh_loc <- randomREH
reh_loc$omit_dyad[[2]]$dyad$actor2[4] <-  as.character(rpois(1,lambda = 30)) 
expect_warning(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "tie"),
"Warning: one or more actors/types supplied in `omit_dyad` were not found in the edgelist. Therefore the corresponding dyads defined in the `omit_dyad` object were ignored.",
fixed=TRUE
)



# tests on self-loops removal - time as DatatimeVector

## weighted 
reh_loc <- randomREH
reh_loc$edgelist$actor1[1:50] <- reh_loc$edgelist$actor2[1:50]
reh_loc$edgelist$weight <- as.numeric(reh_loc$edgelist$time)**0.5 # adding a fake weight

### weighted - C>1 - tie-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### weighted - C>1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### weighted - C=1 - tie-oriented model
reh_loc$edgelist$type <- "1"
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### weighted - C=1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

## not weighted 
reh_loc <- randomREH
reh_loc$edgelist$actor1[1:50] <- reh_loc$edgelist$actor2[1:50]

### not weighted - C>1 - tie-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### not weighted - C>1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### not weighted - C=1 - tie-oriented model
reh_loc$edgelist$type <- "1"
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### not weighted - C=1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)


# tests on self-loops removal - time as Datatime

## weighted 
reh_loc <- randomREH
reh_loc$edgelist$actor1[1:50] <- reh_loc$edgelist$actor2[1:50]
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
reh_loc$edgelist$weight <- as.numeric(reh_loc$edgelist$time)**0.5 # adding a fake weight

### weighted - C>1 - tie-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### weighted - C>1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### weighted - C=1 - tie-oriented model
reh_loc$edgelist$type <- "1"
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### weighted - C=1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

## not weighted 
reh_loc <- randomREH
reh_loc$edgelist$actor1[1:50] <- reh_loc$edgelist$actor2[1:50]
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)

### not weighted - C>1 - tie-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### not weighted - C>1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### not weighted - C=1 - tie-oriented model
reh_loc$edgelist$type <- "1"
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### not weighted - C=1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)


# tests on self-loops removal - time as Numeric or Integer

## weighted 
reh_loc <- randomREH
reh_loc$edgelist$actor1[1:50] <- reh_loc$edgelist$actor2[1:50]
reh_loc$edgelist$time <- as.numeric(reh_loc$edgelist$time)
reh_loc$edgelist$weight <- as.numeric(reh_loc$edgelist$time)**0.5 # adding a fake weight

### weighted - C>1 - tie-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### weighted - C>1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### weighted - C=1 - tie-oriented model
reh_loc$edgelist$type <- "1"
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### weighted - C=1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

## not weighted 
reh_loc <- randomREH
reh_loc$edgelist$actor1[1:50] <- reh_loc$edgelist$actor2[1:50]
reh_loc$edgelist$time <- as.numeric(reh_loc$edgelist$time)

### not weighted - C>1 - tie-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### not weighted - C>1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### not weighted - C=1 - tie-oriented model
reh_loc$edgelist$type <- "1"
expect_warning(remify(edgelist = reh_loc$edgelist, model = "tie"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)

### not weighted - C=1 - actor-oriented model
expect_warning(remify(edgelist = reh_loc$edgelist, model = "actor"),
"Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.",
fixed=TRUE
)
