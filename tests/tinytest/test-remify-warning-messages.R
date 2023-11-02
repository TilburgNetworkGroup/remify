## tests on warning messages ##


# this commented test is temporarily not working
# when there is at least one self-event in the event sequence
#reh_loc <- randomREH
#reh_loc$edgelist$actor1[10] <- reh_loc$edgelist$actor2[10] 
#expect_stdout(
#  remify(edgelist = reh_loc$edgelist,
#                  actors = reh_loc$actors,
#                  types = reh_loc$types, 
#                  directed = TRUE, # events are directed
#                  ordinal = FALSE, # REM with waiting times
#                  origin = reh_loc$origin,
#                  omit_dyad = reh_loc$omit_dyad,
#                  model = "tie"),
#  "Warning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing."
#)

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
expect_stdout(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "tie"),
"Warning: the `time` variable is not sorted. Sorting will be forced."
)

# first `time` value and `origin` are the same
reh_loc <- randomREH
reh_loc$origin <- reh_loc$edgelist$time[1]
expect_stdout(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "tie"),
"Warning: value supplied as `origin` is greater or equal than the first time point. `origin` is then automatically set either to one day/second before the first time point or to 0."
)

# one or more actors in `omit_dyad` are not found in `edgelist`
reh_loc <- randomREH
reh_loc$omit_dyad[[2]]$dyad$actor2[4] <-  as.character(rpois(1,lambda = 30)) 
expect_stdout(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "tie"),
"Warning: one or more actors/types supplied in `omit_dyad` were not found in the edgelist. Therefore the corresponding dyads defined in the `omit_dyad` object were ignored."
)

# plot.remify() method warning

## N > 50
out <- data.frame(time=1:100,actor1=1:100,actor2=2:101)
out <- remify(edgelist = out,
                directed = TRUE,
                model = "tie")  
expect_warning(plot(out),"Too many actors for rendering plots with a good quality: the 50 most active actors are selected (descriptives on dyads and actors may differ from the descriptives conducted on the whole set of actors)",fixed=TRUE)

## on a subset of actors but still larger than 50 actors
expect_warning(plot(out,actors=as.character(1:80)),"Too many actors for rendering plots with a good quality: the 50 most active actors are selected (descriptives on dyads and actors may differ from the descriptives conducted on the whole set of actors)",fixed=TRUE)

## for directed = FALSE
out <- data.frame(time=1:100,actor1=1:100,actor2=2:101)
out <- remify(edgelist = out,
                directed = FALSE,
                model = "tie")  
expect_warning(plot(out),"Too many actors for rendering plots with a good quality: the 50 most active actors are selected (descriptives on dyads and actors may differ from the descriptives conducted on the whole set of actors)",fixed=TRUE)

