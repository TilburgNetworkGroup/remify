## tests on error messages ##

# when input 'edgelist' has less than three columns
expect_error(remify(edgelist = randomREH$edgelist[,1:2],
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "tie"),
"`edgelist` must be a data.frame of three columns: one column for the vector of event times, and two columns that describe the actors that form the dyadic event.",
fixed=TRUE
)


# when input 'edgelist' has no first column named 'time'
edgelist_loc <- randomREH$edgelist
names(edgelist_loc)[1] <- "zzz"
expect_silent(remify(edgelist = edgelist_loc,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "tie"))

# when input 'edgelist' has no second column named 'actor1'
edgelist_loc <- randomREH$edgelist
names(edgelist_loc)[2] <- "zzz"
expect_silent(remify(edgelist = edgelist_loc,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "tie"))

# when input 'edgelist' has no third column named 'actor2'
edgelist_loc <- randomREH$edgelist
names(edgelist_loc)[3] <- "zzz"
expect_silent(remify(edgelist = edgelist_loc,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "tie"))
                  
# when setting argument 'riskset' to "active"
expect_silent(remify(edgelist = randomREH$edgelist,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  riskset = "active",
                  model = "tie"))             


# `model` is not "actor" or "tie"
expect_error(remify(edgelist = randomREH$edgelist,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "model"),
"`model` must be set to either `tie` or `actor`.",
fixed=TRUE
)

# 'edgelist' is not a data.frame
reh_loc <- randomREH
reh_loc$edgelist <- data.matrix(reh_loc$edgelist)
expect_error(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie"),
  "`edgelist` must be of class `data.frame`.",
  fixed = TRUE
)

# argument 'model' not correctly specified
reh_loc <- randomREH
expect_error(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie-oriented"),
  "`model` must be set to either `tie` or `actor`.",
  fixed=TRUE
)


# class of 'origin' and class of 'time' column in 'edgelist' are different
reh_loc <- randomREH
reh_loc$origin <- as.Date(reh_loc$origin)
expect_error(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = NULL,
                  model = "tie"),
  "the class of `origin` and the class of `edgelist$time` must be the same.",
  fixed = TRUE
)

# input 'omit_dyad' is not a list
reh_loc <- randomREH
reh_loc$omit_dyad <- data.matrix(reh_loc$omit_dyad[[1]]$dyad)
expect_error(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie"),
  "`omit_dyad` must be a list. Check vignette(topic = 'remify', package= 'remify') for more information.",
  fixed = TRUE
)

# class of `time` in 'omit_dyad' and class of 'time' column in 'edgelist' are different (at least for one object inside omit_dyad)
reh_loc <- randomREH
reh_loc$omit_dyad[[1]]$time <- as.Date(reh_loc$omit_dyad[[1]]$time)
expect_error(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie"),
  "the class of the time specified in `omit_dyad` and the class of `edgelist$time` must be the same.",
  fixed = TRUE
)

# input 'omit_dyad' is a list but at least one of its objects is not a named list of 'dyad' and 'time'
reh_loc <- randomREH
names(reh_loc$omit_dyad[[1]]) <- c("dy","tme")
expect_error(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie"),
  "`omit_dyad` must be a collection of lists with two named objects: `dyad` and `time`. Check vignette(topic = 'remify', package= 'remify') for more information.",
  fixed = TRUE
)

# class of 'time' must be one of the followings
reh_loc <- randomREH
reh_loc$edgelist$time <- as.POSIXlt(reh_loc$edgelist$time)
expect_error(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = NULL,
                  omit_dyad = NULL,
                  model = "tie"),
  "the class of column `time` in  `edgelist` must be one of the following types: numeric, integer, Date or POSIXct",
  fixed = TRUE
) 

# errors from Rcpp functions, handled via expect_stdout(print(tryCatch(expr,error=function(e) e)),"error message",fixed=TRUE)

# time points defined in omit_dyad are removed
reh_loc <- randomREH
reh_loc$omit_dyad[[1]]$time[2] <- reh_loc$edgelist$time[9000]+60
tryCatch_error_loc <- tryCatch(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "tie"),error=function(e) e)
expect_stdout(print(tryCatch_error_loc),
"<Rcpp::exception: either start or stop in one of the elements in the list 'omit_dyad' are not found in the edgelist. Please, provide observed time points as start and stop values>"
)

# when more than two time points are supplied in any of the object inside `omit_dyad`
reh_loc <- randomREH
reh_loc$omit_dyad[[1]]$time <- c(reh_loc$omit_dyad[[1]]$time,reh_loc$edgelist$time[9000]+60)
tryCatch_error_loc <- tryCatch(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "tie"),error=function(e) e)
expect_stdout(print(tryCatch_error_loc),
"<Rcpp::exception: time vector in each element of the list 'omit_dyad' must be of length 2: start and stop time when the riskset changed>"
)

# when more than two time points are supplied in any of the object inside `omit_dyad`
reh_loc <- randomREH
reh_loc$omit_dyad[[1]]$time <- c(reh_loc$omit_dyad[[1]]$time[2],reh_loc$omit_dyad[[1]]$time[1])
tryCatch_error_loc <- tryCatch(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "tie"),error=function(e) e)
expect_stdout(print(tryCatch_error_loc),
"<Rcpp::exception: time vector in each element of the list 'omit_dyad' must be sorted so that elements indicate respectively start and stop time when the riskset changed>"
)

# warning when directed = FALSE and model = "actor"
reh_loc <- randomREH
tryCatch_error_loc <- tryCatch(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = FALSE, # events are not directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "actor"),error=function(e) e)
expect_stdout(print(tryCatch_error_loc),
"<Rcpp::exception: actor-oriented model can only work with directed networks>"
)

# when time variable as at least one negative value
reh_loc <- randomREH
reh_loc$edgelist$time <- c(-5:9909)
tryCatch_error_loc<- tryCatch(remify(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = NULL,
                    omit_dyad = NULL,
                    model = "tie"),error=function(e) e)
expect_stdout(print(tryCatch_error_loc),
"<Rcpp::exception: time variable can't be negative>"
)

# when 'ncores' is larger than it should be
reh_loc <- randomREH
expect_error(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = NULL,
                  omit_dyad = NULL,
                  model = "tie",
                  ncores = 1e05),
  "'ncores' is recommended to be set at most to: floor(parallel::detectCores()-2L)",
  fixed = TRUE
) 

# plot.remify() method
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                  model = "tie")

# when one or more actors supplied via the argument 'actors' are not found in the network
expect_error(plot(x=out,actors = c("0","1")), "one or more actors' names ('actors') are not found in the remify object 'x'.", fixed = TRUE)

## when N < 50 and the selection of 'actors' brings to zero events selected from the event sequence
out <- data.frame(time=1:30,actor1=11:40,actor2=12:41)
out <- remify(edgelist = out,
                actors = as.character(1:41),
                directed = TRUE,
                model = "tie")  
expect_error(plot(x=out,actors = as.character(1:10)),"no events found when selecting the set of actors (supplied via the argument 'actors').",fixed=TRUE)

## when N > 50 and the selection of 'actors' brings to zero events selected from the event sequence
out <- data.frame(time=1:200,actor1=61:260,actor2=62:261)
out <- remify(edgelist = out,
                actors = as.character(1:261),
                directed = TRUE,
                model = "tie")  
expect_error(plot(x=out,actors = as.character(1:60)),"no events found when selecting the set of actors (supplied via the argument 'actors').",fixed=TRUE)
