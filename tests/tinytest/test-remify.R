## tests on function remify::remify() - tie-oriented modeling ##
out <- remify(edgelist = randomREH$edgelist,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "tie")

# expectations on output object features               
expect_inherits(out, "remify")
expect_true(is.list(out))
expect_equal(length(out), 7)

# expectations on objects inside the 'remify' object
expect_identical(names(out),c("M","N","C","D","intereventTime","edgelist","omit_dyad"))
expect_equal(out$M, dim(randomREH$edgelist)[1])
expect_equal(out$edgelist[,1],randomREH$edgelist[,1])

# expectations on attributes of the 'remify' object 
expect_identical(names(attributes(out)),c("names","class","with_type","weighted","directed","ordinal","model","riskset","dictionary","origin","ncores","dyadID","actor1ID","actor2ID","typeID"))
expect_false(attr(out,"ordinal")) 
expect_true(attr(out,"directed"))
expect_identical(attr(out,"model"),"tie")

# if the input `edgelist`` does not have a type column
reh_loc <- randomREH
reh_loc$edgelist$type <- NULL
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = NULL, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_null(out$C)
expect_null(out$edgelist$type)
expect_false(attr(out,"with_type"))

# if the input `edgelist` has a type column
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = NULL, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_equal(out$C,3)
expect_true(attr(out,"with_type"))

# if the input `edgelist`` does not have a weight column
reh_loc <- randomREH
reh_loc$edgelist$weight <- NULL
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_null(out$edgelist$weight)
expect_false(attr(out,"weighted"))

# if the input `edgelist`` has a weight column
reh_loc <- randomREH
reh_loc$edgelist$weight <- rnorm(n=dim(reh_loc$edgelist)[1])
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_equal(out$edgelist$weight,reh_loc$edgelist$weight)
expect_true(attr(out,"weighted"))

# processing the input `omit_dyad` when the network is undirected
reh_loc <- randomREH
reh_loc$omit_dyad[[2]]$dyad <- rbind(reh_loc$omit_dyad[[2]]$dyad,c("Megan","Zackary",NA))
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = FALSE, # events are not directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_true(!is.null(out$omit_dyad))

# if the input `omit_dyad` contains time intervals of the type [start = NA, stop = x] or [start = x, stop = NA]
reh_loc <- randomREH
reh_loc$omit_dyad[[1]]$time[1] <- NA
reh_loc$omit_dyad[[2]]$time[2] <- NA
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_true(!is.null(out$omit_dyad))

## creating a new omit_dyad object to test more complex overlapping of time intervals
# [ ... code here ... ]


### here the new test
# if the input `omit_dyad`` object contains dyads specified in other ways that the default inside randomREH object
reh_loc <- randomREH
reh_loc$omit_dyad[[2]]$dyad <- data.frame(actor1=c("Megan","Richard",NA,"Derek"),actor2=c("Zackary",NA,"Crystal","Lexy"),type=c("conflict","conflict","conflict",NA))

## tested for the tie-oriented framework
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
expect_true(!is.null(out$omit_dyad))

## tested for the actor-oriented framework
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "actor")
expect_true(!is.null(out$omit_dyad))

# test omit_dyad object with sequence with simultaneous events
reh_loc <- randomREH
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
reh_loc$origin <- as.Date(reh_loc$origin)-1
reh_loc$omit_dyad[[1]]$time <- as.Date(reh_loc$omit_dyad[[1]]$time)
reh_loc$omit_dyad[[2]]$time <- as.Date(reh_loc$omit_dyad[[1]]$time)
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")


# `time` in 'omit_dyad' defined as c(NA,NA)
reh_loc <- randomREH
reh_loc$omit_dyad[[1]]$time <- c(NA,NA)
expect_silent(
  remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie"))

# test on unsorted time variable (Rcpp level) 
reh_loc <- randomREH
reh_loc$edgelist$time <- reh_loc$edgelist$time[sample(1:dim(reh_loc$edgelist)[1],size=dim(reh_loc$edgelist)[1],replace=FALSE)]
out <- remify(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = NULL,
                  omit_dyad = NULL,
                  model = "tie")
expect_equal(sort(reh_loc$edgelist$time),out$edgelist[,1])


# test on argument 'ncores' argument
expect_silent(remify(edgelist = randomREH$edgelist,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "tie",
                  ncores = NULL))

# check snapshots [[At the moment tonly testing that the expectation of an stdout - should compare with a snapshot defined in pattern]]

# test (1) on method print()"
reh_loc <- randomREH
expect_stdout(
remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie") )



# test (1) on method summary()
reh_loc <- randomREH
expect_stdout(
summary(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie")))

# test (1) on method print.remify()
reh_loc <- randomREH
expect_stdout(
print(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie")))


# test (2) on method summary()
## without input 'origin'
reh_loc <- randomREH
expect_stdout(
summary(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = NULL,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie"))
)



# test (3) on method summary()"
## time input set to Date
reh_loc <- randomREH
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
reh_loc$origin <- as.Date(reh_loc$origin)-1
reh_loc$omit_dyad <- NULL
expect_stdout(
summary(remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie"))
)



## tests on function remify::remify() - actor-oriented modeling ##
out <- remify(edgelist = randomREH$edgelist,
                  actors = randomREH$actors,
                  types = randomREH$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = randomREH$origin,
                  omit_dyad = randomREH$omit_dyad,
                  model = "actor")

# expectations on output object features               
expect_inherits(out, "remify")
expect_true(is.list(out))
expect_equal(length(out), 7)

# expectations on objects inside the 'remify' object
expect_identical(names(out),c("M","N","C","D","intereventTime","edgelist","omit_dyad"))
expect_equal(out$M, dim(randomREH$edgelist)[1])
expect_equal(out$edgelist[,1],randomREH$edgelist[,1])

# expectations on attributes of the 'remify' object 
expect_identical(names(attributes(out)),c("names","class","with_type","weighted","directed","ordinal","model","riskset","dictionary","origin","ncores","actor1ID","actor2ID","typeID"))
expect_false(attr(out,"ordinal")) 
expect_true(attr(out,"directed"))
expect_identical(attr(out,"model"),"actor")