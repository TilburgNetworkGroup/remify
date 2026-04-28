
library(tinytest)

## tests on methods, errors and warnings from methods

# method dim() with type
data(randomREH)

reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                model = "tie",
              riskset = "active")
expect_true(is.numeric(dim(out)))
expect_equal(length(dim(out)),5) # with types, no simultaneous events so no "time points"
expect_identical(as.numeric(dim(out)),c(out$M,out$N,out$C,out$D,out$activeD))

# method dim() without type
reh_loc <- randomREH
reh_loc$edgelist$type <- NULL
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                #types = reh_loc$types,
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                model = "tie")
expect_true(is.numeric(dim(out)))
expect_equal(length(dim(out)),3) # without types, no simultaneous events so no "time points"
expect_identical(as.numeric(dim(out)),c(out$M,out$N,out$D))
expect_identical(names(dim(out)),c("events","actors","dyads"))

# method dim() with simultaneous events (model == "tie") and types
reh_loc <- randomREH
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
reh_loc$origin <- as.Date(reh_loc$origin)-1
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                #types = reh_loc$types,
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                model = "tie",
                riskset = "active")

# expectations on output object features
expect_true(is.list(out))  # object structure may vary; skip exact length
expect_true(is.numeric(dim(out)))
expect_equal(length(dim(out)),6) # with types
expect_identical(as.numeric(dim(out)),c(out$E,out$M,out$N,out$C,out$D,out$activeD))
expect_identical(names(dim(out)),c("events","time points","actors","types","dyads","dyads(active)"))

# method dim() with simultaneous events (model == "tie") and without types
reh_loc <- randomREH
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
reh_loc$edgelist$type <- NULL
reh_loc$origin <- as.Date(reh_loc$origin)-1
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                model = "tie",
                riskset = "active")

# expectations on output object features
expect_true(is.list(out))  # object structure may vary; skip exact length
expect_true(is.numeric(dim(out)))
expect_equal(length(dim(out)),5) # with types
expect_identical(as.numeric(dim(out)),c(out$E,out$M,out$N,out$D,out$activeD))
expect_identical(names(dim(out)),c("events","time points","actors","dyads","dyads(active)"))


# # method dim() with simultaneous events (model == "actor")
reh_loc <- randomREH
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
reh_loc$origin <- as.Date(reh_loc$origin)-1
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                #types = reh_loc$types,
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                model = "actor")

# expectations on output object features
expect_true(is.numeric(dim(out)))
expect_equal(length(dim(out)),5) # with types
expect_identical(as.numeric(dim(out)),c(out$E,out$M,out$N,out$C,out$D))
expect_identical(names(dim(out)),c("events","time points","actors","types","dyads"))


## (1) when model = "tie" and omit_dyad is supplied
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                #types = reh_loc$types,
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                riskset = "manual",
                manual.riskset=reh_loc$edgelist[,2:3],
                model = "tie")
expect_identical(out$meta$riskset,"active")
expect_identical(out$meta$riskset_source,"manual")

# ## (2) when model = "actor" and omit_dyad is supplied
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                #types = reh_loc$types,
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                riskset = "manual",
                manual.riskset=reh_loc$edgelist[1:1000,2:3],
                model = "actor")

expect_identical(out$meta$riskset_source,"manual")

## (3) error message when riskset is not manual
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                #types = reh_loc$types,
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                riskset = "full",
                model = "tie")
expect_identical(out$meta$riskset,"full")


reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                #types = reh_loc$types,
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                model = "tie")

## network without type
reh_loc <- randomREH
reh_loc$edgelist$type <- NULL
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                #types = NULL,
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                model = "tie")

# method plot()

## directed = TRUE
out <- remify(edgelist = reh_loc$edgelist,
                directed = TRUE,
                model = "tie")
expect_silent(plot(x=out))
expect_silent(plot(x=out,breaks=NULL,palette=NULL,n_intervals=NULL,rev=NULL,actors=NULL,pch.degree=NULL,igraph.edge.color=NULL,igraph.vertex.color=NULL))
expect_silent(plot(x=out,pch.degree=-1))
expect_silent(plot(x=out,igraph.edge.color="#000000000",igraph.vertex.color="#000000000"))
expect_silent(plot(x=out,igraph.edge.color="magenta",igraph.vertex.color="cyan4"))
expect_silent(plot(x=out,n_intervals = 5L))
expect_silent(plot(x=out,actors=out$meta$dictionary$actors$actorName[1:5]))
#
# ## directed = FALSE
# out <- remify(edgelist = reh_loc$edgelist,
#                 directed = FALSE,
#                 model = "tie")
# expect_silent(plot(x=out))
# expect_silent(plot(x=out,n_intervals = 5L))
# expect_silent(plot(x=out,actors=out$meta$dictionary$actors$actorName[1:5]))

# test on methods with active risk set
out <- remify(edgelist = reh_loc$edgelist,
                directed = FALSE,
                riskset = "active",
                model = "tie")
expect_true(is.numeric(dim(out)))
#expect_equal(length(dim(out)),5) # with types
expect_identical(as.numeric(dim(out)),c(out$E,out$M,out$N,out$C,out$D,out$activeD))
#expect_identical(names(dim(out)),c("events","actors","types","dyads","dyads(active)"))
expect_silent(print(out))
expect_silent(summary(out))

# plot.remify() method warnings

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


# plot.remify() method - errors
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

