
## tests on methods

# method dim() with type
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie")        
expect_true(is.numeric(dim(out)))
expect_equal(length(dim(out)),4) # with types
expect_identical(as.numeric(dim(out)),c(out$M,out$N,out$C,out$D))
expect_identical(names(dim(out)),c("events","actors","types","dyads"))

# method dim() without type
reh_loc <- randomREH
reh_loc$edgelist$type <- NULL
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = NULL,
                model = "tie")        
expect_true(is.numeric(dim(out)))
expect_equal(length(dim(out)),3) # with types
expect_identical(as.numeric(dim(out)),c(out$M,out$N,out$D))
expect_identical(names(dim(out)),c("events","actors","dyads"))

# method dim() with simultaneous events (model == "tie") and types
reh_loc <- randomREH
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
reh_loc$origin <- as.Date(reh_loc$origin)-1
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = NULL,
                model = "tie",
                riskset = "active")

# expectations on output object features               
expect_equal(length(out), 8)
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
                types = NULL, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = NULL,
                model = "tie",
                riskset = "active")

# expectations on output object features               
expect_equal(length(out), 8)
expect_true(is.numeric(dim(out)))
expect_equal(length(dim(out)),5) # with types
expect_identical(as.numeric(dim(out)),c(out$E,out$M,out$N,out$D,out$activeD))
expect_identical(names(dim(out)),c("events","time points","actors","dyads","dyads(active)"))


# method dim() with simultaneous events (model == "actor")
reh_loc <- randomREH
reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
reh_loc$origin <- as.Date(reh_loc$origin)-1
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = NULL,
                model = "actor")

# expectations on output object features               
expect_equal(length(out), 7)
expect_true(is.numeric(dim(out)))
expect_equal(length(dim(out)),5) # with types
expect_identical(as.numeric(dim(out)),c(out$E,out$M,out$N,out$C,out$D))
expect_identical(names(dim(out)),c("events","time points","actors","types","dyads"))


# method getRiskset()

## (1) when model = "tie" and omit_dyad is supplied
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                riskset = "manual",
                omit_dyad = reh_loc$omit_dyad,
                model = "tie")              

expect_identical(attr(out,"riskset"),"manual")
expect_true(is.list(getRiskset(out)))
expect_equal(length(getRiskset(out)),1)
expect_identical(names(getRiskset(out)),"riskset")

## (2) when model = "actor" and omit_dyad is supplied
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                riskset = "manual",
                omit_dyad = reh_loc$omit_dyad,
                model = "actor")              

expect_identical(attr(out,"riskset"),"manual")
expect_true(is.list(getRiskset(out)))
expect_equal(length(getRiskset(out)),2)
expect_identical(names(getRiskset(out)),c("sender","dyad"))  

## (3) error message when riskset is not manual
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                riskset = "full",
                model = "tie")
expect_identical(attr(out,"riskset"),"full")                
expect_error(getRiskset(out),
"risk set is neither 'active' nor 'manual'.",
fixed = TRUE)


# methods getActorName(), getTypeName(), getDyad(), getActorID(), getTypeID(), getDyadID()

reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie")

## (1) method getActorName()                
expect_true(is.character(getActorName(x = out,actorID = c(2,3,4))))
expect_equal(length(getActorName(x = out,actorID = c(2,3,4))),3)
expect_identical(getActorName(x = out,actorID = c(2,3,4)),c("Andrey","Breanna","Charles"))
expect_error(getActorName(x = out, actorID = NULL),
"provide at least one actorID.",
fixed = TRUE)
expect_error(getActorName(x = out, actorID = out$N+10),
"no actorID was found in the dictionary.",
fixed = TRUE)
expect_warning(getActorName(x = out, actorID = c(2,3,4,out$N+10)),
  "some actorID was not found in the dictionary.",
  fixed = TRUE
)
expect_error(getActorName(x = out, actorID = as.character(rnorm(1,mean=1))),
"'actorID' must be numeric or integer.",
fixed = TRUE)
expect_identical(suppressWarnings(getActorName(x = out,actorID = c(2,3,4,out$N+10))),c("Andrey","Breanna","Charles"))

## (2) method getTypeName()
expect_true(is.character(getTypeName(x = out,typeID = c(1,2,3))))
expect_equal(length(getTypeName(x = out,typeID = c(1,2,3))),3)
expect_identical(getTypeName(x = out,typeID = c(1,2,3)),c("competition","conflict","cooperation"))
expect_error(getTypeName(x = out, typeID = NULL),
"provide at least one typeID.",
fixed = TRUE)
expect_error(getTypeName(x = out, typeID = out$C+10),
"no typeID was found in the dictionary.",
fixed = TRUE)
expect_warning(getTypeName(x = out, typeID = c(1,2,3,out$C+10)),
  "some typeID was not found in the dictionary.",
  fixed = TRUE
)
expect_error(getTypeName(x = out, typeID = as.character(rnorm(1,mean=1))),
"'typeID' must be numeric or integer.",
fixed = TRUE)
expect_identical(suppressWarnings(getTypeName(x = out,typeID = c(1,2,3,out$C+10))),c("competition","conflict","cooperation"))

### when no types are available
out_no_event_types <- remify::remify(edgelist=reh_loc$edgelist[,1:3], model="tie")
expect_error(getTypeName(x = out_no_event_types, typeID = 1),
"'remify' object has no event types",
fixed = TRUE)

## (3) method getActorID()
expect_true(is.integer(getActorID(x = out,actorName = c("Maya","Derek","Megan"))))
expect_equal(length(getActorID(x = out,actorName = c("Maya","Derek","Megan"))),3)
expect_identical(getActorID(x = out,actorName = c("Maya","Derek","Megan")),as.integer(c(14,7,16)))
expect_error(getActorID(x = out, actorName = NULL),
"provide at least one actorName.",
fixed = TRUE)
expect_error(getActorID(x = out, actorName = as.character(rnorm(1,mean=1))),
"no actorName was found in the dictionary.",
fixed = TRUE)
expect_warning(getActorID(x = out, actorName = c("Maya","Derek","Megan",as.character(rnorm(1,mean=1)))),
  "some actorName was not found in the dictionary.",
  fixed = TRUE
)
expect_identical(suppressWarnings(getActorID(x = out,actorName = c("Maya","Derek","Megan",as.character(rnorm(1,mean=1))))),as.integer(c(14,7,16)))


## (4) method getTypeID()
expect_true(is.integer(getTypeID(x = out,typeName = c("cooperation","conflict"))))
expect_equal(length(getTypeID(x = out,typeName = c("cooperation","conflict"))),2)
expect_identical(getTypeID(x = out,typeName = c("cooperation","conflict")),as.integer(c(3,2)))
expect_error(getTypeID(x = out, typeName = NULL),
"provide at least one typeName.",
fixed = TRUE)
expect_error(getTypeID(x = out, typeName = as.character(rnorm(1,mean=1))),
"no typeName was found in the dictionary.",
fixed = TRUE)
expect_warning(getTypeID(x = out, typeName = c("cooperation","conflict",as.character(rnorm(1,mean=1)))),
  "some typeName was not found in the dictionary.",
  fixed = TRUE
)
expect_identical(suppressWarnings(getTypeID(x = out,typeName = c("cooperation","conflict",as.character(rnorm(1,mean=1))))),as.integer(c(3,2)))

### when no event types are available
out_no_event_types <- remify::remify(edgelist=randomREH$edgelist[,1:3], model="tie")
expect_error(getTypeID(x = out_no_event_types, typeName="type1"),
"'remify' object has no event types",
fixed = TRUE)


# method getDyad()
expect_true(is.data.frame(getDyad(x = out,dyadID = c(1))))
expect_equal(dim(getDyad(x = out, dyadID = c(2,3,4)))[2],4)
expect_identical(names(getDyad(x = out,dyadID = c(1))),c("dyadID","actor1","actor2","type"))
expect_identical(getDyad(x = out,dyadID = c(1))$dyadID,c(1L))
expect_identical(getDyad(x = out,dyadID = c(1))$actor1,c("Alexander"))
expect_identical(getDyad(x = out,dyadID = c(1))$actor2,c("Andrey"))
expect_identical(getDyad(x = out,dyadID = c(1))$type,c("competition"))
expect_error(getDyad(x = out,dyadID = c("1")),
"'dyadID' must be a numeric (or integer) vector",
fixed = TRUE)
expect_error(getDyad(x = out, dyadID = c(1), active = TRUE),
"'active' = TRUE works only for attr(x,'riskset') = 'active'",
fixed = TRUE)
expect_warning(getDyad(x = out, dyadID = c(1,1,2)),
  "'dyadID' contains ID's that are repeated more than once. Such ID's will be processed once",
  fixed = TRUE
)

tryCatch_error_loc<- tryCatch(getDyad(x = out,dyadID = c(0)),error=function(e) e)       
expect_inherits(tryCatch_error_loc,c("Rcpp::exception","C++Error","error","condition")) 
expect_match(print(tryCatch_error_loc),
"<Rcpp::exception: one or more dyad ID's can't be found in the remify object 'x': dyad ID's must range between 1 and x$D>")

# getDyad without type
reh_loc$edgelist$type <- NULL
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = NULL, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = NULL,
                model = "tie")
tryCatch_error_loc <- tryCatch(getDyad(x = out,dyadID = c(0)),error=function(e) e)      
expect_inherits(tryCatch_error_loc,c("Rcpp::exception","C++Error","error","condition")) 
expect_match(print(tryCatch_error_loc),
"Error: one or more dyad ID's can't be found in the remify object 'x': dyad ID's must range between 1 and x$D\n")

expect_silent(getDyad(x = out, dyadID = c(1:10)))
expect_true(is.data.frame(getDyad(x = out, dyadID = c(1:10))))
expect_identical(getDyad(x = out, dyadID = c(1:10))$actor1,rep("Alexander",10))
expect_identical(getDyad(x = out, dyadID = c(1:10))$actor2,c("Andrey","Breanna","Charles","Colton","Crystal","Derek","Francesca","Justin","Kayla","Kelsey"))
expect_identical(getDyad(x = out, dyadID = c(1:10))$dyadID,c(1:10))

# method getDyadID()

## network with type
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = reh_loc$omit_dyad,
                model = "tie")

expect_error(getDyadID(x = out, actor1 = "Alexander", actor2 = "Charles", type = c("cooperation","conflict")),
"'type' must be a character vector of length 1",
fixed = TRUE)
expect_error(getDyadID(x = out, actor1 = "Alexander", actor2 = c("Charles","Colton"), type = c("conflict")),
"'actor1' and 'actor2' must be character vectors of length 1",
fixed = TRUE)
expect_error(getDyadID(x = out, actor1 = "Alexander", actor2 = c("Alexander"), type = c("conflict")),
"'actor1' and 'actor2' must be different",
fixed = TRUE)  
expect_error(getDyadID(x = out, actor1 = "Alexander", actor2 = c("Chareles"), type = c("conflict")),
  "input  'actor2' not found in the 'remify' object",
  fixed = TRUE
)
expect_error(getDyadID(x = out, actor1 = "Alexaner", actor2 = c("Chareles"), type = c("conflict")),
  "input 'actor1' and 'actor2' not found in the 'remify' object",
  fixed = TRUE
)
expect_error(getDyadID(x = out, actor1 = "Alexaner", actor2 = c("Charles"), type = c("conflict")),
  "input 'actor1' not found in the 'remify' object",
  fixed = TRUE
)
expect_error(getDyadID(x = out, actor1 = "Alexander", actor2 = c("Charles"), type = c("conflicts")),
  "'type' not found in the 'remify' object",
  fixed = TRUE
)
expect_silent(getDyadID(x = out, actor1 = "Alexander", actor2 = "Charles", type = c("cooperation")))

## network without type
reh_loc <- randomREH
reh_loc$edgelist$type <- NULL
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = NULL, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin,
                omit_dyad = NULL,
                model = "tie")
expect_silent(getDyadID(x = out, actor1 = "Alexander", actor2 = "Charles"))

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
expect_silent(plot(x=out,actors=attr(out,"dictionary")$actors$actorName[1:5]))


## directed = FALSE
out <- remify(edgelist = reh_loc$edgelist,
                directed = FALSE,
                model = "tie")  
expect_silent(plot(x=out))
expect_silent(plot(x=out,n_intervals = 5L))
expect_silent(plot(x=out,actors=attr(out,"dictionary")$actors$actorName[1:5]))

# test on methods with active risk set
out <- remify(edgelist = reh_loc$edgelist,
                directed = FALSE,
                riskset = "active",
                model = "tie")  
expect_true(is.numeric(dim(out)))
#expect_equal(length(dim(out)),5) # with types
expect_identical(as.numeric(dim(out)),c(out$M,out$N,out$C,out$D,out$activeD))
#expect_identical(names(dim(out)),c("events","actors","types","dyads","dyads(active)"))
expect_silent(print(out))
expect_silent(summary(out))
# getDyad method                  
expect_silent(getDyad(x = out, dyadID = c(1), active = TRUE))          
# getDyadID method
expect_silent(getDyadID(x = out, actor1 = "Alexander", actor2 = "Charles", type = "cooperation"))          
