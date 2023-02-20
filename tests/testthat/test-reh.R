test_that("reh", {
  out <- reh(edgelist = randomREH$edgelist,
                    actors = randomREH$actors,
                    types = randomREH$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = randomREH$origin,
                    omit_dyad = randomREH$omit_dyad,
                    model = "tie")

  # expectations on output object features               
  expect_s3_class(out, "reh")
  expect_true(is.list(out))
  expect_equal(length(out), 7)

  # expectations on objects inside the 'reh' object
  expect_identical(names(out),c("M","N","C","D","intereventTime","edgelist","omit_dyad"))
  expect_equal(out$M, dim(randomREH$edgelist)[1])
  expect_equal(out$edgelist[,1],as.numeric(randomREH$edgelist[,1]))

  # expectations on attributes of the 'reh' object 
  expect_identical(names(attributes(out)),c("names","class","with_type","weighted","directed","ordinal","model","riskset","dictionary","time"))
  expect_false(attr(out,"ordinal")) 
  expect_true(attr(out,"directed"))
  expect_identical(attr(out,"model"),"tie")



  ## tests on error messages ##

  # 'edgelist' is not a data.frame
  reh_loc <- randomREH
  reh_loc$edgelist <- data.matrix(reh_loc$edgelist)
  expect_error(
    reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = reh_loc$omit_dyad,
                    model = "tie"),
    "input `edgelist` must be of class `data.frame`.",
    fixed = TRUE
  )

  # column 'time' missing
  reh_loc <- randomREH
  colnames(reh_loc$edgelist)[1] <- ""
  expect_error(
    reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = reh_loc$omit_dyad,
                    model = "tie"),
    "`edgelist` should contain a column named `time` with the timing/order information for the events.",
    fixed = TRUE
  )

  # column 'actor1' missing
  reh_loc <- randomREH
  colnames(reh_loc$edgelist)[2] <- ""
  expect_error(
    reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = reh_loc$omit_dyad,
                    model = "tie"),
    "`edgelist` should contain a column named `actor1` with the first actors/senders of the events.",
    fixed = TRUE
  )

  # column 'actor2' missing
  reh_loc <- randomREH
  colnames(reh_loc$edgelist)[3] <- ""
  expect_error(
    reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = reh_loc$omit_dyad,
                    model = "tie"),
    "`edgelist` should contain a column named `actor2` with the second actors/receivers of the events.",
    fixed = TRUE
  )

  # argument 'model' not correctly specified
  reh_loc <- randomREH
  expect_error(
    reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = reh_loc$omit_dyad,
                    model = "tie-oriented"),
    "argument `model` must be set to either `tie` or `actor`.",
    fixed=TRUE
  )


  # class of 'origin' and class of 'time' column in 'edgelist' are different
  reh_loc <- randomREH
  reh_loc$origin <- as.Date(reh_loc$origin)
  expect_error(
    reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = NULL,
                    model = "tie"),
    "the class of input `origin` and the class of `edgelist$time` must be the same.",
    fixed = TRUE
  )

  # input 'omit_dyad' is not a list
  reh_loc <- randomREH
  reh_loc$omit_dyad <- data.matrix(reh_loc$omit_dyad[[1]]$dyad)
  expect_error(
    reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = reh_loc$omit_dyad,
                    model = "tie"),
    "the input `omit_dyad` must be a list. Check vignette(topic = 'reh', package= 'remify') for more information.",
    fixed = TRUE
  )

  # class of `time` in 'omit_dyad' and class of 'time' column in 'edgelist' are different (at least for one object inside omit_dyad)
  reh_loc <- randomREH
  reh_loc$omit_dyad[[1]]$time <- as.Date(reh_loc$omit_dyad[[1]]$time)
  expect_error(
    reh(edgelist = reh_loc$edgelist,
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
    reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = reh_loc$omit_dyad,
                    model = "tie"),
    "the input `omit_dyad` must be a collection of lists with two named objects: `dyad` and `time`. Check vignette(topic = 'reh', package= 'remify') for more information.",
    fixed = TRUE
  )

  # errors from Rcpp functions, handled via expect_output(print(tryCatch(expr,error=function(e) e)),"error message",fixed=TRUE)
  
  # time points defined in omit_dyad are removed
  reh_loc <- randomREH
  reh_loc$omit_dyad[[1]]$time[2] <- reh_loc$edgelist$time[9000]+60
  tryCatch_error_loc <- tryCatch(reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin, # origin time is defiend
                  omit_dyad = reh_loc$omit_dyad, 
                  model = "tie"),error=function(e) e)
  expect_output(print(tryCatch_error_loc),
  "<Rcpp::exception: either start or stop in one of the elements in the list 'omit_dyad' are not found in the edgelist. Please, provide observed time points as start and stop values>",
  fixed = TRUE
  )
  
  # when more than two time points are supplied in any of the object inside `omit_dyad`
  reh_loc <- randomREH
  reh_loc$omit_dyad[[1]]$time <- c(reh_loc$omit_dyad[[1]]$time,reh_loc$edgelist$time[9000]+60)
  tryCatch_error_loc <- tryCatch(reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin, # origin time is defiend
                  omit_dyad = reh_loc$omit_dyad, 
                  model = "tie"),error=function(e) e)
  expect_output(print(tryCatch_error_loc),
  "<Rcpp::exception: Error: time vector in each element of the list 'omit_dyad' must be of length 2: start and stop time when the riskset changed>",
  fixed = TRUE
  )

  # when more than two time points are supplied in any of the object inside `omit_dyad`
  reh_loc <- randomREH
  reh_loc$omit_dyad[[1]]$time <- c(reh_loc$omit_dyad[[1]]$time[2],reh_loc$omit_dyad[[1]]$time[1])
  tryCatch_error_loc <- tryCatch(reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin, # origin time is defiend
                  omit_dyad = reh_loc$omit_dyad, 
                  model = "tie"),error=function(e) e)
  expect_output(print(tryCatch_error_loc),
  "<Rcpp::exception: time vector in each element of the list 'omit_dyad' must be sorted so that elements indicate respectively start and stop time when the riskset changed>",
  fixed = TRUE
  )

  # warning when directed = FALSE and model = "actor"
  reh_loc <- randomREH
  tryCatch_error_loc <- tryCatch(reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = FALSE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin, # origin time is defiend
                  omit_dyad = reh_loc$omit_dyad, 
                  model = "actor"),error=function(e) e)
  expect_output(print(tryCatch_error_loc),
  "<Rcpp::exception: Error: actor-oriented model can only work with directed networks>",
  fixed = TRUE
  )

  ## tests on warning messages ##

  # agument 'model' set to default
  reh_loc <- randomREH
  expect_warning(
    reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = reh_loc$omit_dyad),
    "argument `model` set to `tie` by default",
    fixed = TRUE
  )

  # 'edgelist' contains missing data
  reh_loc <- randomREH
  reh_loc$edgelist[1,c(2,4)] <- c(NA,NA)
  expect_warning(
    reh(edgelist = reh_loc$edgelist,
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
  expect_output(reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin, # origin time is defiend
                  omit_dyad = reh_loc$omit_dyad, 
                  model = "tie"),
  "Warning: the `time` variable is not sorted. Sorting will be forced.",
  fixed = TRUE
  )

  # first `time` value and `origin` are the same
  reh_loc <- randomREH
  reh_loc$origin <- reh_loc$edgelist$time[1]
  expect_output(reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin, # origin time is defiend
                  omit_dyad = reh_loc$omit_dyad, 
                  model = "tie"),
  "Warning: both `origin` and first time point have the same value. `origin` is then automatically set either to one day/second before the first time point or to 0.",
  fixed = TRUE
  )

  # `time` column is not sorted
  reh_loc <- randomREH
  reh_loc$omit_dyad[[2]]$dyad$actor2[4] <-  as.character(rpois(1,lambda = 30)) 
  expect_output(reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin, # origin time is defiend
                  omit_dyad = reh_loc$omit_dyad, 
                  model = "tie"),
  "Warning: one or more actors/types supplied in `omit_dyad` were not found in the edgelist. Therefore the corresponding rows defined in the data.frame `dyad` were removed.",
  fixed = TRUE
  )

  # method dim()
  reh_loc <- randomREH
  out <- reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")        
  #method dim()
  expect_true(is.numeric(dim(out)))
  expect_equal(length(dim(out)),4)
  expect_identical(as.numeric(dim(out)),c(out$M,out$N,out$C,out$D))
  expect_named(dim(out),c("events","actors","types","dyads"))

  # method getDynamicRiskset()
  ## (1) when model = "tie" and omit_dyad is supplied
  reh_loc <- randomREH
  out <- reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")              

  expect_identical(attr(out,"riskset"),"dynamic")
  expect_true(is.list(getDynamicRiskset(out)))
  expect_equal(length(getDynamicRiskset(out)),1)
  expect_named(getDynamicRiskset(out),"riskset")

  ## (2) when model = "actor" and omit_dyad is supplied
  reh_loc <- randomREH
  out <- reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "actor")              

  expect_identical(attr(out,"riskset"),"dynamic")
  expect_true(is.list(getDynamicRiskset(out)))
  expect_equal(length(getDynamicRiskset(out)),2)
  expect_named(getDynamicRiskset(out),c("sender","dyad"))  

  ## (3) error message when riskset is not dynamic
  reh_loc <- randomREH
  reh_loc$omit_dyad <- NULL
  out <- reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
  expect_identical(attr(out,"riskset"),"static")                
  expect_error(getDynamicRiskset(out),
  "risk set is not dynamic",
  fixed = TRUE)

  
  # methods actorName(), typeName(), actorID(), typeID()
  reh_loc <- randomREH
  out <- reh(edgelist = reh_loc$edgelist,
                  actors = reh_loc$actors,
                  types = reh_loc$types, 
                  directed = TRUE, # events are directed
                  ordinal = FALSE, # REM with waiting times
                  origin = reh_loc$origin,
                  omit_dyad = reh_loc$omit_dyad,
                  model = "tie")
  ## (1) method actorName()                
  expect_true(is.character(actorName(reh = out,actorID = c(1,2,3))))
  expect_equal(length(actorName(reh = out,actorID = c(1,2,3))),3)
  expect_identical(actorName(reh = out,actorID = c(1,2,3)),c("Andrey","Breanna","Charles"))
  expect_error(actorName(reh = out, actorID = NULL),
  "provide at least one actorID.",
  fixed = TRUE)
  expect_error(actorName(reh = out, actorID = 1e10),
  "no actorID was found in the dictionary.",
  fixed = TRUE)
  expect_warning(actorName(reh = out, actorID = c(1,2,3,1e10)),
    "some actorID was not found in the dictionary.",
    fixed = TRUE
  )
  expect_identical(suppressWarnings(actorName(reh = out,actorID = c(1,2,3,1e10))),c("Andrey","Breanna","Charles"))

  ## (2) method typeName()
  expect_true(is.character(typeName(reh = out,typeID = c(0,1,2))))
  expect_equal(length(typeName(reh = out,typeID = c(0,1,2))),3)
  expect_identical(typeName(reh = out,typeID = c(0,1,2)),c("competition","conflict","cooperation"))
  expect_error(typeName(reh = out, typeID = NULL),
  "provide at least one typeID.",
  fixed = TRUE)
  expect_error(typeName(reh = out, typeID = 1e10),
  "no typeID was found in the dictionary.",
  fixed = TRUE)
  expect_warning(typeName(reh = out, typeID = c(0,1,2,1e10)),
    "some typeID was not found in the dictionary.",
    fixed = TRUE
  )
  expect_identical(suppressWarnings(typeName(reh = out,typeID = c(0,1,2,1e10))),c("competition","conflict","cooperation"))


  ## (3) method actorID()
  expect_true(is.integer(actorID(reh = out,actorName = c("Maya","Derek","Megan"))))
  expect_equal(length(actorID(reh = out,actorName = c("Maya","Derek","Megan"))),3)
  expect_identical(actorID(reh = out,actorName = c("Maya","Derek","Megan")),as.integer(c(13,6,15)))
  expect_error(actorID(reh = out, actorName = NULL),
  "provide at least one actorName.",
  fixed = TRUE)
  expect_error(actorID(reh = out, actorName = as.character(rnorm(1,mean=1))),
  "no actorName was found in the dictionary.",
  fixed = TRUE)
  expect_warning(actorID(reh = out, actorName = c("Maya","Derek","Megan",as.character(rnorm(1,mean=1)))),
    "some actorName was not found in the dictionary.",
    fixed = TRUE
  )
  expect_identical(suppressWarnings(actorID(reh = out,actorName = c("Maya","Derek","Megan",as.character(rnorm(1,mean=1))))),as.integer(c(13,6,15)))


  ## (4) method typeID()
  expect_true(is.integer(typeID(reh = out,typeName = c("cooperation","conflict"))))
  expect_equal(length(typeID(reh = out,typeName = c("cooperation","conflict"))),2)
  expect_identical(typeID(reh = out,typeName = c("cooperation","conflict")),as.integer(c(2,1)))
  expect_error(typeID(reh = out, typeName = NULL),
  "provide at least one typeName.",
  fixed = TRUE)
  expect_error(typeID(reh = out, typeName = as.character(rnorm(1,mean=1))),
  "no typeName was found in the dictionary.",
  fixed = TRUE)
  expect_warning(typeID(reh = out, typeName = c("cooperation","conflict",as.character(rnorm(1,mean=1)))),
    "some typeName was not found in the dictionary.",
    fixed = TRUE
  )
  expect_identical(suppressWarnings(typeID(reh = out,typeName = c("cooperation","conflict",as.character(rnorm(1,mean=1))))),as.integer(c(2,1)))

})


test_that("test (1) on method summary()",{
  reh_loc <- randomREH
  expect_snapshot(
    summary(reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = reh_loc$omit_dyad,
                    model = "tie")))
})

test_that("test (2) on method summary()",{
  ## without input 'origin'
  reh_loc <- randomREH
  expect_snapshot(
    summary(reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = NULL,
                    omit_dyad = reh_loc$omit_dyad,
                    model = "tie"))
  )
})

test_that("test (3) on method summary()",{
  ## time input set to Date
  reh_loc <- randomREH
  reh_loc$edgelist$time <- as.Date(reh_loc$edgelist$time)
  reh_loc$origin <- as.Date(reh_loc$origin)-1
  reh_loc$omit_dyad <- NULL
  expect_snapshot(
    summary(reh(edgelist = reh_loc$edgelist,
                    actors = reh_loc$actors,
                    types = reh_loc$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = reh_loc$origin,
                    omit_dyad = reh_loc$omit_dyad,
                    model = "tie"))
  )
})