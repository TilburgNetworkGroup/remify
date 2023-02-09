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
    "`edgelist` should contain a column named `time` with the timing/order information for the events."
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
    "`edgelist` should contain a column named `actor1` with the first actors/senders of the events."
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
    "`edgelist` should contain a column named `actor2` with the second actors/receivers of the events."
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
    "argument `model` must be set to either `tie` or `actor`"
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
    "argument `model` set to `tie` by default"
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
    "`edgelist` contains missing data: incomplete events are dropped."
  )
})