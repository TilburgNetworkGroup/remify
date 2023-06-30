# tie-oriented modeling
out <- remify(edgelist = randomREH$edgelist,
                actors = randomREH$actors,
                types = randomREH$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = randomREH$origin, # origin time is defiend
                omit_dyad = randomREH$omit_dyad, 
                model = "tie")

## tests on converted relevent-rem object ##

expect_silent(rehshape(data = out, output_format = "relevent-rem"))
to_relevent_rem_obj <- rehshape(data = out, output_format = "relevent-rem") 

# expectations on output object features
expect_inherits(to_relevent_rem_obj, "relevent-rem")
expect_true(is.list(to_relevent_rem_obj))
expect_equal(length(to_relevent_rem_obj),3)

# expectations on objects inside the 'remify' object
expect_identical(names(to_relevent_rem_obj),c("eventlist","supplist","timing"))
expect_equal(out$M, dim(to_relevent_rem_obj$supplist)[1])
expect_equal(out$M, dim(to_relevent_rem_obj$eventlist)[1])
expect_equal(2,dim(to_relevent_rem_obj$eventlist)[2])
expect_equal(out$D,dim(to_relevent_rem_obj$supplist)[2])

## tests on converted relevent-rem.dyad object ##
expect_silent(rehshape(data = out, output_format = "relevent-rem"))
to_relevent_rem.dyad_obj <- rehshape(data = out, output_format = "relevent-rem.dyad") 


# expectations on output object features
expect_inherits(to_relevent_rem.dyad_obj, "relevent-rem.dyad")
expect_true(is.list(to_relevent_rem.dyad_obj))
expect_equal(length(to_relevent_rem.dyad_obj),3)

# expectations on objects inside the 'remify' object
expect_identical(names(to_relevent_rem.dyad_obj),c("edgelist","n","ordinal"))
expect_equal(out$M, dim(to_relevent_rem.dyad_obj$edgelist)[1])
expect_equal(1,length(to_relevent_rem.dyad_obj$n))
expect_equal(1,length(to_relevent_rem.dyad_obj$ordinal))

# ncores
expect_error(rehshape(data = out, output_format = "relevent-rem", ncores = 1e05),
"'ncores' is recommended to be set at most to: floor(parallel::detectCores()-2L)",
fixed = TRUE)
expect_silent(rehshape(data = out, output_format = "relevent-rem", ncores = NULL))

## tests on error messages ##

# input class data
class(out) <- c("class1")
expect_error(rehshape(data = out, output_format = "relevent-rem"),
"'data' must be a 'remify' object",
fixed = TRUE)

## expecting no errors if origin = NULL
out <- remify(edgelist = randomREH$edgelist,
            actors = randomREH$actors,
            types = randomREH$types, 
            directed = TRUE, # events are directed
            ordinal = FALSE, # REM with waiting times
            origin = NULL, # origin time is defiend
            omit_dyad = randomREH$omit_dyad, 
            model = "tie")
expect_silent(rehshape(data = out, output_format = "relevent-rem"))    
expect_silent(rehshape(data = out, output_format = "relevent-rem.dyad"))  

# actor-oriented modeling

## with type
reh_loc <- randomREH
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = reh_loc$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = reh_loc$omit_dyad, 
                model = "actor")
expect_silent(rehshape(data = out, output_format = "relevent-rem"))
expect_silent(rehshape(data = out, output_format = "relevent-rem.dyad"))

## without type
reh_loc$edgelist$type <- NULL    
out <- remify(edgelist = reh_loc$edgelist,
                actors = reh_loc$actors,
                types = NULL, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = reh_loc$origin, # origin time is defiend
                omit_dyad = NULL, 
                model = "actor")
expect_silent(rehshape(data = out, output_format = "relevent-rem"))
expect_silent(rehshape(data = out, output_format = "relevent-rem.dyad"))
