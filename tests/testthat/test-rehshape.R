test_that("rehshape", {

    # tie-oriented modeling
    out <- remify(edgelist = randomREH$edgelist,
                    actors = randomREH$actors,
                    types = randomREH$types, 
                    directed = TRUE, # events are directed
                    ordinal = FALSE, # REM with waiting times
                    origin = randomREH$origin, # origin time is defiend
                    omit_dyad = randomREH$omit_dyad, 
                    model = "tie")

    ## tests on converted relevent object ##
    expect_no_error(rehshape(data = out, output_format = "relevent"))
    to_relevent_obj <- rehshape(data = out, output_format = "relevent") 

    # expectations on output object features
    expect_s3_class(to_relevent_obj, "relevent")
    expect_true(is.list(to_relevent_obj))
    expect_equal(length(to_relevent_obj),3)

    # expectations on objects inside the 'remify' object
    expect_identical(names(to_relevent_obj),c("eventlist","supplist","timing"))
    expect_equal(out$M, dim(to_relevent_obj$supplist)[1])
    expect_equal(out$M, dim(to_relevent_obj$eventlist)[1])
    expect_equal(2,dim(to_relevent_obj$eventlist)[2])
    expect_equal(out$D,dim(to_relevent_obj$supplist)[2])

    ## tests on converted reh object ##
    to_relevent_obj$N <- 20
    to_relevent_obj$C <- 3
    to_relevent_obj$directed <- TRUE
    to_remify_obj <- rehshape(data = to_relevent_obj, output_format = "remify") 
    expect_s3_class(to_remify_obj, "remify")

    ## tests on error messages ##
    
    # input class data
    class(to_relevent_obj) <- c("class1")
    expect_error(rehshape(data = to_relevent_obj, output_format = "remify"),
    "'data' must be either a 'remify' object or a (artificial) object of class 'relevent'.",
    fixed = TRUE)

    ## tests on warning messages ##

    # same input and output structure
    class(to_relevent_obj) <- c("relevent")
    expect_error(rehshape(data = to_relevent_obj, output_format = "relevent"),
    "'output_format' and class of 'data' must be different.",
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
    expect_no_error(rehshape(data = out, output_format = "relevent"))    

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
    expect_no_error(rehshape(data = out, output_format = "relevent"))
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
    expect_no_error(rehshape(data = out, output_format = "relevent"))
    

})