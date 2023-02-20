test_that("rehshape", {
    out <- reh(edgelist = randomREH$edgelist,
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

    # expectations on objects inside the 'reh' object
    expect_identical(names(to_relevent_obj),c("eventlist","supplist","timing"))
    expect_equal(out$M, dim(to_relevent_obj$supplist)[1])
    expect_equal(out$M, dim(to_relevent_obj$eventlist)[1])
    expect_equal(2,dim(to_relevent_obj$eventlist)[2])
    expect_equal(out$D,dim(to_relevent_obj$supplist)[2])

    ## tests on converted reh object ##
    to_relevent_obj$N <- 20
    to_relevent_obj$C <- 3
    to_reh_obj <- rehshape(data = to_relevent_obj, output_format = "reh") 

    ## tests on error messages ##

    # length of class input data is greater than 1
    class(to_relevent_obj) <- c("relevent","rem")
    expect_error(rehshape(data = to_relevent_obj, output_format = "reh"),
    "class of input data must be of length 1.")
    
    # input class data
    class(to_relevent_obj) <- c("class1")
    expect_error(rehshape(data = to_relevent_obj, output_format = "reh"),
    "class of input data must be either `reh` or `relevent`.")

    ## tests on warning messages ##

    # same input and output structure
    class(to_relevent_obj) <- c("relevent")
    expect_warning(rehshape(data = to_relevent_obj, output_format = "relevent"),
    "the format of the input data is the same as the required output format. The input data is returned.")

    ## expecting no errors if origin = NULL
    out <- reh(edgelist = randomREH$edgelist,
                actors = randomREH$actors,
                types = randomREH$types, 
                directed = TRUE, # events are directed
                ordinal = FALSE, # REM with waiting times
                origin = NULL, # origin time is defiend
                omit_dyad = randomREH$omit_dyad, 
                model = "tie")
    expect_no_error(rehshape(data = out, output_format = "relevent"))     

})