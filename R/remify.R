#' reh  
#'
#' A function that returns a 'reh' S3 object 
#'
#' @param edgelist a a dataframe of relational events sorted by time: [time,sender,receiver,type,weight]
#' @param riskset is a list of length equal to the number of events, each object a matrix with unobserved dyads (using actors string names)
#' @param covariates list of covariates to be provided according to the input structure working with 'remstats'
#' @param add_actors vector of actors not in the network but to be considered in the analysis
#' @param add_types vector of types not in the network but to considered in the analysis
#' @param directed dyadic events directed (TRUE) or undirected (FALSE)
#' @param ordinal  TRUE if the only the time order of events is known, FALSE if also the time value is known
#' @param time_unit string definind the time unit to consider when computing the waiting time between to subsequent relational events. If the `time` object class is `Date` then it can range in "day" (default), "month" and "year". If the class is `POSIXct` the time_unit can range in "second" (default), "minute", "hour", "day", "month", "year". If the class is either `integer` or `numeric` it is not necessary to define (it remains NULL by default) and the user should know which time unit measure the input edgelist refers to.
#'
#' @return  object list (the function saves also the output of optim)
#' @export

reh <- function(edgelist,
                riskset = replicate(NROW(edgelist),NaN,simplify=FALSE), 
                covariates = list(default = NULL),
                add_actors = NULL,
                add_types = NULL,
                directed = TRUE,
                ordinal = FALSE,
                time_unit = NULL){
    # Check for missing values (NA's) in `edgelist` :
    if(is.null(dim(edgelist)[1])) stop("The `edgelist` object is empty.")
	  if(anyNA(edgelist)) {
		warning("The `edgelist` object contains missing data: incomplete events (rows) are dropped.")
		to_remove <- which(is.na(edgelist), arr.ind = T)[,1]
		edgelist <- edgelist[-to_remove,]
    if(is.null(dim(edgelist)[1])) stop("The `edgelist` object is empty.")
    else{
      riskset <- riskset[-to_remove]
    }
    }

    # Check for missing values (NA's) in `riskset` :
    # presence of NA's is checked per each m: if the matrix of dyads to remove from the risk set at t_m as any NA
    for(m in 1:dim(edgelist)[1]) if(is.matrix(riskset[[m]]) & anyNA(riskset[[m]])) stop("NA found in at least one of the element of the `riskset` object. Dyads to remove from the riskset cannot ")
    
    # Check for missing values (NA's) in `covariates` :
    # How are we going to handle this check?

    # Check for additional actors input (add_actors) :
    if(is.null(add_actors)) {add_actors <- character(0)} else{add_actors <- as.character(add_actors)}

    # Check for additional types input (add_types) :
    if(is.null(add_types)) {add_types <- character(0)} else{add_types <- as.character(add_types)}

    # Check for the R class of variable `time`:

    # (1) If the R class of `time` is either `integer` or `numeric` 
    if((is.integer(edgelist[,1]) | is.numeric(edgelist[,1]))){
      time_class <- "numeric"
      if(!is.null(time_unit)) time_unit <- "NULL"
    }
    else{
    # (2) If the R class of `time` is `Date` (`Date` is the data type in Rcpp)
        if(is.Date(edgelist[,1])){
          time_class <- "Date"
          if(is.null(time_unit)) time_unit <- "day"
          else{
            if(!any(grepl(paste("^",time_unit,"$",sep=""),c("day","month","year"),ignore.case=TRUE))){
              stop("Error : input `time_unit` is not correct for class object `Date` ")
            }
            else{
              time_unit <- tolower(time_unit)
            }
          }
        }
        else{
    # (3) If the R class of `time` is `POSIXct` (`Datetime` is the data type in Rcpp)
          if(class(edgelist[,1])[1] == "POSIXct"){
            time_class <- "Datetime"
            if(is.null(time_unit)) time_unit <- "second"
            else{
              if(!any(grepl(paste("^",time_unit,"$",sep=""),c("second","minute","hour","day","month","year"),ignore.case=TRUE))){ # microseconds (?)
                stop("Error : input `time_unit` is not correct for class object `POSIXct` ")
              }
              else{
                time_unit <- tolower(time_unit)
              }
            }
          }
          else{
            stop("Error : the class of `time` object is not one of the following: integer, numeric, Date, POSIXct")
          }
        }
        
    }


    # Pre-processing relational event history (rehCpp.cpp)
    out <- rehCpp(edgelist = edgelist,
                    riskset = riskset, 
                    covariates = covariates, 
                    add_actors = add_actors, 
                    add_types = add_types, 
                    directed = directed,
                    ordinal = ordinal,
                    time_unit = time_unit,
                    time_class = time_class)

    # possibly these won't be returned anymore
    out$old_edgelist <- edgelist
    out$old_riskset <- riskset
    out$old_covariates <- covariates
    
    class(out) <- "reh"
    return(out)
}

#' remify 
#'
#' A function that transform the REH input in one of the possible REH formats.
#'
#' @param input
#'
#' @return  otuput of remify function
#' @export

remify <- function(input){
# [...] do stuff here
                }                   