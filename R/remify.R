#' reh  
#'
#' A function that returns a 'reh' S3 object 
#'
#' @param edgelist a a dataframe of relational events sorted by time: [time,sender,receiver,type,weight]
#' @param covariates list of covariates to be provided according to the input structure working with 'remstats'
#' @param add_actors vector of actors not in the network but to be considered in the analysis
#' @param add_types vector of types not in the network but to considered in the analysis
#' @param directed dyadic events directed (TRUE) or undirected (FALSE)
#' @param ordinal  TRUE if the only the time order of events is known, FALSE if also the time value is known
#' @param origin starting time point (default is NULL)
#' @param riskset is a list of length equal to the number of events, each object a matrix with unobserved dyads (using actors string names)
#'
#' @return  object list (the function saves also the output of optim)
#' @export

reh <- function(edgelist,
                covariates = list(default = NULL),
                add_actors = NULL,
                add_types = NULL,
                directed = TRUE,
                ordinal = FALSE,
                origin = NULL,
                riskset = replicate(NROW(edgelist),NaN,simplify=FALSE)){

    # (2) Checking for `edgelist` columns (names and class of time variable)

    # (3) Checking for `origin` and `time` variable class (they must be the same)

    # (4) Checking for `riskset` object names 

    # (1) Checking for NA's 

    # (1.1) NA's in `edgelist` :
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

    # (1.2) NA's in `riskset` :
    # Checked per each m: if the matrix of dyads to remove from the risk set at t_m as any NA
    for(m in 1:dim(edgelist)[1]) if(is.matrix(riskset[[m]]) & anyNA(riskset[[m]])) stop(errorMessage(0))
    
    # (1.3) Check NA's in `covariates` :
    # How are we going to handle this check?


    # Check for additional actors input (add_actors) :
  #  if(is.null(add_actors)) {add_actors <- character(0)} else{add_actors <- as.character(add_actors)}

    # Check for additional types input (add_types) :
  # if(is.null(add_types)) {add_types <- character(0)} else{add_types <- as.character(add_types)}

    # Pre-processing relational event history (rehCpp.cpp)
    out <- rehCpp(edgelist = edgelist,
                    covariates = covariates, 
                    add_actors = add_actors, 
                    add_types = add_types, 
                    directed = directed,
                    ordinal = ordinal,
                    origin = origin,
                    riskset = riskset)

    # possibly these won't be returned anymore
    out$old_edgelist <- edgelist
    out$old_riskset <- riskset
    out$old_covariates <- covariates
    
    class(out) <- "reh"
    return(out)
}

#' remify 
#'
#' A function that transforms the REH input in one of the possible REH formats.
#'
#' @param input
#'
#' @return  otuput of remify function
#' @export

remify <- function(input){
# [...] do stuff here
                }                   