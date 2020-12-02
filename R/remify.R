#' reh  
#'
#' A function that returns a 'reh' S3 object 
#'
#' @param edgelist a a dataframe of relational events sorted by time: [time,sender,receiver,type,weight]
#' @param actors vector of actors not in the network but to be considered in the analysis
#' @param types vector of types not in the network but to considered in the analysis
#' @param directed dyadic events directed (TRUE) or undirected (FALSE)
#' @param ordinal  TRUE if the only the time order of events is known, FALSE if also the time value is known
#' @param origin starting time point (default is NULL)
#' @param omit_dyad list where each element is a list of two elements: `time`, that is a vector of time points which to omit dyads from, `dyad`, which is a data.frame where dyads to omit are supplied (see more documentation about the potentials of omit_dyad in defining time varying risksets)
#'
#' @return  object list (the function saves also the output of optim)
#' @export

reh <- function(edgelist,
                actors = NULL,
                types = NULL,
                directed = TRUE,
                ordinal = FALSE,
                origin = NULL,
                omit_dyad = NULL){

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
    }

    
    # (1.2) Check NA's in `covariates` :
    # How are we going to handle this check?


    # Check for additional actors input (actors) :
  #  if(is.null(actors)) {actors <- character(0)} else{actors <- as.character(actors)}

    # Check for additional types input (types) :
  # if(is.null(types)) {types <- character(0)} else{types <- as.character(types)}

    # Pre-processing relational event history (rehCpp.cpp)
    out <- rehCpp(edgelist = edgelist,
                    actors = actors, 
                    types = types, 
                    directed = directed,
                    ordinal = ordinal,
                    origin = origin,
                    omit_dyad = omit_dyad)

    # possibly these won't be returned anymore
    #out$old_edgelist <- edgelist
    #out$old_omit_dyad <- omit_dyad
 
    
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