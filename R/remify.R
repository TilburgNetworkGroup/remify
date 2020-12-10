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

    
    str_out <- structure(list(
      M = out$M,
      N = out$N,
      C = out$C,
      D = out$D,
      intereventTime = out$intereventTime,
      edgelist = out$edgelist,
      risksetMatrix = out$risksetMatrix,
      risksetCube = out$risksetCube,
      rehBinary = out$rehBinary
    ), class="reh")

   
    
    attr(str_out, "with_type") <- out$with_type
    attr(str_out, "weighted") <- out$weighted
    attr(str_out, "directed") <- directed
    attr(str_out, "ordinal") <- ordinal
    attr(str_out, "riskset") <- ifelse(length(omit_dyad)>0,"dynamic","static")
    attr(str_out, "dictionary") <- list(actors = out$actorsDictionary, types = out$typesDictionary) 
    attr(str_out, "time") <- list(class = class(edgelist$time), value = data.frame(time = edgelist$time, intereventTime = out$intereventTime), origin = origin)

    return(str_out)
}


#######################################################################################
#######################################################################################
##########(START)           Methods for `reh` object           (START)#################
#######################################################################################
#######################################################################################

#' @title getName
#' 
#' A function that returns the name of a vector of actors or types given their ID's
#' 
#' @param reh an reh object
#' @param actorID other arguments
#' @param typeID other arguments
#' @param ... other arguments
#' @examples
#' data("randomREH")
#' edgelist_reh <- reh(edgelist = edgelist,
#'                    actors = actors,
#'                    types = types, 
#'                    directed = TRUE,
#'                   ordinal = FALSE,
#'                    origin = origin,
#'                    omit_dyad = omit_dyad)
#'getName(reh = edgelist_reh, actorID = c(0,12,16))
#' @export
getName <- function(reh, actorID = NULL, typeID = NULL, ...){
  UseMethod("getName")
}

#' @export
getName.reh <- function(reh, actorID = NULL, typeID = NULL, ...) {
  names <- NULL
  if((is.null(actorID) & is.null(typeID))) stop("Provide at least one actorID or typeID.")
  else{ 
    if(!is.null(actorID)){
      actors <- attr(reh, "dictionary")$actors
      which_actor <- sapply(actorID, function(x) which(actors$actorID == x))
      which_actor <- unlist(which_actor)
      names$actorName <- actors$actorName[which_actor]
      if(length(names$actorName) == 0) warning("No actorID was found in the dictionary")
      else if(length(names$actorName) < length(actorID)) warning("Some or all actorID's were not found in the dictionary.") 
    }
    if(!is.null(typeID)){
      types <- attr(reh, "dictionary")$types
      which_type <- sapply(typeID, function(x) which(types$typeID == x))
      which_type <- unlist(which_type)
      names$typeName <- types$typeName[which_type]
      if(length(names$typeName) == 0) warning("No typeID was found in the dictionary")
      else if(length(names$typeName) < length(typeID)) warning("Some or all typeID's were not found in the dictionary.")       
    }
  }
  return(names)
}

#' @export
dim.reh <- function(reh, ...){
  dimensions <- c(reh$M, reh$N, reh$C, reh$D)
  names(dimensions) <- c("events","actors","types","dyads")
  return(dimensions)
}


#######################################################################################
#######################################################################################
##########(END)             Methods for `reh` object             (END)#################
#######################################################################################
#######################################################################################




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