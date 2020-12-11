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



#' @export
summary.reh <- function(reh){
  print("this is a summary of an `reh` object")
}



#' @export
dim.reh <- function(reh){
  dimensions <- c(reh$M, reh$N, reh$C, reh$D)
  names(dimensions) <- c("events","actors","types","dyads")
  return(dimensions)
}



#' @export
print.reh <- function(reh){
  print(reh$edgelist)
}



#' @export
View.reh <- function(reh){
  View(reh$edgelist)
}



#' @export
head.reh <- function(reh, ...){
  head(reh$edgelist)
}



#' @export
tail.reh <- function(reh, ...){
  tail(reh$edgelist)
}



#' @title getRiskset
#' 
#' A function that returns the name of a vector of actors or types given their ID's
#' 
#' @param reh an reh object
#' @export
getRiskset <- function(reh){
  UseMethod("getRiskset")
}

#' @export
getRiskset.reh <- function(reh) {
  return(reh$rehBinary>=0) # only show actors at risk (-1 = FALSE, meaning to be omitted by the riskset on that time point)
}




#' @title actorName
#' A function that given a vector of actor ID's returns the corresponding vector of actor names
#' @param reh an reh object
#' @param actorID other arguments
#' @export
actorName <- function(reh, actorID = NULL){
  UseMethod("actorName")
}



#' @export
actorName.reh <- function(reh, actorID = NULL) {
  names <- NULL
  if(is.null(actorID)) stop("Provide at least one actorID.")
  else{ 
    if(!is.null(actorID)){
      actors <- attr(reh, "dictionary")$actors
      which_actor <- sapply(actorID, function(x) which(actors$actorID == x))
      which_actor <- unlist(which_actor)
      names <- actors$actorName[which_actor]
      if(length(names) == 0) stop("No actorID was found in the dictionary.")
      else if(length(names) < length(actorID)) warning("Some or all actorID's were not found in the dictionary.") 
    }
  }
  return(names)
}



#' @title typeName
#' A function that given a vector of type ID's returns the corresponding vector of type names
#' @param reh an reh object
#' @param typeID other arguments
#' @export
typeName <- function(reh, typeID = NULL){
  UseMethod("typeName")
}


#' @export
typeName.reh <- function(reh, typeID = NULL) {
  names <- NULL
  if(is.null(typeID)) stop("Provide at least one typeID.")
  else{ 
    if(!is.null(typeID)){
      types <- attr(reh, "dictionary")$types
      which_type <- sapply(typeID, function(x) which(types$typeID == x))
      which_type <- unlist(which_type)
      names <- types$typeName[which_type]
      if(length(names) == 0) stop("No typeID was found in the dictionary.")
      else if(length(names) < length(typeID)) warning("Some or all typeID's were not found in the dictionary.")       
    }
  }
  return(names)
}



#' @title actorID
#' A function that returns the actors's ID given the actors's name
#' @param reh an reh object
#' @param actorName other arguments
#' @export
actorID <- function(reh, actorName = NULL){
  UseMethod("actorID")
}

#' @export
actorID.reh <- function(reh, actorName = NULL) {
  IDs <- NULL
  if(is.null(actorName)) stop("Provide at least one actorName.")
  else{ 
    if(!is.null(actorName)){
      actors <- attr(reh, "dictionary")$actors
      which_actor <- sapply(actorName, function(x) which(actors$actorName == x))
      which_actor <- unlist(which_actor)
      IDs <- actors$actorID[which_actor]
      if(length(IDs) == 0) stop("No actorName was found in the dictionary.")
      else if(length(IDs) < length(actorName)) warning("Some or all actorName's were not found in the dictionary.") 
    }
  }
  return(IDs)
}


#' @title typeID
#' A function that returns the type's ID given they type's name
#' @param reh an reh object
#' @param typeName other arguments
#' @export
typeID <- function(reh, typeName = NULL){
  UseMethod("actorID")
}


#' @export
typeID.reh <- function(reh, typeName = NULL) {
  IDs <- NULL
  if(is.null(typeName)) stop("Provide at least one typeName.")
  else{ 
    if(!is.null(typeName)){
      types <- attr(reh, "dictionary")$types
      which_type <- sapply(typeName, function(x) which(types$typeName == x))
      which_type <- unlist(which_type)
      IDs <- types$typeID[which_type]
      if(length(IDs) == 0) stop("No typeName was found in the dictionary.")
      else if(length(IDs) < length(typeName)) warning("Some or all typeName's were not found in the dictionary.")       
    }
  }
  return(IDs)
}

#' @title dyad.info
#' A function that returns several information about the dyad specified according to the original names
#' @param reh an reh object
#' @param actor1 name of actor1
#' @param actor2 name of actor2
#' @param type name of type
#' @param info information to return. It can be a vector of times when the dyad occurred, or the vector of weights
#' @param begin first time index from which to consider events when computing the output (computing info only for a specific time window)
#' @param end last time index until which to consider events when computing the output (computing info only for a specific time window)
#' @export
dyad.info <- function(reh, actor1 = NULL, actor2 = NULL, type = NULL, info = NULL, begin = NULL, end = NULL){
  UseMethod("dyad.info")
}



#' @export
dyad.info.reh <- function(reh, actor1 = NULL, actor2 = NULL, type = NULL, info = NULL, begin = NULL, end = NULL){ #add start and stop
  # Checking input arguments :
  # ... info
  if(is.null(info)) info <- "summary"
  else if(!(info %in% c("time","weight","summary"))) stop("The argument `info` must be one of the following: `time`, `weight`, `summary`.")
  # ... [actor1,actor2,type]
  if(is.null(actor1) & is.null(actor2) & is.null(type)) stop("User must supply at least one of the three components of a dyad: `actor1`, `actor2`, `type`.")
  if(any(c(length(actor1)>1,length(actor2)>1,length(type)>1))) stop("Only one dyad at a time can be specified: `actor1`, `actor2` and `type` must have length = 1.")
  if(!is.null(actor1) & !is.null(actor2)){if(actor1 == actor2) stop("`actor1` and `actor2` cannot be the same actor.")}
  # ... [begin,end]
  if(is.null(begin) & is.null(end)) {begin <- 1; end <- reh$M}
  else{
    if(!is.null(begin) & !is.null(end)){
      if(begin>end) stop("`end` must be greater than `begin`.")
    }
    else{
      if(is.null(begin) & !is.null(end)) {begin <- 1}
      if(!is.null(begin) & is.null(end)) {end <- reh$M}
    }
  }
  if(begin<1) {begin <- 1; warning("Cannot input `begin < 1`, `begin` is set to 1.")}
  if(end>reh$M) {end <- M; warning(paste("Cannot input `end >",reh$M,"`, `end` is set to ",reh$M,"."))}

  # Converting input dyad according to ID's
  actor_names <- attr(reh,"dictionary")$actors$actorName 
  type_names <- attr(reh,"dictionary")$types$typeName

  actor1ID <- attr(reh,"dictionary")$actors$actorID[actor_names == actor1]+1
  actor2ID <- attr(reh,"dictionary")$actors$actorID[actor_names == actor2]+1
  typeID <- attr(reh,"dictionary")$types$typeID[type_names == type]+1

  # Checking for possible mistyping errors in the input actor1/actor2/type
  input_not_found <- NULL
  if(length(actor1ID)==0 & !is.null(actor1)) input_not_found <- paste(input_not_found,"  -> actor1 \n",sep="")
  if(length(actor2ID)==0 & !is.null(actor2)) input_not_found <- paste(input_not_found,"  -> actor2 \n",sep="")
  if(length(typeID)==0 & !is.null(type)) input_not_found <- paste(input_not_found,"  -> type \n",sep="")
  if(!is.null(input_not_found)) stop(paste("Could not find:",input_not_found,sep="\n"))

  # selecting indices
  if(is.null(actor1)){ #[NULL,?,?]
    if(is.null(actor2) & !is.null(type)){ #[NULL,NULL,type]
      indices_loc <- reh$risksetCube[,,typeID]
      indices <- c(indices_loc[upper.tri(indices_loc,diag=FALSE)],indices_loc[lower.tri(indices_loc,diag=FALSE)])
      if(info == "summary"){
        events_loc <- (reh$rehBinary[begin:end,]>=1)*1
        frequencies <- apply(events_loc,2,sum)
        summary_out <- matrix(0,nrow=reh$N,ncol=reh$N)
        summary_out[upper.tri(summary_out,diag=FALSE)] <- frequencies[indices_loc[upper.tri(indices_loc,diag=FALSE)]]
        summary_out[lower.tri(summary_out,diag=FALSE)] <- frequencies[indices_loc[lower.tri(indices_loc,diag=FALSE)]]
        rownames(summary_out) <- attr(reh, "dictionary")$actors$actorName
        colnames(summary_out) <- attr(reh, "dictionary")$actors$actorName
        return(summary_out)
      }
    }
    if(!is.null(actor2) & is.null(type)) # [NULL,actor2,NULL]
      indices <- as.vector(reh$risksetCube[-actor2ID,actor2ID,])
    if(!is.null(actor2) & !is.null(type)) # [NULL,actor2,type]
      indices <- as.vector(reh$risksetCube[-actor2ID,actor2ID,typeID])
  }
  else{ #[actor1,?,?]
    if(is.null(actor2) & is.null(type)) #[actor1,NULL,NULL]
      indices <- as.vector(reh$risksetCube[-actor1ID,actor1ID,]) 
    if(is.null(actor2) & !is.null(type)) #[actor1,NULL,type]
      indices <- as.vector(reh$risksetCube[-actor1ID,actor1ID,typeID])
    if(!is.null(actor2) & is.null(type)) # [actor1,actor2,NULL]
      indices <- as.vector(reh$risksetCube[actor1ID,actor2ID,])
    if(!is.null(actor2) & !is.null(type)) # [actor1,actor2,type]
      indices <- as.vector(reh$risksetCube[actor1ID,actor2ID,typeID])
  }

  # Considering only events
  events_loc <- (reh$rehBinary[begin:end,indices]>=1)*1
  events_loc <- apply(rbind(events_loc),1,sum)

  if(info == "time"){
    time <- attr(reh,"time")$value
    time <- time[which(events_loc == 1),]
    return(time)
  }

  if(info == "weight"){
    w <- reh$edgelist$weight
    w <- w[which(events_loc == 1)]
    return(w)
  }
  if(info == "summary"){
    # ...
    return(0)
  }
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