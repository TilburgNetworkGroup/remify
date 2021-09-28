#' @title reh
#'
#' @description A function that processes raw data and returns a 'reh' S3 object which is used as input in other functions in \code{remverse}.
#'
#' @param edgelist an object of class \code{"\link[base]{data.frame}"} or 
#' \code{"\link[base]{matrix}"} characterizing the relational event history sorted by 
#' time with columns 'time', 'actor1', 'actor2' and optionally 'type' and 
#' 'weight'.  
#' @param actors vector of actors that may be observed interacting in the network. If \code{NULL}, actor names will be drawn from the input edgelist.
#' @param types vector of event types that may occur in the network. If \code{NULL}, type names will be drawn from the input edgelist.
#' @param directed logical value indicating whether dyadic events are directed (\code{TRUE}) or undirected (\code{FALSE}).
#' @param ordinal  logical value indicating whether only the order of events matters in the model (\code{TRUE}) or also the waiting time must be considered in the model (\code{FALSE}).
#' @param origin time point since which when events could occur (default is \code{NULL}). If it is defined, it must have the same class of the time column in the input edgelist.
#' @param omit_dyad list of lists of two elements: `time`, that is a vector of the time points which to omit dyads from, `dyad`, which is a \code{"\link[base]{data.frame}"} where dyads to be omitted are supplied.
#'
#' @return  'reh' S3 object 
#'
#' @details for more details about inputs, outputs, attributes and methods of \code{remify::reh()}, see \code{vignette("reh")}.
#'
#' @export
reh <- function(edgelist,
                actors = NULL,
                types = NULL,
                directed = TRUE,
                ordinal = FALSE,
                origin = NULL,
                omit_dyad = NULL){

    # Make sure edgelist is a dataframe
    edgelist <- as.data.frame(edgelist)
     
    # Checking `edgelist$time` column
    if(!("time" %in% names(edgelist))) {
      stop("Edgelist should contain a column named 'time' with the timing/order information for the events.")
    }

    # Checking `edgelist$actor1` column
    if(!("actor1" %in% names(edgelist))) {
      stop("Edgelist should contain a column named 'actor1' with the first actors/senders of the events.")
    }

    # Checking `edgelist$actor2` column
    if(!("actor2" %in% names(edgelist))) {
      stop("Edgelist should contain a column named 'actor2' with the second actors/receivers of the events.")
    }

    

    # (2) Checking for `edgelist` columns (names and class of time variable)

    # (3) Checking for `origin` and `time` variable class (they must be the same)

    # (4) Checking for `riskset` object names 

    # (1) Checking for NA's 

    ## (1.1) NA's in `edgelist` :
    if(is.null(dim(edgelist)[1])) stop("The `edgelist` object is empty.")
	  if(anyNA(edgelist)) {
		warning("The `edgelist` object contains missing data: incomplete events (rows) are dropped.")
		to_remove <- which(is.na(edgelist), arr.ind = T)[,1]
		edgelist <- edgelist[-to_remove,]
    if(is.null(dim(edgelist)[1])) stop("The `edgelist` object is empty.")
    }

    # Pre-processing relational event history (rehCpp_new.cpp)
    out <- rehCpp_new(edgelist = edgelist,
                    actors = actors, 
                    types = types, 
                    directed = directed,
                    ordinal = ordinal,
                    origin = origin,
                    omit_dyad = omit_dyad)
  
    str_out <- structure(list(
      M = out$M,
      N = out$N,
      C = out$C,
      D = out$D,
      intereventTime = out$intereventTime,
      edgelist = out$edgelist,
      omit_dyad = out$omit_dyad
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

#' @title summary.reh
#' @description A function that returns a summary of the temporal network.
#' @param object is an \code{reh} object 
#' @param ... further arguments to be passed.
#' @export
summary.reh <- function(object,...){
  title <- "Relational Event Network:"
  events <- paste("\t> events = ",object$M,sep="")
  actors <- paste("\t> actors = ",object$N,sep="")
  types <- paste("\t> (event) types = ",object$C,sep="")
  riskset <- paste("\t> riskset = ",attr(object,"riskset"),sep="")
  directed <- paste("\t> directed = ",attr(object,"directed"),sep="")
  ordinal <- paste("\t> ordinal = ",attr(object,"ordinal"),sep="")
  weighted <- paste("\t> weighted = ",attr(object,"weighted"),sep="")
  time_length <- NULL
  if(!attr(object,"ordinal")){
    time_length_loc <- object$edgelist$time[object$M]
    if(is.null(attr(object,"time")$origin)){time_length_loc <- time_length_loc - (object$edgelist$time[1]-1)}
    else{time_length_loc <- time_length_loc - attr(object,"time")$origin}
    time_length <- paste("\t> time length ~ ",round(time_length_loc)," ",attr(time_length_loc, "units"),sep="")
  }

  interevent_time <- NULL
  if(!attr(object,"ordinal")){
    min_interevent_time <- min(object$intereventTime) 
    max_interevent_time <- max(object$intereventTime)
    units_minmax <- NULL # in case it is either numeric or integer
    if(class(object$edgelist$time)[1] == "Date"){ # is a Date (until days)
      units_minmax <- "days"   
    }
    else if(!is.numeric(object$edgelist$time) & !is.integer(object$edgelist$time)){ # is a timestamp (until seconds)
      units_minmax <- "seconds"
    }
    interevent_time <- paste("\t> interevent time \n\t\t >> minimum ~ ",round(min_interevent_time,4)," ",units_minmax,"\n\t\t >> maximum ~ ",round(max_interevent_time,4)," ",units_minmax,sep="")
  }

  cat(paste(title,events,actors,types,riskset,directed,ordinal,weighted,time_length,interevent_time,sep="\n"))
}

#######################################################################################
#######################################################################################

#' @title dim.reh
#' @description A function that returns the dimension of the temporal network.
#' @param x an \code{reh} object.
#' @rdname dim.reh
#' @export
dim.reh <- function(x){
  dimensions <- c(x$M, x$N, x$C, x$D)
  names(dimensions) <- c("events","actors","types","dyads")
  return(dimensions)
}

#######################################################################################
#######################################################################################


#' @title View.reh
#' @description A function that opens a view of the output edgelist in a new window. 
#' @param x an \code{reh} object.
#' @param title a title for the window. Default title is 'x'.
#' @export
View.reh <- function(x,title){
  View(x$edgelist)
}

#######################################################################################
#######################################################################################

#' @title print.reh
#' @description A function that prints out the output edgelist on the console.
#' @param x an \code{reh} object.
#' @param ... further arguments to be passed.
#' @export
print.reh <- function(x,...){
  print(x$edgelist)
}

#######################################################################################
#######################################################################################

#' @title head.reh
#' @description A function that returns the first 6 rows (by default) of the output edgelist.
#' @param x an \code{reh} object.
#' @param ... further arguments to be passed.
#' @export
head.reh <- function(x,...){
  head(x$edgelist)
}

#######################################################################################
#######################################################################################

#' @title tail.reh
#' @description A function that returns the last 6 rows (by default) of the output edgelist.
#' @param x an \code{reh} object.
#' @param ... further arguments to be passed.
#' @export
tail.reh <- function(x,...){
  tail(x$edgelist)
}

#######################################################################################
#######################################################################################

#' @title getRiskset
#' @description A function that returns a logical matrix where by row (time point), dyads that could occur at a specific time point assume value \code{TRUE}, \code{FALSE} otherwise.
#' @param reh an \code{reh} object.
#' @export
getRiskset <- function(reh){
  UseMethod("getRiskset")
}

#' @export
getRiskset.reh <- function(reh) {
  return(reh$omit_dyad$riskset) # only show dyads at risk. In other terms, those dyads that have value 0 (didn't occur but could) or 1 (occurred).
}

#######################################################################################
#######################################################################################

#' @title actorName
#' @description A function that given a vector of actor ID's returns the corresponding vector of actor (input) names.
#' @param reh an \code{reh} object.
#' @param actorID a vector of actor ID's.
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
      else if(length(names) < length(actorID)) warning("Some actorID was not found in the dictionary.") 
    }
  }
  return(names)
}

#######################################################################################
#######################################################################################

#' @title typeName
#' @description A function that given a vector of type ID's returns the corresponding vector of type (input) names.
#' @param reh an \code{reh} object.
#' @param typeID a vector of type ID's.
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
      else if(length(names) < length(typeID)) warning("Some typeID was not found in the dictionary.")       
    }
  }
  return(names)
}

#######################################################################################
#######################################################################################

#' @title actorID
#' @description A function that given a vector of actor names returns the corresponding vector of ID's.
#' @param reh an \code{reh} object.
#' @param actorName a vector of actor names.
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
      else if(length(IDs) < length(actorName)) warning("Some actorName was not found in the dictionary.") 
    }
  }
  return(IDs)
}

#######################################################################################
#######################################################################################

#' @title typeID
#' @description A function that given a vector of type names returns the corresponding vector of ID's.
#' @param reh an \code{reh} object.
#' @param typeName a vector of type names.
#' @export
typeID <- function(reh, typeName = NULL){
  UseMethod("typeID")
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
      else if(length(IDs) < length(typeName)) warning("Some typeName was not found in the dictionary.")       
    }
  }
  return(IDs)
}

#######################################################################################
#######################################################################################

#' @title dyad.count
#' @description A function that returns the count of a dyad (or a subset of dyads) that is specified according to the (original) actors and type names. The method allows for counting also on a restricted sequence of events by specifying arguments \code{begin} and/or \code{end}.
#' @param reh an \code{reh} object.
#' @param actor1 input name of actor1.
#' @param actor2 input name of actor2.
#' @param type input name of event type.
#' @param begin first time index from which to consider events when computing the output.
#' @param end last time index until which to consider events when computing the output.
#' @export
dyad.count <- function(reh, actor1 = NULL, actor2 = NULL, type = NULL, begin = NULL, end = NULL){
  UseMethod("dyad.count")
}

#' @export
dyad.count.reh <- function(reh, actor1 = NULL, actor2 = NULL, type = NULL, begin = NULL, end = NULL){ #add start and stop
  # Checking input arguments :
  
  # ... [actor1,actor2,type]
  if(is.null(actor1) & is.null(actor2) & is.null(type)) stop("User must supply at least one of the three components of a dyad: `actor1`, `actor2`, `type`.")
  if(any(c(length(actor1)>1,length(actor2)>1,length(type)>1))) stop("`actor1`, `actor2` and `type`, if specified, must have length = 1.")
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
  if(end>reh$M) {end <- reh$M; warning(paste("Cannot input `end >",reh$M,"`, `end` is set to ",reh$M,"."))}

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
#      indices_loc <- reh$risksetCube[,,typeID]
#      indices <- c(indices_loc[upper.tri(indices_loc,diag=FALSE)],indices_loc[lower.tri(indices_loc,diag=FALSE)])+1
#      events_loc <- (reh$rehBinary[begin:end,indices]>=1)*1
#      frequencies <- apply(events_loc,2,sum)
#      summary_out <- matrix(0,nrow=reh$N,ncol=reh$N)
#      summary_out[upper.tri(summary_out,diag=FALSE)] <- frequencies[indices_loc[upper.tri(indices_loc,diag=FALSE)]]
#      summary_out[lower.tri(summary_out,diag=FALSE)] <- frequencies[indices_loc[lower.tri(indices_loc,diag=FALSE)]]
#      rownames(summary_out) <- attr(reh, "dictionary")$actors$actorName
#      colnames(summary_out) <- attr(reh, "dictionary")$actors$actorName
#      return(summary_out)
    }
    if(!is.null(actor2) & is.null(type)){ # [NULL,actor2,NULL]
#      indices <- as.vector(reh$risksetCube[-actor2ID,actor2ID,])+1
#      events_loc <- (reh$rehBinary[begin:end,indices]>=1)*1
#      events_loc <- apply(events_loc,2,sum)
#      frequencies <- matrix(events_loc,nrow=(reh$N-1),ncol=reh$C,byrow=FALSE) 
#      rownames(frequencies) <- attr(reh, "dictionary")$actors$actorName[-actor2ID]
#      colnames(frequencies) <- attr(reh, "dictionary")$types$typeName
#      return(frequencies)
    }
    if(!is.null(actor2) & !is.null(type)){ # [NULL,actor2,type]
#      indices <- as.vector(reh$risksetCube[-actor2ID,actor2ID,typeID])+1
#      events_loc <- (reh$rehBinary[begin:end,indices]>=1)*1
#      events_loc <- apply(events_loc,2,sum)
#      frequencies <- matrix(events_loc,nrow=(reh$N-1),ncol=1,byrow=FALSE) 
#      rownames(frequencies) <- attr(reh, "dictionary")$actors$actorName[-actor2ID]
#      colnames(frequencies) <- attr(reh, "dictionary")$types$typeName[typeID]
#      return(frequencies) 
    }    
  }
  else{ #[actor1,?,?]
    if(is.null(actor2) & is.null(type)){ #[actor1,NULL,NULL]
#      indices <- as.vector(reh$risksetCube[actor1ID,-actor1ID,])+1
#      events_loc <- (reh$rehBinary[begin:end,indices]>=1)*1
#      events_loc <- apply(events_loc,2,sum)
#      frequencies <- matrix(events_loc,nrow=(reh$N-1),ncol=reh$C,byrow=FALSE) 
#      rownames(frequencies) <- attr(reh, "dictionary")$actors$actorName[-actor1ID]
#      colnames(frequencies) <- attr(reh, "dictionary")$types$typeName
#      return(frequencies)             
    }
    if(is.null(actor2) & !is.null(type)){ #[actor1,NULL,type]
#      indices <- as.vector(reh$risksetCube[actor1ID,-actor1ID,typeID])+1
#      events_loc <- (reh$rehBinary[begin:end,indices]>=1)*1
#      events_loc <- apply(events_loc,2,sum)
#      frequencies <- matrix(events_loc,nrow=(reh$N-1),ncol=1,byrow=FALSE) 
#      rownames(frequencies) <- attr(reh, "dictionary")$actors$actorName[-actor1ID]
#      colnames(frequencies) <- attr(reh, "dictionary")$types$typeName[typeID]
#      return(frequencies)            
    }
    if(!is.null(actor2) & is.null(type)){ # [actor1,actor2,NULL]
#      indices <- as.vector(reh$risksetCube[actor1ID,actor2ID,])+1
#      events_loc <- cbind((reh$rehBinary[begin:end,indices]>=1)*1)
#      events_loc <- apply(events_loc,2,sum)
#      frequencies <- matrix(events_loc,nrow=reh$C,ncol=1,byrow=FALSE) 
#      rownames(frequencies) <- attr(reh, "dictionary")$types$typeName
#      colnames(frequencies) <- c(paste(actor1,actor2,sep="->"))
#      return(frequencies)            
    }
    if(!is.null(actor2) & !is.null(type)){ # [actor1,actor2,type]
#      indices <- as.vector(reh$risksetCube[actor1ID,actor2ID,typeID])+1
#      events_loc  <- (reh$rehBinary[begin:end,indices]>=1)*1
#      frequencies <- sum(events_loc)
#      return(frequencies)  
    }
  }
  
  return(NULL)
}

#######################################################################################
#######################################################################################
##########(END)             Methods for `reh` object             (END)#################
#######################################################################################
#######################################################################################


#' @title remify 
#'
#' @description A function that transforms a \code{reh} object into one of the possible formats that suits external packages.
#'
#' @param input an input argument for the function \code{remify()}.
#'
#' @return  otuput of \code{remify()}.
#' @export
remify <- function(input){
                  # [...] 
                }                   