#' @title reh
#'
#' @description A function that processes raw data and returns a 'reh' S3 object which is used as input in other functions in \code{remverse}.
#'
#' @param edgelist an object of class \code{"\link[base]{data.frame}"} or 
#' \code{"\link[base]{matrix}"} characterizing the relational event history sorted by 
#' time with columns `time`, `actor1`, `actor2` and optionally `type` and 
#' `weight`.  
#' @param actors vector of actors that may be observed interacting in the network. If \code{NULL}, actor names will be taken from the input edgelist.
#' @param types vector of event types that may occur in the network. If \code{NULL}, type names will be taken from the input edgelist.
#' @param directed logical value indicating whether dyadic events are directed (\code{TRUE}) or undirected (\code{FALSE}).
#' @param ordinal  logical value indicating whether only the order of events matters in the model (\code{TRUE}) or also the waiting time must be considered in the model (\code{FALSE}).
#' @param origin time point since when events could occur (default is \code{NULL}). If it is defined, it must have the same class of the time column in the input edgelist.
#' @param omit_dyad list of lists. Each list must have two objects: a first object named `time`, that is a vector of two values defining the first and last time point of the time window where to apply the change to the risk set and a second object, named `dyad`, which is a \code{"\link[base]{data.frame}"} where dyads to be removed are supplied in the format \code{actor1,actor2,type} (by row).
#' @param model can be "tie" or "actor" oriented modeling. This argument plays a fundamental role when \code{omit_dyad} is supplied. Indeed, when actor-oriented modeling, the dynamic risk set will consist of two risk sets objects (senders' and dyads' risk sets). In the tie-oriented model the function will return a dynamic risk set referred at a dyad-level.
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
                omit_dyad = NULL,
                model = c("tie","actor")){

    # Make sure edgelist is a data.frame
    edgelist <- as.data.frame(edgelist)
     
    # Checking `edgelist$time` column
    if(!("time" %in% names(edgelist))){
      stop("`edgelist` should contain a column named `time` with the timing/order information for the events.")
    }

    # Checking `edgelist$actor1` column
    if(!("actor1" %in% names(edgelist))){
      stop("`edgelist` should contain a column named `actor1` with the first actors/senders of the events.")
    }

    # Checking `edgelist$actor2` column
    if(!("actor2" %in% names(edgelist))){
      stop("`edgelist` should contain a column named `actor2` with the second actors/receivers of the events.")
    }

    # checking input argument "model" :
    if(is.null(model) || all(model==c("tie","actor"))) {
        model <- "tie"
        warning("argument `model` set to `tie` by default")
    }
    if(!is.null(model) & !(model %in% c("tie","actor"))) stop("argument `model` must be set to either `tie` or `actor`")

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
    out <- rehCpp(edgelist = edgelist,
                    actors = actors, 
                    types = types, 
                    directed = directed,
                    ordinal = ordinal,
                    origin = origin,
                    omit_dyad = omit_dyad,
                    model = model)
  
    str_out <- structure(list(
      M = out$M,
      N = out$N,
      C = out$C,
      D = out$D,
      intereventTime = out$intereventTime,
      edgelist = data.matrix(out$edgelist),
      omit_dyad = out$omit_dyad
    ), class="reh")

   
    
    attr(str_out, "with_type") <- out$with_type
    attr(str_out, "weighted") <- out$weighted
    attr(str_out, "directed") <- directed
    attr(str_out, "ordinal") <- ordinal
    attr(str_out, "model") <- model # useful because tie and actor models have two different ways for handling changing risksets
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
#' @rdname summary.reh
#' @description A function that returns a summary of the event history.
#' @param object is an \code{reh} object 
#' @param ... further arguments to be passed.
#' @method summary reh
#' @export
summary.reh <- function(object,...){
  title <- "Relational Event Network"
  model <- paste("(processed for ",attr(object,"model"),"-oriented modeling):",sep="")
  events <- paste("\t> events = ",object$M,sep="")
  actors <- paste("\t> actors = ",object$N,sep="")
  types <- paste("\t> (event) types = ",object$C,sep="")
  riskset <- paste("\t> riskset = ",attr(object,"riskset"),sep="")
  directed <- paste("\t> directed = ",attr(object,"directed"),sep="")
  ordinal <- paste("\t> ordinal = ",attr(object,"ordinal"),sep="")
  weighted <- paste("\t> weighted = ",attr(object,"weighted"),sep="")
  time_length <- NULL
  if(!attr(object,"ordinal")){
    time_length_loc <- attr(object,"time")$value$time[object$M] 
    if(is.null(attr(object,"time")$origin)){time_length_loc <- time_length_loc - (attr(object,"time")$value$time[1]-1)}
    else{time_length_loc <- time_length_loc - attr(object,"time")$origin}
    time_length <- paste("\t> time length ~ ",round(time_length_loc)," ",attr(time_length_loc, "units"),sep="")
  }

  interevent_time <- NULL
  if(!attr(object,"ordinal")){
    min_interevent_time <- min(object$intereventTime) 
    max_interevent_time <- max(object$intereventTime)
    units_minmax <- NULL # in case it is either numeric or integer
    if((length(attr(object,"time")$class)==1) & (attr(object,"time")$class[1] == "Date")){ # is a Date (until days)
      units_minmax <- "days"   
    }
    else if(!is.numeric(attr(object,"time")$value$time) & !is.integer(attr(object,"time")$value$time)){ # is a timestamp (until seconds)
      units_minmax <- "seconds"
    }
    interevent_time <- paste("\t> interevent time \n\t\t >> minimum ~ ",round(min_interevent_time,4)," ",units_minmax,"\n\t\t >> maximum ~ ",round(max_interevent_time,4)," ",units_minmax,sep="")
  }

  cat(paste(title,model,events,actors,types,riskset,directed,ordinal,weighted,time_length,interevent_time,sep="\n"))
}

#######################################################################################
#######################################################################################

#' @title dim.reh
#' @rdname dim.reh
#' @description A function that returns the dimension of the temporal network.
#' @param x an \code{reh} object.
#' @method dim reh
#' @export
dim.reh <- function(x){
  dimensions <- c(x$M, x$N, x$C, x$D)
  names(dimensions) <- c("events","actors","types","dyads")
  return(dimensions)
}

#######################################################################################
#######################################################################################

#' @title getDynamicRiskset
#' @description This function returns a matrix describing the possible risk set changes specified by the input `omit_dyad`. In such a matrix: value 1 refers to the dyads in the risk set, and 0 otherwise (dyads excluded from the risk set). All the possible risk set modifications are described by row, and the columns identify the dyads. Note: This matrix is the output given by processing the input `omit_dyad`, and the number of rows might be equal to or higher than the number of objects in `omit_dyad`. This might happen because more than one modification of the risk set defined in the input could overlap over time with others. 
#' @param reh an \code{reh} object.
#' @export
getDynamicRiskset <- function(reh){
  UseMethod("getDynamicRiskset")
}

#' @describeIn getDynamicRiskset dynamic riskset object
#' @method getDynamicRiskset reh
#' @export
getDynamicRiskset.reh <- function(reh) {
  if(attr(reh, "riskset") == "dynamic"){
    if(attr(reh,"model") == "tie"){
      return(list(riskset = reh$omit_dyad$riskset))
      }
    else if(attr(reh,"model") == "actor"){
      return(list(sender = reh$omit_dyad$risksetSender, dyad = reh$omit_dyad$riskset)) 
    }
  }
  else{
    stop("risk set is not dynamic")
  }
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

#' @describeIn actorName actor's name from actor's ID
#' @method actorName reh
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

#' @describeIn typeName type's name from type's ID
#' @method typeName reh
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

#' @describeIn actorID actor's ID from actor's name
#' @method actorID reh
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

#' @describeIn typeID type's ID from type's name
#' @method typeID reh
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
##########(END)             Methods for `reh` object             (END)#################
#######################################################################################
#######################################################################################


#' @title remify 
#'
#' @description A function that transforms a \code{reh} object into one of the possible formats that suit external packages, or vice versa.
#'
#' @param input an input argument for the function \code{remify()}.
#'
#' @return  otuput of \code{remify()}.
#' @export
remify <- function(input){
                  # [...] 
                }                   