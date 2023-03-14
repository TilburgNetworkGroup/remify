#' @title Process a Relational Event History
#'
#' @description A function that processes raw data and returns a 'reh' S3 object which is used as input in other functions in \code{remverse}.
#'
#' @param edgelist an object of class \code{\link[base]{data.frame}} characterizing the relational event history sorted by 
#' time with columns named `time`, `actor1`, `actor2` and optionally `type` and 
#' `weight`.  
#' @param actors character vector of actors' names that may be observed interacting in the network. If \code{NULL}, actor names will be taken from the input edgelist.
#' @param types character vector of event types that may occur in the network. If \code{NULL}, type names will be taken from the input edgelist.
#' @param directed logical value indicating whether dyadic events are directed (\code{TRUE}) or undirected (\code{FALSE}).
#' @param ordinal  logical value indicating whether only the order of events matters in the model (\code{TRUE}) or also the waiting time must be considered in the model (\code{FALSE}).
#' @param origin time point since when events could occur (default is \code{NULL}). If it is defined, it must have the same class of the time column in the input edgelist.
#' @param omit_dyad list of lists. Each list refers to one risk set modification and must have two objects: a first object named `time`, that is a vector of two values defining the first and last time point of the time window where to apply the change to the risk set and a second object, named `dyad`, which is a \code{\link[base]{data.frame}} where dyads to be removed are supplied in the format \code{actor1,actor2,type} (by row). The \code{NA} value can be used to remove multiple objects from the risk set at once with one risk set modification list (see Details).
#' @param model can be "tie" or "actor" oriented modeling. This argument plays a fundamental role when \code{omit_dyad} is supplied. Indeed, when actor-oriented modeling, the dynamic risk set will consist of two risk sets objects (senders' and dyads' risk sets). In the tie-oriented model the function will return a dynamic risk set referred at a dyad-level.
#'
#' @return  'reh' S3 object 
#'
#' @details In \code{omit_dyad}, the \code{NA} value can be used to remove multiple objects from the risk set at once with one risk set modification list. For example, to remove all events with sender equal to actor “A” add a list with two objects \code{time = c(NA, NA)} and \code{dyad = data.frame(actor1 = A, actor2 = NA, type = NA)} to the \code{omit_dyad} list.
#' 
#' For more details about the \code{omit_dyad} argument, inputs, outputs, attributes and methods of \code{remify::reh()}, see \code{vignette("reh")}. 
#'
#' @export
reh <- function(edgelist,
                actors = NULL,
                types = NULL,
                directed = TRUE,
                ordinal = FALSE,
                origin = NULL,
                omit_dyad = NULL,
                model = c("tie","actor") #,
                #[[to work on]] time = c("seconds","minutes","hours","days","weeks","months") this input will process the intervent time to different time measures
                ){
    
    # (1) Checking for 'edgelist' input object

    # Make sure edgelist is a data.frame
    if(!is.data.frame(edgelist)){
      stop("input `edgelist` must be of class `data.frame`.")
    }

    # (2) Checking for `edgelist` columns (names and class of time variable)
     
    # Checking `edgelist$time` column
    if(!("time" %in% names(edgelist))){
      stop("`edgelist` should contain a column named `time` with the timing/order information for the events.")
    }
    if(!(class(edgelist$time)[1] %in% c("numeric","integer","Date","POSIXct"))){
      stop("the class of column`time` must be one of the following types: numeric, integer, Date or POSIXct")
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
    if(!is.null(model) & !(model %in% c("tie","actor"))) stop("argument `model` must be set to either `tie` or `actor`.")

    # (3) Checking for time variable classes (they must be the same)

    # input `origin` and `time` column in `edgelist`
    if(!is.null(origin)){
      if(any(class(origin) != class(edgelist$time)))
        stop("the class of input `origin` and the class of `edgelist$time` must be the same.")
    }

    # input `omit_dyad` and `time` column in `edgelist`
    if(!is.null(omit_dyad)){
      if(!is.list(omit_dyad)){
        stop("the input `omit_dyad` must be a list. Check vignette(topic = 'reh', package= 'remify') for more information.")
      }
      else{
        obj_names_check <- unlist(lapply(omit_dyad,function(x) sort(names(x))==c("dyad","time")))
        if(all(obj_names_check)){
          class_time_check <- unlist(lapply(omit_dyad, function(x) all(class(x$time) == class(edgelist$time))))
          if(!all(class_time_check)){
            stop("the class of the time specified in `omit_dyad` and the class of `edgelist$time` must be the same.")
          }
        }
        else{
          stop("the input `omit_dyad` must be a collection of lists with two named objects: `dyad` and `time`. Check vignette(topic = 'reh', package= 'remify') for more information.")
        }
      }
    }

    # (1) Checking for NA's 

    ## (1.1) NA's in `edgelist` :
	  if(anyNA(edgelist)) {
		warning("`edgelist` contains missing data: incomplete events are dropped.") # `edgelist` contains missing data: incomplete events (rows) are dropped.
		to_remove <- unique(which(is.na(edgelist), arr.ind = T)[,1])
		edgelist <- edgelist[-to_remove,]
    #[[to check]]if(is.null(dim(edgelist)[1])) stop("The `edgelist` object is empty.")
    }

    # Pre-processing relational event history (rehCpp.cpp)
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
      edgelist = out$edgelist, #[[to remove]] it was a data.matrix()
      omit_dyad = out$omit_dyad
    ), class="reh")

   
    
    attr(str_out, "with_type") <- out$with_type
    attr(str_out, "weighted") <- out$weighted
    attr(str_out, "directed") <- directed
    attr(str_out, "ordinal") <- ordinal
    attr(str_out, "model") <- model # useful because tie and actor models have two different ways for handling changing risksets
    attr(str_out, "riskset") <- ifelse(length(omit_dyad)>0,"dynamic","static")
    attr(str_out, "dictionary") <- list(actors = out$actorsDictionary, types = out$typesDictionary) 
    attr(str_out, "time") <- list(class= class(out$edgelist$time), value = data.frame(time = out$edgelist$time,intereventTime = out$intereventTime), origin = origin)

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
  time <- object$edgelist$time
  origin <- attr(object, "time")$origin
  if(!attr(object,"ordinal")){
    if(is.null(origin)){
      origin <- min(time) - 1
      if(origin < 0) origin <- 0
    }
    else if( origin >= min(time)){
      origin <- min(time) - 1
      if(origin < 0) origin <-0
    }
    time_length <- time[object$M] - origin
    time_length <- paste("\t> time length ~ ",round(time_length)," ",attr(time_length, "units"),sep="")
  }

  interevent_time <- NULL
  if(!attr(object,"ordinal")){
    min_interevent_time <- min(object$intereventTime) 
    max_interevent_time <- max(object$intereventTime)
    units_minmax <- NULL # in case it is either numeric or integer
    if(inherits(time,"Date")){ # is a Date (until days)
      units_minmax <- "days"   
    }
    else if(!is.numeric(time) & !is.integer(time)){ # is a timestamp (until seconds) #[[to check]] it will change based on the new input where the user can define the interevent time to be scaled in seconds, minutes, hours etc..
      units_minmax <- "seconds"
    }
    interevent_time <- paste("\t> interevent time \n\t\t >> minimum ~ ",round(min_interevent_time,4)," ",units_minmax,"\n\t\t >> maximum ~ ",round(max_interevent_time,4)," ",units_minmax,"\n",sep="")
  }

  cat(paste(title,model,events,actors,types,riskset,directed,ordinal,weighted,time_length,interevent_time,sep="\n"))
}

#######################################################################################
#######################################################################################

#' @title print.reh
#' @rdname print.reh
#' @description print a summary of the event history.
#' @param x is an \code{reh} object 
#' @param ... further arguments to be passed.
#' @method print reh
#' @export
print.reh <- function(x,...){
  summary(object=x,...)
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
  if(is.null(actorID)) stop("provide at least one actorID.")
  else{ 
    if(!is.null(actorID)){
      actors <- attr(reh, "dictionary")$actors
      which_actor <- sapply(actorID, function(x) which(actors$actorID == x))
      which_actor <- unlist(which_actor)
      names <- actors$actorName[which_actor]
      if(length(names) == 0) stop("no actorID was found in the dictionary.")
      else if(length(names) < length(actorID)) warning("some actorID was not found in the dictionary.") 
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
  if(is.null(typeID)) stop("provide at least one typeID.")
  else{ 
    if(!is.null(typeID)){
      types <- attr(reh, "dictionary")$types
      which_type <- sapply(typeID, function(x) which(types$typeID == x))
      which_type <- unlist(which_type)
      names <- types$typeName[which_type]
      if(length(names) == 0) stop("no typeID was found in the dictionary.")
      else if(length(names) < length(typeID)) warning("some typeID was not found in the dictionary.")       
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
  if(is.null(actorName)) stop("provide at least one actorName.")
  else{ 
    if(!is.null(actorName)){
      actors <- attr(reh, "dictionary")$actors
      which_actor <- sapply(actorName, function(x) which(actors$actorName == x))
      which_actor <- unlist(which_actor)
      IDs <- actors$actorID[which_actor]
      if(length(IDs) == 0) stop("no actorName was found in the dictionary.")
      else if(length(IDs) < length(actorName)) warning("some actorName was not found in the dictionary.") 
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
  if(is.null(typeName)) stop("provide at least one typeName.")
  else{ 
    if(!is.null(typeName)){
      types <- attr(reh, "dictionary")$types
      which_type <- sapply(typeName, function(x) which(types$typeName == x))
      which_type <- unlist(which_type)
      IDs <- types$typeID[which_type]
      if(length(IDs) == 0) stop("no typeName was found in the dictionary.")
      else if(length(IDs) < length(typeName)) warning("some typeName was not found in the dictionary.")       
    }
  }
  return(IDs)
}

#######################################################################################
#######################################################################################

#' @title dyadID
#' @description A function that given a vector of names as to actor1, actor2 and type returns the corresponding dyad ID. The names to supply are the original input names of the edgelist before the processing via the function \code{remify::reh()}
#' @param reh an \code{reh} object.
#' @param actor1 name of actor1 
#' @param actor2 name of actor2 
#' @param type name of type
#' @export
dyadID <- function(reh, actor1, actor2, type){
  UseMethod("dyadID")
}

#' @describeIn dyadID dyadID from dyad composition
#' @method dyadID reh
#' @export
dyadID.reh <- function(reh, actor1, actor2, type) {
  if(!inherits(reh,"reh")){
    stop("the input argument 'reh' must be an object of class 'reh'") #[IMPORTANT] add this check in all the other methods and also perform tests on it
  }
  if(attr(reh,"with_type")){
    if(!is.vector(type) | (length(type)>1)){
      stop("the input argument 'type' must be a character vector of length 1")
    }
    type <- as.character(type)
  }
  if((!is.vector(actor1) | !is.vector(actor2)) | ((length(actor1)>1) | (length(actor2)>1))){
    stop("the input arguments 'actor1' and 'actor2' must be character vectors of length 1")
  }
  actor1 <- as.character(actor1)
  actor2 <- as.character(actor2)

  # check on self-event
  if((actor1==actor2)){ ## add the following condition "!attr(reh,"self-event") & " when remify will support self-events
    stop("the input 'reh' does not support self-events. 'actor1' and 'actor2' must be different")
  }

  dict_loc <- attr(reh,"dictionary")
  # finding actors from the dictionary of names (attribute of the reh object)
  actor1_which <- which(dict_loc$actors$actorName == actor1)
  actor2_which <- which(dict_loc$actors$actorName == actor2)
  check_on_actors <- (c(length(actor1_which),length(actor2_which))==0) 
  if(any(check_on_actors)){
    stop(paste("input ",ifelse(sum(check_on_actors)==1,c("'actor1' "," 'actor2' ")[check_on_actors],c("'actor1' and 'actor2' ")),"not found in the processed object 'reh'",sep=""))
  }
  actor1_id <- dict_loc$actors$actorID[actor1_which]
  actor2_id <- dict_loc$actors$actorID[actor2_which]
  # finding type from the dictionary of names (attribute of the reh object)
  type_id <- 0
  if(attr(reh,"with_type")){
    type_which <- which(dict_loc$types$typeName == type)
    if(length(type_which)==0){
      stop("input 'type' not found in the processed object 'reh'")
    }
    type_id <- dict_loc$types$typeID[type_which]
  }
  # finding dyad ID
  dyad_id <- remify:::getDyadIndex(actor1 = actor1_id,
                                    actor2 = actor2_id,
                                    type = type_id,
                                    N = reh$N,
                                    directed = attr(reh,"directed")) # we return the id in the C++ form -- at the moment --

  return(dyad_id)
}

#######################################################################################
#######################################################################################

#' @title dyad
#' @description A function that given a vector of one or more dyad ID's returns the corresponding dyad composition of "actor1", "actor2" and "type" (if event types are present). The ID's to supply must range between 1 and D (largest risk set size).
#' @param reh an \code{reh} object.
#' @param dyadID a vector of one or more dyad ID's, each one ranging from 1 to D (largest risk set size)
#' @export
dyad <- function(reh, dyadID){
  UseMethod("dyad")
}

#' @describeIn dyad dyad composition in actor1, actor2 and type from dyad ID
#' @method dyad reh
#' @export
dyad.reh <- function(reh, dyadID) {
  if(!inherits(reh,"reh")){
    stop("the input argument 'reh' must be an object of class 'reh'") #[IMPORTANT] add this check in all the other methods and also perform tests on it
  }
  if(!is.numeric(dyadID) & !is.integer(dyadID)){
    stop("input argument 'dyadID' must be a numeric (or integer) vector")
  }
  out <- NULL
  dyadID <- as.integer(dyadID) # if the ID supplied is 124.8, the ID considered will be 124
  if(length(dyadID)==1){
    # the output will be a character vector
    if((dyadID < 1) | (dyadID > reh$D)){
      stop(paste("input argument 'dyadID' must range between 1 and ",reh$D,", given that the size of the largest risk set is ",reh$D,sep="")) 
    }
    out <- remify:::getDyadComposition(d = dyadID-1, C = reh$C, N = reh$N, D = reh$D)
    if(attr(reh,"with_type")){
      out <- c(dyadID,out)
      names(out) <- c("dyadID","actor1","actor2","type")
    }
    else{
      out <- c(dyadID,out[-3]) # excluding the base event type (if there is only one event type in the sequence)
      names(out) <- c("dyadID","actor1","actor2")
    }
  }
  else if(length(dyadID)>1){
    # the output will be a data.frame

    # check for duplicates in dyadID
    length_orig <- length(dyadID)
    dyadID <- unique(dyadID)
    if(length_orig > length(dyadID)){
      warning("input argument 'dyadID' contains ID's that are repeated more than once. Such ID's will be processed once")
    }

    # apply function remify:::getDyadComposition()
    dict_loc <- attr(reh,"dictionary")
    if(attr(reh,"with_type")){ # output with 'type' column
      actor1_name <- actor2_name <- type_name <- rep(NA, length=length(dyadID))
      for(d in 1:length(dyadID)){
        if((dyadID[d] < 1) | (dyadID[d] > reh$D)){
          stop(paste("input argument 'dyadID' must range between 1 and ",reh$D,", given that the size of the largest risk set is ",reh$D,sep=""))
        }
        dyad_composition_loc <- remify:::getDyadComposition(d = dyadID[d]-1, C = reh$C, N = reh$N, D = reh$D)
        actor1_name[d] <- dict_loc$actors$actorName[dyad_composition_loc[1]+1]
        actor2_name[d] <- dict_loc$actors$actorName[dyad_composition_loc[2]+1]
        type_name[d] <- dict_loc$types$typeName[dyad_composition_loc[3]+1]
        rm(dyad_composition_loc)
      }
      out <- data.frame(dyadID = dyadID, actor1 = actor1_name, actor2 = actor2_name, type = type_name)  
      rm(actor1_name,actor2_name,type_name)   
    }
    else{ # output without 'type' column (for sequences with one or none event type)
      actor1_name <- actor2_name <- rep(NA, length=length(dyadID))
      for(d in 1:length(dyadID)){
        if((dyadID[d] < 1) | (dyadID[d] > reh$D)){
          stop(paste("input argument 'dyadID' must range between 1 and ",reh$D,", givent that the size of the largest risk set is ",reh$D,sep=""))
        }
        dyad_composition_loc <- remify:::getDyadComposition(d = dyadID[d]-1, C = reh$C, N = reh$N, D = reh$D)
        actor1_name[d] <- dict_loc$actors$actorName[dyad_composition_loc[1]+1]
        actor2_name[d] <- dict_loc$actors$actorName[dyad_composition_loc[2]+1]
        rm(dyad_composition_loc)
      }
      out <- data.frame(dyadID = dyadID, actor1 = actor1_name, actor2 = actor2_name)  
      rm(actor1_name,actor2_name)     
    }
  }
  return(out)
}

#######################################################################################
#######################################################################################
##########(END)             Methods for `reh` object             (END)#################
#######################################################################################
#######################################################################################


#' @title Transform processed relational event sequences to different formats
#'
#' @description A function that transforms a \code{reh} object into one of the possible formats that suit external packages, and vice versa. The function can convert, at the moment, the data structure from (to) an object of class \code{reh} to (from) a data structure required by the function \code{relevent::rem()} from the \href{https://CRAN.R-project.org/package=relevent}{relevent} package (Butts, C.T. 2023).
#'
#' @param data an object of either class 'reh' (see function \code{remify::reh()}) or class 'relevent'. The class 'relevent' is an dummy class object that contains a list of objects named after the argument names of the function \code{relvent::rem()} that need to be converted. For instance, if one wants to convert an object of structure 'relevent' we need it to contain: 'eventlist' (mandatory), 'supplist' (optional), 'timing'(mandatory). If the object 'timing' is \code{NULL}, the output object will assume an \code{"interval"} timing. The 'supplist' object can be left uspecified (\code{NULL}).
#' @param output_format a character indicating the output format which the input data has to be converted to. It can assume two values: "reh" , "relevent"
#'
#' @return  an object of class specified by the \code{format} argument and containing the converted objects according to the required format
#' @export
rehshape <- function(data, output_format = c("reh","relevent")){

    output_format <- match.arg(output_format)
    data_format <- class(data)
    if(length(data_format)>1){
      stop("class of input data must be of length 1.")
    }
    # data has to be either of class 'reh' or 'relevent'
    if(!any(data_format == c("reh","relevent"))){
      stop("class of input data must be either `reh` or `relevent`.") 
    }
    # check data and output format
    if(data_format == output_format){
      warning("the format of the input data is the same as the required output format. The input data is returned.")
      return(data)
    }
    
    # if data structure is 'reh'
    if(data_format ==  "reh"){

      # check reh object here
      ## ##
      ## ## ##
      # stop('') + add tests
      ## ##

      out <- NULL
      if(output_format == "relevent"){
        # (1) processing the edgelist
        eventlist <- data$edgelist[,c(2,1)] # [dyad,time]
        eventlist[,1] <- eventlist[,1]+1
        if(is.null(attr(data,"time")$origin)){
          eventlist[,2] <- attr(data,"time")$value[,1] # if 'origin' is NULL the we use the time column,
        }
        else{
          eventlist[,2] <-cumsum(data$intereventTime) # if 'origin' is provided inside object 'reh', then the time variable is reconstructed via cumulative sum of intervent time variable 
        }
    
        colnames(eventlist) <- c("dyad","time")
        supplist <- NULL
        if(!is.null(data$omit_dyad)){
          # (2) converting the omit_dyad output object to the 'supplist' argument in relevent::rem()
          supplist <- matrix(TRUE,nrow=data$M,ncol=data$D)
          for(m in 1:data$M){
            if(data$omit_dyad$time[m]!=(-1)){
              change_m <- data$omit_dyad$riskset[data$omit_dyad$time[m]+1,]
              supplist[m,] <- as.logical(change_m)
              rm(change_m)
            }
          }
        }
        # (3) processing information about likelihood
        timing <- ifelse(attr(data,"ordinal"),"ordinal","interval")
        out <- structure(list(eventlist = eventlist,
                              supplist = supplist,
                              timing = timing),
                        class = "relevent")
      }
      return(out)
    }


    if(data_format == "relevent"){

      # check relevent object here
      ## ##
      ## ## ##
      # stop('') + add tests
      ## ##

      out <- NULL
      #convert from 'relevent' structure to 'reh'

      # full riskset (this will have different actors' names than the original data)
      dyads_l <- expand.grid(1:data$N,1:data$N) # number of actors 
      dyads_l <- dyads_l[-which(dyads_l[,1]==dyads_l[,2]),]
      dyads_l <- dyads_l[,c(2,1)] 
      dyads_l <- data.frame(actor1=rep(dyads_l[,1],data$C),actor2=rep(dyads_l[,2],data$C),type=rep(1:data$C,each=data$N*(data$N-1))) # number of event types

      # edgelist converted to [actor1,actor2,type]
      data$M <- dim(data$eventlist)[1]
      edgelist_orig <- data.frame(time = data$eventlist[,2], actor1 = rep(NA,data$M), actor2 = rep(NA,data$M), type = rep(NA,data$M))
      for(m in 1:data$M){
        edgelist_orig[m,2:4] <- dyads_l[data$eventlist[m,1],]
      }

      # convert supplist to omit_dyad (it remains NULL if supplist is NULL or has full riskset over all the time points)
      converted_omit_dyad <- NULL
      if(!is.null(data$supplist)){
        check_changing_riskset <- sum(data$supplist)
        if(check_changing_riskset < (data$M*data$N*(data$N-1)*data$C)){
          converted_omit_dyad <- list()
          converted_omit_dyad$riskset <- (rbind(unique(data$supplist)[-1,]))*1 # this operation can be faster at rcpp level
          converted_omit_dyad$time <- rep(-1,data$M)
          for(m in 1:data$M){
            if(sum(data$supplist[m,]) < (data$N*(data$N-1)*data$C)){ #if there is a change in the riskset, we need to assign which row (in c++ notation it is in the riskset object matrix)
            temp_mat <-  matrix(rep(data$supplist[m,],dim(converted_omit_dyad$riskset)[1]),nrow=dim(converted_omit_dyad$riskset)[1],byrow=TRUE)
            find_loc <- apply(converted_omit_dyad$riskset - temp_mat,1,sum)
            converted_omit_dyad$time[m] <- (which(find_loc==0)-1)
            }
          }
        }
      }

      # create 'reh' class object
      out <- remify::reh(edgelist = edgelist_orig,
                          actors = as.character(1:data$N),
                          types = as.character(1:data$C), 
                          directed = TRUE, 
                          ordinal = FALSE, 
                          origin = 0,
                          omit_dyad = NULL, # set to NULL but added later
                          model = "tie")
      # we have to reorder the columns of converted_omit_dyad
      dict_loc <- attr(out,"dictionary")
      position_rearranged <- NULL
      for(d in 1:dim(dyads_l)[1]){
        sender_old <- dyads_l$actor1[d]-1
        receiver_old <- dyads_l$actor2[d]-1 
        type_old <- dyads_l$type[d]-1
        
        sender_new <- as.numeric(dict_loc$actors$actorName[which(dict_loc$actors$actorID == sender_old)])-1
        receiver_new <- as.numeric(dict_loc$actors$actorName[which(dict_loc$actors$actorID == receiver_old)])-1
        type_new <- as.numeric(dict_loc$types$typeName[which(dict_loc$types$typeID == type_old)])-1

        position_new <- getDyadIndex(actor1=sender_new,actor2=receiver_new,type=type_new,N=out$N,directed=attr(out,"directed"))+1 
        position_rearranged <- c(position_rearranged,position_new)
      }
      converted_omit_dyad$riskset <- converted_omit_dyad$riskset[,position_rearranged]
      out$omit_dyad <- converted_omit_dyad
      return(out)
    }
         
}                   








