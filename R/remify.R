#' @title reh
#'
#' @description A function that processes raw data and returns a 'reh' S3 object which is used as input in other functions in \code{remverse}.
#'
#' @param edgelist an object of class \code{"\link[base]{data.frame}"} or 
#' \code{"\link[base]{matrix}"} characterizing the relational event history sorted by 
#' time with columns `time`, `actor1`, `actor2` and optionally `type` and 
#' `weight`.  
#' @param actors character vector of actors' names that may be observed interacting in the network. If \code{NULL}, actor names will be taken from the input edgelist.
#' @param types character vector of event types that may occur in the network. If \code{NULL}, type names will be taken from the input edgelist.
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
    
    # (1) Checking for 'edgelist' input object

    # Make sure edgelist is a data.frame
    if(!is.data.frame(edgelist)){
      edgelist <- as.data.frame(edgelist)
    }

    # (2) Checking for `edgelist` columns (names and class of time variable)
     
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

    # (3) Checking for `origin` and `time` variable class (they must be the same)

    # (4) Checking for `riskset` object names 

    # (1) Checking for NA's 

    ## (1.1) NA's in `edgelist` :
	  if(anyNA(edgelist)) {
		warning("`edgelist` contains missing data: incomplete events are dropped.") # `edgelist` contains missing data: incomplete events (rows) are dropped.
		to_remove <- unique(which(is.na(edgelist), arr.ind = T)[,1])
		edgelist <- edgelist[-to_remove,]
    #if(is.null(dim(edgelist)[1])) stop("The `edgelist` object is empty.")
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


#' @title rehshape
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
      stop('class of input data must be of length 1')
    }
    # data has to be either of class 'reh' or 'relevent'
    if(!all(data_format == c("reh","relevent"))){
      stop('class of input data must be either `reh` or `relevent`. see help(rehshape) for more information about the input structure of the argument `data`') 
    }
    # check data and output format
    if(data_format == output_format){
      warning("the format of the input data is the same as the required output format. The input data is returned")
      return(data)
    }
    
    # if data structure is 'reh'
    if(data_format ==  "reh"){

      # check reh object here
      ## ##
      ## ## ##
      # stop('')
      ## ##

      out <- NULL
      if(output_format == "relevent"){
        # (1) processing the edgelist
        eventlist <- data$edgelist[,c(2,1)] # [dyad,time]
        eventlist[,1] <- eventlist[,1]+1
        eventlist[,2] <- attr(data,"time")$value[,1] # if the input argument 'origin' is provided, this lines should be coded differently
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
      # stop('')
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
                          origin = NULL,
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








