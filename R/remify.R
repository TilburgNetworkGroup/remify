#' @title Process a Relational Event History
#'
#' @description A function that processes raw relational event history data and returns a S3 object of class 'remify' which is used as input in other functions inside \code{remverse}.
#'
#' @param edgelist an object of class \code{\link[base]{data.frame}} characterizing the relational event history sorted by 
#' time with columns named `time`, `actor1`, `actor2` and optionally `type` and 
#' `weight`.  
#' @param actors character vector of actors' names that may be observed interacting in the network. If \code{NULL}, actors' names will be taken from the input edgelist.
#' @param types character vector of event types that may occur in the network. If \code{NULL}, type names will be taken from the input edgelist.
#' @param directed logical value indicating whether dyadic events are directed (\code{TRUE}) or undirected (\code{FALSE}).
#' @param ordinal  logical value indicating whether only the order of events matters in the model (\code{TRUE}) or also the waiting time must be considered in the model (\code{FALSE}).
#' @param origin time point since when events could occur (default is \code{NULL}). If it is defined, it must have the same class of the time column in the input edgelist.
#' @param omit_dyad list of lists. Each list refers to one risk set modification and must have two objects: a first object named `time`, that is a vector of two values defining the first and last time point of the time window where to apply the change to the risk set and a second object, named `dyad`, which is a \code{\link[base]{data.frame}} where dyads to be removed are supplied in the format \code{actor1,actor2,type} (by row). The \code{NA} value can be used to remove multiple objects from the risk set at once with one risk set modification list (see Details).
#' @param model can be "tie" or "actor" oriented modeling. This argument plays a fundamental role when \code{omit_dyad} is supplied. Indeed, when actor-oriented modeling, the dynamic risk set will consist of two risk sets objects (senders' and dyads' risk sets). In the tie-oriented model the function will return a dynamic risk set referred at a dyad-level.
#'
#' @return  'remify' S3 object 
#'
#' @details In \code{omit_dyad}, the \code{NA} value can be used to remove multiple objects from the risk set at once with one risk set modification list. For example, to remove all events with sender equal to actor “A” add a list with two objects \code{time = c(NA, NA)} and \code{dyad = data.frame(actor1 = A, actor2 = NA, type = NA)} to the \code{omit_dyad} list.
#' 
#' For more details about the \code{omit_dyad} argument, inputs, outputs, attributes and methods of \code{remify::remify()}, see \code{vignette("remify")}. 
#'
#' @export
remify <- function(edgelist,
                actors = NULL,
                types = NULL,
                directed = TRUE,
                ordinal = FALSE,
                origin = NULL,
                omit_dyad = NULL,
                model = c("tie","actor") #,
                #[[to work on]] timeunit = c("second","minute","hour","day","week") this input will process the intervent time to different time unit
                ){
    
    # (1) Checking for 'edgelist' input object

    # Make sure edgelist is a data.frame
    if(!is.data.frame(edgelist)){
      stop("`edgelist` must be of class `data.frame`.")
    }

    # (2) Checking for `edgelist` columns (names and class of time variable)
     
    # Checking `edgelist$time` column
    if(!("time" %in% names(edgelist))){
      stop("`edgelist` should contain a column named `time` with the timing/order information for the events.")
    }
    if(!(class(edgelist$time)[1] %in% c("numeric","integer","Date","POSIXct"))){
      stop("the class of column `time` in  `edgelist` must be one of the following types: numeric, integer, Date or POSIXct")
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
    if(is.null(model) || all(model==c("tie","actor")) || (length(model)>1)) {
        model <- "tie"
        warning("`model` set to `tie` by default")
    }
    if(!is.null(model) & !(model %in% c("tie","actor"))) stop("`model` must be set to either `tie` or `actor`.")

    # (3) Checking for time variable classes (they must be the same)

    # input `origin` and `time` column in `edgelist`
    if(!is.null(origin)){
      if(any(class(origin) != class(edgelist$time)))
        stop("the class of `origin` and the class of `edgelist$time` must be the same.")
    }

    # input `omit_dyad` and `time` column in `edgelist`
    if(!is.null(omit_dyad)){
      if(!is.list(omit_dyad)){
        stop("`omit_dyad` must be a list. Check vignette(topic = 'remify', package= 'remify') for more information.")
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
          stop("`omit_dyad` must be a collection of lists with two named objects: `dyad` and `time`. Check vignette(topic = 'remify', package= 'remify') for more information.")
        }
      }
    }

    # (1) Checking for NA's 

    ## (1.1) NA's in `edgelist` :
	  if(anyNA(edgelist)){
		warning("`edgelist` contains missing data: incomplete events are dropped.") # `edgelist` contains missing data: incomplete events (rows) are dropped.
		to_remove <- unique(which(is.na(edgelist), arr.ind = T)[,1])
		edgelist <- edgelist[-to_remove,]
    #[[to check]]if(is.null(dim(edgelist)[1])) stop("The `edgelist` object is empty.")
    }

    # Pre-processing relational event history (rehCpp.cpp)
    out <- remifyCpp(input_edgelist = edgelist,
                    actors = actors, 
                    types = types, 
                    directed = directed,
                    ordinal = ordinal,
                    origin = origin,
                    omit_dyad = omit_dyad,
                    model = model)
                    
    str_out <- structure(list(M = out$M
                            ,N = out$N
                            ,C = out$C
                            ,D = out$D
                            ,intereventTime = out$intereventTime
                            ,edgelist = out$edgelist
                            ,omit_dyad = out$omit_dyad
                            )
                            ,class="remify")

    attr(str_out, "with_type") <- out$with_type
    attr(str_out, "weighted") <- out$weighted
    attr(str_out, "directed") <- directed
    attr(str_out, "ordinal") <- ordinal
    attr(str_out, "model") <- model # useful because tie and actor models have two different ways for handling changing risksets
    attr(str_out, "riskset") <- ifelse(length(omit_dyad)>0,"dynamic","static")
    attr(str_out, "dictionary") <- list(actors = out$actorsDictionary, types = out$typesDictionary)
    attr(str_out, "origin") <- out$edgelist$time[1]-out$intereventTime[1]
    attr(str_out, "dyad") <- out$dyad


    return(str_out)
}


#######################################################################################
#######################################################################################
###########(START)           Methods for `remify` object           (START)#############
#######################################################################################
#######################################################################################

#' @title summary.remify
#' @rdname summary.remify
#' @description A function that returns a summary of the event history.
#' @param object a \code{remify} object.
#' @param ... other arguments.
#' @method summary remify
#' @export
summary.remify <- function(object,...){
  title <- "Relational Event Network"
  model <- paste("(processed for ",attr(object,"model"),"-oriented modeling):",sep="")
  events <- paste("\t> events = ",object$M,sep="")
  actors <- paste("\t> actors = ",object$N,sep="")
  types <- if(!attr(object,"with_type")) NULL else {paste("\t> (event) types = ",object$C,sep="")}
  riskset <- paste("\t> riskset = ",attr(object,"riskset"),sep="")
  directed <- paste("\t> directed = ",attr(object,"directed"),sep="")
  ordinal <- paste("\t> ordinal = ",attr(object,"ordinal"),sep="")
  weighted <- paste("\t> weighted = ",attr(object,"weighted"),sep="")
  time_length <- NULL
  time <- object$edgelist$time
  origin <- attr(object, "time")$origin
  if(!attr(object,"ordinal")){
    time_length <- time[object$M] - attr(object,"origin")
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
  out_summary <- c(title,model,events,actors,types,riskset,directed,ordinal,weighted,time_length,interevent_time)
  out_summary <- out_summary[!is.null(out_summary)]
  cat(paste(out_summary,collapse="\n"))
}

#######################################################################################
#######################################################################################

#' @title print.remify
#' @rdname print.remify
#' @description print a summary of the event history.
#' @param x is a \code{remify} object.
#' @param ... other arguments.
#' @method print remify
#' @export
print.remify <- function(x,...){
  summary(object=x,...)
}

#######################################################################################
#######################################################################################

#' @title dim.remify
#' @rdname dim.remify
#' @description A function that returns the dimension of the temporal network.
#' @param x a \code{remify} object.
#' @method dim remify
#' @export
dim.remify <- function(x){
  dimensions <- NULL
  if(attr(x,"with_type")){
    dimensions <- c(x$M, x$N, x$C, x$D)
    names(dimensions) <- c("events","actors","types","dyads")
  }
  else{
    dimensions <- c(x$M, x$N, x$D)
    names(dimensions) <- c("events","actors","dyads") 
  }
  return(dimensions)
}

#######################################################################################
#######################################################################################

#' @title getDynamicRiskset
#' @description This function returns a matrix describing the possible risk set changes specified by the input `omit_dyad`. In such a matrix: value 1 refers to the dyads in the risk set, and 0 otherwise (dyads excluded from the risk set). All the possible risk set modifications are described by row, and the columns identify the dyads. Note: This matrix is the output given by processing the input `omit_dyad`, and the number of rows might be equal to or higher than the number of objects in `omit_dyad`. This might happen because more than one modification of the risk set defined in the input could overlap over time with others. 
#' @param x a \code{remify} object.
#' @export
getDynamicRiskset <- function(x){
  UseMethod("getDynamicRiskset")
}

#' @describeIn getDynamicRiskset dynamic riskset object
#' @method getDynamicRiskset remify
#' @export
getDynamicRiskset.remify <- function(x) {
  if(attr(x, "riskset") == "dynamic"){
    if(attr(x,"model") == "tie"){
      return(list(riskset = x$omit_dyad$riskset))
      }
    else if(attr(x,"model") == "actor"){
      return(list(sender = x$omit_dyad$risksetSender, dyad = x$omit_dyad$riskset)) 
    }
  }
  else{
    stop("risk set is not dynamic")
  }
}

#######################################################################################
#######################################################################################

#' @title getActorName
#' @description A function that given a vector of actor ID's returns the corresponding vector of actor (input) names.
#' @param x a \code{remify} object.
#' @param actorID a vector of actor ID's. The ID value can range between \code{1} and \code{N} (number of actors in the network).
#' @export
getActorName <- function(x, actorID = NULL){
  UseMethod("getActorName")
}

#' @describeIn getActorName return actor's name from actor's ID
#' @method getActorName remify
#' @export
getActorName.remify <- function(x, actorID = NULL) {
  names <- NULL
  if(is.null(actorID)) stop("provide at least one actorID.")
  else{ 
    if(!is.numeric(actorID)){
      stop("'actorID' must be numeric or integer.")
    }
    else{
      actorID <- as.integer(actorID)
      actors <- attr(x, "dictionary")$actors
      which_actor <- sapply(actorID, function(y) which(actors$actorID == y))
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

#' @title getTypeName
#' @description A function that given a vector of type ID's returns the corresponding vector of type (input) names.
#' @param x a \code{remify} object.
#' @param typeID a vector of type ID's. The ID value can range between \code{1} and \code{C} (number of event types in the network).
#' @export
getTypeName <- function(x, typeID = NULL){
  UseMethod("getTypeName")
}

#' @describeIn getTypeName return type's name from type's ID
#' @method getTypeName remify
#' @export
getTypeName.remify <- function(x, typeID = NULL) {
  names <- NULL
  if(is.null(typeID)) stop("provide at least one typeID.")
  else{ 
    if(!is.numeric(typeID)){
      stop("'typeID' must be numeric or integer.")
    }
    else{
      typeID <- as.integer(typeID)
      types <- attr(x, "dictionary")$types
      which_type <- sapply(typeID, function(y) which(types$typeID == y))
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

#' @title getActorID
#' @description A function that given a vector of actor names returns the corresponding vector of ID's.
#' @param x a \code{remify} object.
#' @param actorName a vector of actor names. The same names in the input edgelist.
#' @export
getActorID <- function(x, actorName = NULL){
  UseMethod("getActorID")
}

#' @describeIn getActorID return actor's ID from actor's name
#' @method getActorID remify
#' @export
getActorID.remify <- function(x, actorName = NULL) {
  IDs <- NULL
  if(is.null(actorName)) stop("provide at least one actorName.")
  else{ 
      actorName <- as.character(actorName)
      actors <- attr(x, "dictionary")$actors
      which_actor <- sapply(actorName, function(y) which(actors$actorName == y))
      which_actor <- unlist(which_actor)
      IDs <- actors$actorID[which_actor]
      if(length(IDs) == 0) stop("no actorName was found in the dictionary.")
      else if(length(IDs) < length(actorName)) warning("some actorName was not found in the dictionary.") 
    
  }
  return(IDs)
}

#######################################################################################
#######################################################################################

#' @title getTypeID
#' @description A function that given a vector of type names returns the corresponding vector of ID's.
#' @param x a \code{remify} object.
#' @param typeName a vector of type names. The same names in the input edgelist.
#' @export
getTypeID <- function(x, typeName = NULL){
  UseMethod("getTypeID")
}

#' @describeIn getTypeID return type's ID from type's name
#' @method getTypeID remify
#' @export
getTypeID.remify <- function(x, typeName = NULL) {
  IDs <- NULL
  if(is.null(typeName)) stop("provide at least one typeName.")
  else{ 
    typeName <- as.character(typeName)
    types <- attr(x, "dictionary")$types
    which_type <- sapply(typeName, function(y) which(types$typeName == y))
    which_type <- unlist(which_type)
    IDs <- types$typeID[which_type]
    if(length(IDs) == 0) stop("no typeName was found in the dictionary.")
    else if(length(IDs) < length(typeName)) warning("some typeName was not found in the dictionary.")       
  }
  return(IDs)
}

#######################################################################################
#######################################################################################

#' @title getDyadID
#' @description A function that given a vector of names as to actor1, actor2 and type returns the corresponding dyad ID. The names to supply are the original input names of the edgelist before the processing via the function \code{remify::remify()}.
#' @param x a \code{remify} object.
#' @param actor1 [character] name of actor1. 
#' @param actor2 [character] name of actor2.
#' @param type [character] name of type.
#' @export
getDyadID <- function(x, actor1, actor2, type){
  UseMethod("getDyadID")
}

#' @describeIn getDyadID return dyad's ID from dyad's composition
#' @method getDyadID remify
#' @export
getDyadID.remify <- function(x, actor1, actor2, type) {
  if(attr(x,"with_type")){
    if(!is.vector(type) | (length(type)>1)){
      stop("'type' must be a character vector of length 1")
    }
    type <- as.character(type)
  }
  if((!is.vector(actor1) | !is.vector(actor2)) | ((length(actor1)>1) | (length(actor2)>1))){
    stop("'actor1' and 'actor2' must be character vectors of length 1")
  }
  actor1 <- as.character(actor1)
  actor2 <- as.character(actor2)

  # check on self-event
  if((actor1==actor2)){
    stop("'actor1' and 'actor2' must be different")
  }

  dict_loc <- attr(x,"dictionary")
  # finding actors from the dictionary of names (attribute of the reh object)
  actor1_id <- which(dict_loc$actors$actorName == actor1)
  actor2_id <- which(dict_loc$actors$actorName == actor2)
  check_on_actors <- (c(length(actor1_id),length(actor2_id))==0) 
  if(any(check_on_actors)){
    stop(paste("input ",ifelse(sum(check_on_actors)==1,c("'actor1' "," 'actor2' ")[check_on_actors],c("'actor1' and 'actor2' ")),"not found in the 'remify' object",sep=""))
  }
  # finding type from the dictionary of names (attribute of the reh object)
  type_id <- 1
  if(attr(x,"with_type")){
    type_id <- which(dict_loc$types$typeName == type)
    if(length(type_id)==0){
      stop("'type' not found in the 'remify' object")
    }
  }
  # finding dyad ID
  dyad_id <- remify:::getDyadIndex(actor1 = actor1_id-1,
                                    actor2 = actor2_id-1,
                                    type = type_id-1,
                                    N = x$N,
                                    directed = attr(x,"directed"))+1 

  return(dyad_id)
}

#######################################################################################
#######################################################################################

#' @title getDyad
#' @description A function that given a vector of one or more dyad ID's returns the corresponding dyad composition of "actor1", "actor2" and "type" (if event types are present). The ID's to supply must range between 1 and D (largest risk set size).
#' @param x a \code{remify} object.
#' @param getDyad a vector of one or more dyad ID's, each one ranging from 1 to D (largest risk set size).
#' @export
getDyad <- function(x, dyadID){
  UseMethod("getDyad")
}

#' @describeIn getDyad return dyad composition in actor1, actor2 and type from one (or more) dyad ID
#' @method getDyad remify
#' @export
getDyad.remify <- function(x, dyadID) {
  if(!is.numeric(dyadID) & !is.integer(dyadID)){
    stop("'dyadID' must be a numeric (or integer) vector")
  }
  out <- NULL
  dyadID <- as.integer(dyadID) # if the ID supplied is 124.8, the ID considered will be 124
  
  # check for duplicates in dyadID
  length_orig <- length(dyadID)
  dyadID <- unique(dyadID)
  if(length_orig > length(dyadID)){
    warning("'dyadID' contains ID's that are repeated more than once. Such ID's will be processed once")
  }

  # apply function remify:::getDyadComposition()
  dict_loc <- attr(x,"dictionary")
  if(attr(x,"with_type")){ # output with 'type' column
    actor1_name <- actor2_name <- type_name <- rep(NA, length=length(dyadID))
    for(d in 1:length(dyadID)){
      if((dyadID[d] < 1) | (dyadID[d] > x$D)){
        stop(paste("'dyadID' must range between 1 and ",x$D,", given that the size of the largest risk set is ",x$D,sep=""))
      }
      dyad_composition_loc <- remify:::getDyadComposition(d = dyadID[d]-1, C = x$C, N = x$N, D = x$D)
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
      if((dyadID[d] < 1) | (dyadID[d] > x$D)){
        stop(paste("'dyadID' must range between 1 and ",x$D,", givent that the size of the largest risk set is ",x$D,sep=""))
      }
      dyad_composition_loc <- remify:::getDyadComposition(d = dyadID[d]-1, C = 1, N = x$N, D = x$D)
      actor1_name[d] <- dict_loc$actors$actorName[dyad_composition_loc[1]+1]
      actor2_name[d] <- dict_loc$actors$actorName[dyad_composition_loc[2]+1]
      rm(dyad_composition_loc)
    }
    out <- data.frame(dyadID = dyadID, actor1 = actor1_name, actor2 = actor2_name)  
    rm(actor1_name,actor2_name)     
  }

  return(out)
}

#######################################################################################
#######################################################################################

#' @title plot.remify
#' @rdname plot.remify
#' @description plot visualization.
#' @param x is a \code{remify} object.
#' @param which one or more numbers between 1 and 4. Explain the numbers: (1) plots this, (2) plots that, (3) plots this and (4) plot that.
#' @param caption list of titles for each plot.
#' @param APA_style make the plots APA-style-ready.
#' @param ... other arguments.
#' @method plot remify
#' @export
plot.remify <- function(x,
                    which = c(1:4),
                    caption = list("Title 1",
                                   "Title 2", 
                                   "Title 3",
                                   "Title 4"),   
                    APA_style = FALSE,
                    ...){
  
# [[temporary code]]
x <- .GlobalEnv$edgelist_reh 

# [[current status of the method]] this function ONLY works for single event sequences inside the reh object (two-mode and multiple sequences network)

# checks on input arguments

# check on captions supplied by the user

# ---

# other settings
ask_new_page <- devAskNewPage(TRUE)
on.exit(devAskNewPage(ask_new_page))


# tie-oriented modeling
if(attr(x,"model") == "tie"){
  # [[1]] histogram of the waiting times
  # Y-axis = Frequency (freq=TRUE)
  # X-axis = measurement unit of the waiting time (understand it from the 'remify' object)
  dev.hold()
  time_unit <- NULL
  time_unit <- attr(x$intereventTime,"unit")
  hist(x = x$intereventTime, 
     breaks = 40,
     angle = 45, 
     col = "lavender", 
     border = "darkgray",
     main = paste("Distribution of the waiting times (t[m] - t[m-1]) ",collapse=""),
     xlab = ifelse(!is.null(time_unit),paste("waiting time (",time_unit,")",sep="", collapse=""),paste("waiting time")))
  dev.flush()


  # [[2]] joint and marginal counts (dyads, actor1, actor2) [[[at the moment the TYPE information is omitted]]]
  dev.hold()
  # calculating frequencies
  # ... actor1
  actor1_freq <- table(x$edgelist$actor1_ID)
  # ... actor2
  actor2_freq <- table(x$edgelist$actor2_ID)
  # ... dyad with coords (sorted from large to small frequency values) only observed dyads are included (some actors may be excluded from the plot)
  # [[??]] Maybe reorder the ACTORS depending on their 'activity'. --> this could be diffcult becaus we have to chose which between actor1 and actor2
  dyad_freq <- sort(table(paste(x$edgelist$actor1_ID,x$edgelist$actor2_ID,sep="_")),decreasing=TRUE) # reordering dyads based on their frequency
  X <- matrix(NA, nrow=length(dyad_freq),ncol=3) #temp becaus there can be dyads that we do not observe
  for(d in 1:length(dyad_freq)){
    X[d,3] <- as.integer(dyad_freq[d])
    X[d,1:2] <- as.integer(unlist(strsplit(x = names(dyad_freq[d]),  split = "_")))
  }
  actors_observed <- X[,1:2]
  N_obs <- length(unique(as.vector(actors_observed)))
  egrid <- expand.grid(1:N_obs,1:N_obs)
  egrid <- egrid[,2:1]
  X_out <- data.frame(row = egrid[,1],col = egrid[,2], fill = 0)  
  for(d in 1:dim(X)[1]){
    row_index <- which((X_out$row == X[d,1]) & (X_out$col == X[d,1]))
    X_out$fill[row_index] <- X[d,3]
  }
  X_out$fill <- X_out$fill/max(X_out$fill)
  xlab="actor2"
  ylab="actor1"
  layout_matrix <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(layout_matrix, widths=c(4/5,1/5), heights=c(1/5,4/5))
  top = max(c(actor1_freq, actor2_freq))
  par(mar=c(3,3,1,1))
  # tile plot
  #plot(1:x$N,1:x$N,type="n",axes=FALSE)
  plot.new()
  plot.window(xlim=c(1,x$N),ylim=c(1,x$N),asp=1)
  with(X_out,rect(col-0.5,row-0.5,col+0.5,row+0.5,col=terrain.colors(12)[fill],border="gray"))
  #[[to remove]]with(X,text(col-0.5,row-0.5,val,font=2,cex=2))
  # histograms
  par(mar=c(0,3,1,1))
  barplot(unname(table(x$edgelist$actor2_ID)), axes=FALSE, ylim=c(0, top), space=0, names.arg= NULL,border="darkgray",col="lavender")
  par(mar=c(3,0,1,1))
  barplot(unname(table(x$edgelist$actor1_ID)), axes=FALSE, xlim=c(0, top), space=0, , names.arg = NULL, horiz=TRUE,border="darkgray",col="lavender")
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
    at=floor(x$N/2))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
    at=floor(x$N/2))
  dev.flush()
  # [[3]] observed dyad/potential per interval of the time or "number of active actors / number of actors"
  dev.hold()
  n_intervals <-floor((as.numeric(x$edgelist$time[x$M])-as.numeric(x$edgelist$time[1]))/(60*60*24))  # but we could define an internal function get_intervals(time)
  time <- x$edgelist$time #as.Date(x$edgelist$time)
  time <- cut(as.numeric(time),breaks = n_intervals)
  y <- tapply(X =x$edgelist$time, INDEX = time, FUN = length) # num.events/(day or hour)
  z <- tapply(X =x$edgelist$actor1_ID, INDEX = time, FUN = function(y){length(unique(y)*10)})
  par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
  plot(y,type="l",col=2)             # Create first plot
  par(new = TRUE)                             # Add new plot
  plot(z, type="l", col = 3,              # Create second plot without axes
      axes = FALSE, xlab = "", ylab = "")
  axis(side = 4, at = pretty(range(z)))      # Add second axis
  mtext("actors", side = 4, line = 3)             # Add second axis label
  dev.flush()
  # [[4]] ??
  #as.mmdd <- function(x, ...) UseMethod("as.mmdd")
  #as.mmdd.Date <- function(x, ...) structure(x, class = c("mmdd", "Date"))
  #as.Date.mmdd <- function(x, ...) structure(x, class = "Date")
  #format.mmdd <- function(x, format = "%m-%d", ...) format(as.Date(x), format = format, ...)


  # [[??]] it would be useful to give a X by Y panel with the observed event times = I think that already the distribution of the waiting times is enough

  # [[?? this might need some discussion for large networks]] an aggregated network plot where the thickness is related to the number of events for a tie, 
}
# actor-oriented modeling
else{

}


}

#######################################################################################
#######################################################################################
###########(END)             Methods for `remify` object             (END)#############
#######################################################################################
#######################################################################################

