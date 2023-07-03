#' @title Process a Relational Event History
#'
#' @description A function that processes raw relational event history data and returns a S3 object of class 'remify' which is used as input in other functions inside \code{remverse}.
#'
#' @param edgelist the relational event history. An object of class \code{\link[base]{data.frame}} with first three columns corresponding to time, and actors forming the dyad. The first three columns will be re-named "time", "actor1", "actor2" (where, for directed networks, "actor1" corresponds to the sender and "actor2" to the receiver of the relational event). Optional columns that can be supplied are: `type` and 
#' `weight`. If one or both exist in \code{edgelist}, they have to be named accordingly.
#' @param directed logical value indicating whether events are directed (\code{TRUE}) or undirected (\code{FALSE}). (default value is \code{TRUE})
#' @param ordinal  logical value indicating whether only the order of events matters in the model (\code{TRUE}) or also the waiting time must be considered in the model (\code{FALSE}). (default value is \code{FALSE})
#' @param model can be "tie" or "actor" oriented modeling. This argument plays a fundamental role when \code{omit_dyad} is supplied. Indeed, when actor-oriented modeling, the dynamic risk set will consist of two risk sets objects (senders' and dyads' risk sets). In the tie-oriented model the function will return a dynamic risk set referred at a dyad-level.
#' @param actors [\emph{optional}] character vector of actors' names that may be observed interacting in the network. If \code{NULL} (default), actors' names will be taken from the input edgelist.
#' @param types [\emph{optional}] character vector of event types that may occur in the network. If \code{NULL} (default), types' names will be taken from the input edgelist.
#' @param riskset [\emph{optional}] character value indicating the type of risk set to process: \code{riskset = "full"} (default) consists of all the possible dyadic events given the number of actors (and the number of event types) and it mantains the same structure over time. \code{riskset = "active"} considers at risk only the observed dyads and it mantains the same structure over time. \code{riskset = "manual"}, allows the risk set to have a structure that is user-defined, and it is based on the instructions supplied via the argument \code{omit_dyad}. This type of risk set allows for time-varying risk set, in which, for instance, subset of actors can interact only at specific time windows, or events of a specific type (sentiment) can't be observed within time intervals that are defined by the user.
#' @param origin [\emph{optional}] starting time point of the observaton period (default is \code{NULL}). If it is supplied, it must have the same class of the `time` column in the input \code{edgelist}.
#' @param omit_dyad [\emph{optional}] list of lists. Each list refers to one risk set modification and must have two objects: a first object named `time`, that is a vector of two values defining the first and last time point of the time window where to apply the change to the risk set and a second object, named `dyad`, which is a \code{\link[base]{data.frame}} where dyads to be removed are supplied in the format \code{actor1,actor2,type} (by row). The \code{NA} value can be used to remove multiple objects from the risk set at once with one risk set modification list (see Details).
#' @param ncores [\emph{optional}] number of cores used in the parallelization of the processing functions. (default is \code{1}).
#' 
#' @return  'remify' S3 object 
#'
#' @details In \code{omit_dyad}, the \code{NA} value can be used to remove multiple objects from the risk set at once with one risk set modification list. For example, to remove all events with sender equal to actor “A” add a list with two objects \code{time = c(NA, NA)} and \code{dyad = data.frame(actor1 = A, actor2 = NA, type = NA)} to the \code{omit_dyad} list.
#' 
#' For more details about the \code{omit_dyad} argument, inputs, outputs, attributes and methods of \code{remify::remify()}, see \code{vignette("remify")}. 
#'
#' @export
#' 
#' @examples
#' 
#' # load package and random network 'randomREH'
#' library(remify)
#' data(randomREH)
#' 
#' # first events in the sequence
#' head(randomREH$edgelist)
#' 
#' # actor's names
#' randomREH$actors
#' 
#' # event type's names
#' randomREH$types
#' 
#' # start time of the study (origin)
#' randomREH$origin
#' 
#' # list of changes of the risk set: each one is a list of:
#' # 'time' (indicating the time window where to apply the risk set reduction)
#' # 'dyad' (a data.frame describing the dyads to remove from the risk set 
#' # during the time window specified in 'time')
#' str(randomREH$omit_dyad)
#' 
#' # -------------------------------------- #
#' #  processing for tie-oriented modeling  #
#' # -------------------------------------- #
#' 
#' tie_randomREH <- remify(edgelist = randomREH$edgelist,
#'        directed = TRUE,
#'        ordinal = FALSE,
#'        model = "tie",
#'        actors = randomREH$actors,
#'        types = randomREH$types,
#'        riskset = "manual",
#'        origin = randomREH$origin,
#'        omit_dyad = randomREH$omit_dyad)
#' 
#' # summary
#' summary(tie_randomREH)
#' 
#' # dimensions of the processed network
#' dim(tie_randomREH)
#' 
#' # Which ID is assigned to the actors with names "Francesca" and "Kayla"?
#' getActorID(x = tie_randomREH, actorName = c("Francesca","Kayla"))
#' 
#' # Which ID is assigned to the event type "conflict"?
#' getTypeID(x = tie_randomREH, typeName = "conflict")
#' 
#' # Find dyad composition (names of actor1, actor2 and type) from the dyad ID: c(1,380,760,1140)
#' getDyad(x = tie_randomREH, dyadID = c(1,380,760,1140))
#' 
#' # visualize descriptive measures of relational event data
#' # plot(x = tie_randomREH)
#' 
#' # -------------------------------------- #
#' # processing for actor-oriented modeling #
#' # -------------------------------------- #
#' 
#' # loading network 'randomREHsmall'
#' data(randomREHsmall)
#' 
#' # processing small random network
#' actor_randomREH <- remify(edgelist = randomREHsmall$edgelist,
#'        directed = TRUE,
#'        ordinal = FALSE,
#'        model = "actor",
#'        actors = randomREHsmall$actors,
#'        origin = randomREHsmall$origin)
#'        
#' # summary
#' summary(actor_randomREH)
#' 
#' # dimensions of the processed network
#' dim(actor_randomREH)
#' 
#' # ------------------------------------ #
#' # for more information about remify()  #
#' # check: vignette(package="remify")    #
#' # ------------------------------------ #
#'  
remify <- function(edgelist,
                directed = TRUE,
                ordinal = FALSE,
                model = c("tie","actor"),
                actors = NULL,
                types = NULL,
                riskset = c("full","active","manual"),
                origin = NULL,
                omit_dyad = NULL,
                ncores = 1L
                #[[to work on]] timeunit = c("second","minute","hour","day","week") this input will process the intervent time to different time unit
                ){
    
    # (1) Checking for 'edgelist' input object

    # Make sure edgelist is a data.frame
    if(!is.data.frame(edgelist)){
      stop("`edgelist` must be of class `data.frame`.")
    }

    # ... ncores
    if(is.null(ncores)) ncores <- 1L
    else if(((parallel::detectCores() > 2L) & (ncores > floor(parallel::detectCores()-2L))) | ((parallel::detectCores() == 2L) & (ncores > 1L))){
            stop("'ncores' is recommended to be set at most to: floor(parallel::detectCores()-2L)")
    }


    # (2) Checking for `edgelist` columns (names and class of time variable)
    if(dim(edgelist)[2] < 3){
      stop("`edgelist` must be a data.frame of three columns: one column for the vector of event times, and two columns that describe the actors that form the dyadic event.")
    }
     
    # Checking `edgelist$time` column
    assign_names <- NULL
    if(!("time" %in% names(edgelist))){
      #stop("`edgelist` should contain a column named `time` with the timing/order information for the events.")
      names(edgelist)[1] <- "time"
    }
    if(!(class(edgelist$time)[1] %in% c("numeric","integer","Date","POSIXct"))){
      stop("the class of column `time` in  `edgelist` must be one of the following types: numeric, integer, Date or POSIXct")
    }

    # Checking `edgelist$actor1` column
    if(!("actor1" %in% names(edgelist))){
      #stop("`edgelist` should contain a column named `actor1` with the first actors/senders of the events.")
      names(edgelist)[2] <- "actor1"
    }

    # Checking `edgelist$actor2` column
    if(!("actor2" %in% names(edgelist))){
      #stop("`edgelist` should contain a column named `actor2` with the second actors/receivers of the events.")
      names(edgelist)[3] <- "actor2"
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
          class_time_check <- unlist(lapply(omit_dyad, function(x) if(!all(is.na(x$time))) {all(class(x$time) == class(edgelist$time))} else{TRUE}))
          if(!all(class_time_check)){
            stop("the class of the time specified in `omit_dyad` and the class of `edgelist$time` must be the same.")
          }
        }
        else{
          stop("`omit_dyad` must be a collection of lists with two named objects: `dyad` and `time`. Check vignette(topic = 'remify', package= 'remify') for more information.")
        }
      }
    }


    # Checking argument 'riskset'
    riskset  <- match.arg(arg = riskset, choices = c("full", "active", "manual"), several.ok = FALSE)
    active <- FALSE
    if(riskset == "active"){
      active <- TRUE
    }

    # Checking for NA's 

    ## NA's in `edgelist` :
 
	  if(anyNA(edgelist)){
		warning("`edgelist` contains missing data: incomplete events are dropped.") # `edgelist` contains missing data: incomplete events (rows) are dropped.
		to_remove <- unique(which(is.na(edgelist), arr.ind = T)[,1])
		edgelist <- edgelist[-to_remove,]
    #[[to check]]if(is.null(dim(edgelist)[1])) stop("The `edgelist` object is empty.")
    }
    
    # Pre-processing relational event history (remifyCpp.cpp)
    out <- remifyCpp(input_edgelist = edgelist,
                    actors = actors, 
                    types = types, 
                    directed = directed,
                    ordinal = ordinal,
                    origin = origin,
                    omit_dyad = omit_dyad,
                    model = model,
                    active = active,
                    ncores = ncores)
    
    str_out <- structure(list(M = out$M
                            ,N = out$N
                            ,C = out$C
                            ,D = out$D
                            ,intereventTime = out$intereventTime
                            ,edgelist = out$edgelist
                            )
                            ,class="remify")

    
    attr(str_out, "with_type") <- out$with_type
    attr(str_out, "weighted") <- out$weighted
    attr(str_out, "directed") <- directed
    attr(str_out, "ordinal") <- ordinal
    attr(str_out, "model") <- model # useful because tie and actor models have two different ways for handling changing risksets
    attr(str_out, "riskset") <- riskset
    attr(str_out, "dictionary") <- list(actors = out$actorsDictionary, types = out$typesDictionary)
    attr(str_out, "origin") <- out$edgelist$time[1]-out$intereventTime[1]
    attr(str_out, "dyad") <- out$dyad
    attr(str_out, "ncores") <- ncores 

    if(active){
      str_out$activeD <- out$omit_dyad$D_active
      if(model == "tie"){
        attr(str_out, "dyadIDactive") <- out$omit_dyad$dyadIDactive 
        out$omit_dyad <- NULL
      }
    }                   
    str_out$omit_dyad <- out$omit_dyad

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
#' 
#' @examples 
#'
#' # processing the random network 'randomREHsmall'
#' library(remify)
#' data(randomREHsmall)
#' reh <- remify(edgelist = randomREHsmall$edgelist,
#'               model = "tie")
#' 
#' # printing a summary of the processed 'remify' object
#' summary(reh)
#' 
summary.remify <- function(object,...){
  title <- "Relational Event Network"
  model <- paste("(processed for ",attr(object,"model"),"-oriented modeling):",sep="")
  events <- paste("\t> events = ",object$M,sep="")
  actors <- paste("\t> actors = ",object$N,sep="")
  types <- if(!attr(object,"with_type")) NULL else {paste("\t> (event) types = ",object$C,sep="")}
  riskset <- paste("\t> riskset = ",attr(object,"riskset"),sep="")
  if(attr(object,"riskset")=="active"){
    riskset <- c(riskset,paste("\t\t>> active dyads = ",object$activeD," (full risk set size =",object$D,")",sep=""))
  }
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
#' @param x a \code{remify} object.
#' @param ... further arguments.
#' @method print remify
#' @export
#' 
#' @examples 
#'  
#' # processing the random network 'randomREHsmall'
#' library(remify)
#' data(randomREHsmall)
#' reh <- remify(edgelist = randomREHsmall$edgelist,
#'               model = "tie")
#' 
#' # printing a summary of the processed 'remify' object
#' print(reh)
#' 
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
#' 
#' @examples 
#' 
#' # processing the random network 'randomREHsmall'
#' library(remify)
#' data(randomREHsmall)
#' reh <- remify(edgelist = randomREHsmall$edgelist,
#'               model = "tie")
#' 
#' # dimensions of the processed 'remify' object
#' dim(reh)
#' 
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
  if(attr(x,"riskset")=="active"){
    dimensions <- c(dimensions, "dyads(active)" = x$activeD)
  }
  return(dimensions)
}

#######################################################################################
#######################################################################################

#' @title getRiskset
#' @description This function returns a matrix describing the possible risk set changes specified by the input `omit_dyad`. In such a matrix: value 1 refers to the dyads in the risk set, and 0 otherwise (dyads excluded from the risk set). All the possible risk set modifications are described by row, and the columns identify the dyads. Note: This matrix is the output given by processing the input `omit_dyad`, and the number of rows might be equal to or higher than the number of objects in `omit_dyad`. This might happen because more than one modification of the risk set defined in the input could overlap over time with others. 
#' @param x a \code{remify} object.
#' @export
#' 
#' @examples 
#' 
#' # processing the random network 'randomREH'
#' library(remify)
#' data(randomREH)
#' reh <- remify(edgelist = randomREH$edgelist,
#'               model = "tie",
#'               riskset = "manual",
#'               omit_dyad = randomREH$omit_dyad)
#' 
#' # structure of the processed risk set
#' str(getRiskset(reh))
#' 
getRiskset <- function(x){
  UseMethod("getRiskset")
}

#' @describeIn getRiskset manual riskset object
#' @method getRiskset remify
#' @export
getRiskset.remify <- function(x) {
  if(attr(x, "riskset") != "full"){
    if(attr(x,"model") == "tie"){
      return(list(riskset = x$omit_dyad$riskset))
      }
    else if(attr(x,"model") == "actor"){
      return(list(sender = x$omit_dyad$risksetSender, dyad = x$omit_dyad$riskset)) 
    }
  }
  else{
    stop("risk set is neither 'active' nor 'manual'.")
  }
}

#######################################################################################
#######################################################################################

#' @title getActorName
#' @description A function that given a vector of actor ID's returns the corresponding vector of actor (input) names.
#' @param x a \code{remify} object.
#' @param actorID a vector of actor ID's. The ID value can range between \code{1} and \code{N} (number of actors in the network).
#' @export
#' 
#' @examples 
#' 
#' # processing the random network 'randomREH'
#' library(remify)
#' data(randomREH)
#' reh <- remify(edgelist = randomREH$edgelist,
#'               model = "tie",
#'               riskset = "manual",
#'               omit_dyad = randomREH$omit_dyad)
#' 
#' # find actor name from actor ID
#' getActorName(x = reh, actorID = c(1,2,8,12))
#' 
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
#' 
#' @examples 
#' 
#' # processing the random network 'randomREH'
#' library(remify)
#' data(randomREH)
#' reh <- remify(edgelist = randomREH$edgelist,
#'               model = "tie",
#'               riskset = "manual",
#'               omit_dyad = randomREH$omit_dyad)
#' 
#' # find type name from type ID
#' getTypeName(x = reh, typeID = c(1,3))
#' 
getTypeName <- function(x, typeID = NULL){
  UseMethod("getTypeName")
}

#' @describeIn getTypeName return type's name from type's ID
#' @method getTypeName remify
#' @export
getTypeName.remify <- function(x, typeID = NULL) {
  if(is.null(x$C)){
    stop("'remify' object has no event types")
  }
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

#' @title getDyad
#' @description A function that given a vector of one or more dyad ID's returns the corresponding dyad composition of "actor1", "actor2" and "type" (if event types are present). The ID's to supply must range between 1 and D (largest risk set size).
#' @param x a \code{remify} object.
#' @param dyadID a vector of one or more dyad ID's, each one ranging from 1 to D (largest risk set size).
#' @param active logical, whether to consider the input \code{dyadID} as a vector of ID's of active dyads (\code{active = TRUE}) or dyads from the full risk set (\code{active = FALSE})
#' @export
#' 
#' @examples 
#' 
#' # processing the random network 'randomREH'
#' library(remify)
#' data(randomREH)
#' reh <- remify(edgelist = randomREH$edgelist,
#'               model = "tie",
#'               riskset = "manual",
#'               omit_dyad = randomREH$omit_dyad)
#' 
#' # find dyad composition (names of actor1, actor2 and type) from the dyad ID
#' getDyad(x = reh, dyadID = c(450,239,900))
#' 
getDyad <- function(x, dyadID, active = FALSE){
  UseMethod("getDyad")
}

#' @describeIn getDyad return dyad composition in actor1, actor2 and type from one (or more) dyad ID
#' @method getDyad remify
#' @export
getDyad.remify <- function(x, dyadID, active = FALSE) {

  if(active & attr(x,"riskset") != "active"){
    stop("'active' = TRUE works only for attr(x,'riskset') = 'active'")
  }
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

  # apply function getEventsComposition
  dict_loc <- attr(x,"dictionary")
  dyadID_full <- dyadID
  if(active){
    for(d in 1:length(dyadID)){
      dyadID_full[d] <- attr(x,"dyad")[which(attr(x,"dyadIDactive")==dyadID[d])[1]]
    }
  }
  composition <- getEventsComposition(dyads = dyadID_full, N = x$N, D = x$D,directed = attr(x,"directed"), ncores  = attr(x,"ncores"))
  if(attr(x,"with_type")){ # output with 'type' column
    out <- data.frame(dyadID = dyadID, actor1 = dict_loc$actors$actorName[composition[,1]], actor2 = dict_loc$actors$actorName[composition[,2]], type = dict_loc$types$typeName[composition[,3]])  
  }
  else{ # output without 'type' column (for sequences with one or none event type)
    out <- data.frame(dyadID = dyadID, actor1 = dict_loc$actors$actorName[composition[,1]], actor2 = dict_loc$actors$actorName[composition[,2]]) 
  }
  if(active){
    names(out)[1] <- "dyadIDactive"
  }
  
  rm(composition)

  return(out)
}

#######################################################################################
#######################################################################################

#' @title getActorID
#' @description A function that given a vector of actor names returns the corresponding vector of ID's.
#' @param x a \code{remify} object.
#' @param actorName a vector of actor names. The same names in the input edgelist.
#' @export
#' 
#' @examples 
#' 
#' # processing the random network 'randomREH'
#' library(remify)
#' data(randomREH)
#' reh <- remify(edgelist = randomREH$edgelist,
#'               model = "tie",
#'               riskset = "manual",
#'               omit_dyad = randomREH$omit_dyad)
#' 
#' # find actor ID from the actor name
#' getActorID(x = reh, actorName = c("Francesca","Kayla"))
#' 
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
#' 
#' @examples 
#' 
#' # processing the random network 'randomREH'
#' library(remify)
#' data(randomREH)
#' reh <- remify(edgelist = randomREH$edgelist,
#'               model = "tie",
#'               riskset = "manual",
#'               omit_dyad = randomREH$omit_dyad)
#' 
#' # find type ID from the type name
#' getTypeID(x = reh, typeName = c("conflict","cooperation"))
#' 
getTypeID <- function(x, typeName = NULL){
  UseMethod("getTypeID")
}

#' @describeIn getTypeID return type's ID from type's name
#' @method getTypeID remify
#' @export
getTypeID.remify <- function(x, typeName = NULL) {
  if(is.null(x$C)){
    stop("'remify' object has no event types")
  }
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
#' 
#' @examples 
#' 
#' # processing the random network 'randomREH'
#' library(remify)
#' data(randomREH)
#' reh <- remify(edgelist = randomREH$edgelist,
#'               model = "tie",
#'               riskset = "manual",
#'               omit_dyad = randomREH$omit_dyad)
#' 
#' # find dyad ID from dyad composition (names of actor1, actor2 and type)
#' getDyadID(x = reh, actor1 = "Francesca", actor2 = "Kayla", type = "conflict")
#' 
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
  dyad_id <- getDyadIndex_cpp(actor1 = actor1_id-1,
                                    actor2 = actor2_id-1,
                                    type = type_id-1,
                                    N = x$N,
                                    directed = attr(x,"directed"))
  if((attr(x,"riskset") == "active") & (attr(x,"model")=="tie")){
    select_loc <- which(attr(x,"dyad")==dyad_id)[1] # selecting first occurrence of dyad_id in attr(x,"dyad")
    if(length(select_loc > 0)){
      dyad_id <- c(dyad_id,ifelse(length(select_loc>0),attr(x,"dyadIDactive")[select_loc],NA))
      names(dyad_id) <- c("dyadID","dyadIDactive")
    }
  }                                 

  return(dyad_id)
}

#######################################################################################
#######################################################################################



#######################################################################################
#######################################################################################
###########(END)             Methods for `remify` object             (END)#############
#######################################################################################
#######################################################################################

