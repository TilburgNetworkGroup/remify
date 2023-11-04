#' @title Process a Relational Event History
#'
#' @description A function that processes raw relational event history data and returns a S3 object of class 'remify' which is used as input in other functions inside 'remverse'.
#'
#' @param edgelist the relational event history. An object of class \code{\link[base]{data.frame}} with first three columns corresponding to time, and actors forming the dyad. The first three columns will be re-named "time", "actor1", "actor2" (where, for directed networks, "actor1" corresponds to the sender and "actor2" to the receiver of the relational event). Optional columns that can be supplied are: `type` and `weight`. If one or both exist in \code{edgelist}, they have to be named accordingly.
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
#' @return  'remify' S3 object, list of: number of events (`M`), number of actors (`N`), number of event types (if present, `C`), number of dyads (`D`, and also `activeD` if `riskset="active"`), vector of inter-event times (waiting times between two subsequent events), processed input edgelist as `data.frame`, processed `omit_dyad` object as `list`. The function returns also several attributes that make efficient the processing of the data for future analysis. For more details about the function, input arguments, output, attributes and methods, please read \code{vignette(package="remify",topic="remify")}. 
#'
#' @details In \code{omit_dyad}, the \code{NA} value can be used to remove multiple objects from the risk set at once with one risk set modification list. For example, to remove all events with sender equal to actor “A” add a list with two objects \code{time = c(NA, NA)} and \code{dyad = data.frame(actor1 = A, actor2 = NA, type = NA)} to the \code{omit_dyad} list. For more details about 
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
    if(!is.null(out$order)) edgelist <- edgelist[out$order+1,]
    
    str_out <- structure(list(M = out$M
                            ,N = out$N
                            ,C = out$C
                            ,D = out$D
                            ,intereventTime = out$intereventTime
                            ,edgelist = edgelist
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
    attr(str_out, "ncores") <- ncores 

    # ID of actors, types and dyads
    attr(str_out, "dyadID") <- out$dyad
    attr(str_out,"actor1ID") <- out$edgelist$actor1_ID
    attr(str_out,"actor2ID") <- out$edgelist$actor2_ID
    if(out$with_type){
        attr(str_out,"typeID") <- out$edgelist$type_ID
    }
    # ID's when riskset = "active"
    if(active){
      str_out$activeD <- out$omit_dyad$D_active
      if(model == "tie"){
        attr(str_out, "dyadIDactive") <- out$omit_dyad$dyadIDactive 
        out$omit_dyad <- NULL
      }
    }   

    str_out$omit_dyad <- out$omit_dyad
    out <- NULL # free-ing space
    
    # modifying remify object to account for simultaneous events
    rows_to_remove <- which(str_out$intereventTime == 0) # processing the co-occurrence of events
    if(length(rows_to_remove) != 0){
        str_out$intereventTime <- str_out$intereventTime[-rows_to_remove] # updating interevent time vector
        if(!is.null(str_out$omit_dyad)){
          str_out$omit_dyad$time <- str_out$omit_dyad$time[-rows_to_remove] # updating vector of risk set changes over time
        }
        str_out$E <- str_out$M # number of events
        str_out$M <- length(str_out$intereventTime) # overwrite (lower) number of time points
        time_unique <-unique(str_out$edgelist$time)

        # tie-oriented modeling
        actor1 <- list()
        actor2 <- list()
        type <- list()
        dyad <- list()
        dyadIDactive <- list()
        for(m in 1:length(time_unique)){
            which_time_m <- which(str_out$edgelist$time == time_unique[m])
            actor1[[m]] <- attr(str_out,"actor1ID")[which_time_m]
            actor2[[m]] <- attr(str_out,"actor2ID")[which_time_m]
            dyad[[m]] <- attr(str_out, "dyadID")[which_time_m]
            if(active){
              dyadIDactive[[m]] <- attr(str_out,"dyadIDactive")[which_time_m]
            }
            if(attr(str_out, "with_type")){
              type[[m]] <- attr(str_out,"typeID")[which_time_m]
            }
        }
        attr(str_out,"actor1ID") <- actor1 
        attr(str_out,"actor2ID") <- actor2
        attr(str_out,"dyadID") <- dyad
        if(active){
          attr(str_out,"dyadIDactive") <- dyadIDactive
        }
        if(attr(str_out, "with_type")){
          attr(str_out,"typeID") <- type
        }
        rm(actor1,actor2,type,dyad,dyadIDactive)
    }

  return(str_out)
}


#######################################################################################
#######################################################################################
###########(START)           Methods for `remify` object           (START)#############
#######################################################################################
#######################################################################################

#' @title summary.remify
#' @rdname summary.remify
#' @description A function that returns a easy-to-read summary of the main characteristics as to the processed relational event sequence.
#' @param object a \code{remify} object.
#' @param ... other arguments.
#' 
#' @return prints out the main characteristics of the processed relational event sequence.
#' 
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
  if(is.null(object$E)){
    events <- paste("\t> events = ",object$M,sep="")
  }
  else{
    events <- paste("\t> events = ",object$E," (time points = ",object$M,")",sep="")
  }
  actors <- paste("\t> actors = ",object$N,sep="")
  types <- if(!attr(object,"with_type")) NULL else {paste("\t> (event) types = ",object$C,sep="")}
  riskset <- paste("\t> riskset = ",attr(object,"riskset"),sep="")
  if(attr(object,"riskset")=="active"){
    riskset <- c(riskset,paste("\t\t>> active dyads = ",object$activeD," (full risk set size = ",object$D," dyads)",sep=""))
  }
  directed <- paste("\t> directed = ",attr(object,"directed"),sep="")
  ordinal <- paste("\t> ordinal = ",attr(object,"ordinal"),sep="")
  weighted <- paste("\t> weighted = ",attr(object,"weighted"),sep="")
  time_length <- NULL
  time <- object$edgelist$time
  origin <- attr(object, "time")$origin
  if(!attr(object,"ordinal")){
    time_length <- time[length(time)] - attr(object,"origin")
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
#' 
#' @return displays the same information provided by the summary method.
#' 
#' @method print remify
#' 
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
#' 
#' @return vector of dimensions of the processed event sequence.
#' 
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
  if(is.null(x$E)){
    if(attr(x,"with_type")){
      dimensions <- c(x$M, x$N, x$C, x$D)
      names(dimensions) <- c("events","actors","types","dyads")
    }
    else{
      dimensions <- c(x$M, x$N, x$D)
      names(dimensions) <- c("events","actors","dyads") 
    }
  }
  else{
    if(attr(x,"with_type")){
      dimensions <- c(x$E, x$M, x$N, x$C, x$D)
      names(dimensions) <- c("events","time points","actors","types","dyads")
    }
    else{
      dimensions <- c(x$E, x$M, x$N, x$D)
      names(dimensions) <- c("events","time points","actors","dyads") 
    } 
  }
  if(attr(x,"riskset")=="active"){
    dimensions <- c(dimensions, "dyads(active)" = x$activeD)
  }
  return(dimensions)
}

#######################################################################################
#######################################################################################

#' @title getRiskset
#' @description This function returns the processed risk set changes specified by the input `omit_dyad`. In such a matrix: value 1 refers to the dyads in the risk set, and 0 otherwise (dyads excluded from the risk set). All the possible risk set modifications are described by row, and the columns identify the dyads. Note: This matrix is the output given by processing the input `omit_dyad`, and the number of rows might be equal to or higher than the number of objects in `omit_dyad`. This might happen because more than one modification of the risk set defined in the input could overlap over time with others. For more details about how the risk set is processed, see \code{vignette(package="remify",topic="riskset")}.
#' @param x a \code{remify} object.
#' 
#' @return list of objects describing the processed the risk set.
#' 
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
#' 
#' @return character vector of actors' names.
#' 
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
#' 
#' @return character vector of types' names.
#' 
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
#' @param active logical, whether to consider the input \code{dyadID} as a vector of ID's of active dyads (\code{active = TRUE}) or dyads from the full risk set (\code{active = FALSE}).
#' 
#' @return a data.frame with "actor1", "actor2" and "type" names corresponding to the vector \code{dyadID}.
#' 
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
      dyadID_full[d] <- attr(x,"dyadID")[which(attr(x,"dyadIDactive")==dyadID[d])[1]]
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
#' 
#' @return actor ID as integer value.
#' 
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
#' 
#' @return type ID as integer value.
#' 
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
#' 
#' @return dyad ID as integer value.
#' 
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
    select_loc <- which(attr(x,"dyadID")==dyad_id)[1] # selecting first occurrence of dyad_id in attr(x,ID"dyad")
    if(length(select_loc > 0)){
      dyad_id <- c(dyad_id,ifelse(length(select_loc>0),attr(x,"dyadIDactive")[select_loc],NA))
      names(dyad_id) <- c("dyadID","dyadIDactive")
    }
  }                                 

  return(dyad_id)
}

#######################################################################################
#######################################################################################

#' Generic plot method
#'
#' @title plot.remify
#' @rdname plot.remify
#' @description several plots that describe the network of relational events, both for directed and undirected relational events.
#' @param x is a \code{remify} object.
#' @param which one or more numbers between 1 and 5. Plots described in order: (1) distribution of the inter-event times (histogram), (2) tile plot titled 'activity plot', with in-degree and out-degree activity line plots on the sides (or total-degree on the top side if the network is undirected). Tiles' color is scaled based on the count of the directed (or undirected) dyad, (3) for directed networks two plots of normalized out-degree and in-degree (values ranging in [0,1]) over a set of \code{n_intervals} (evenly spaced). For undirected networks one plot of normalized total-degree over the \code{n_intervals} (also here values ranging in [0,1]). The normalization is calculated in each interval as the \code{(degree-min(degree))/(max(degree)-min(degree)))} for each actor considering minimum and maximum degree (in-, out- or total-) observed in the interval (opacity and size of the points is proportional to the normalized measure), (4) four plots: (i) number of events (# events) per time interval, (ii) proportion of observed dyads (# dyads / x$D) per time interval, (iii) and (iv) (for directed network only) proportion of active senders and receivers per time interval (calculated as # senders/ x$N and # receiver/x$N per interval), (5) two networks: (i) network of events where edges are considered undirected (edges' opacity is proportional to the counts of the undirected events, vertices' opacity is proportional to the total-degree of the actors), (ii) visualization of directed network (edges' opacity is proportional to the counts of the directed events, vertices' opacity is proportional to the in-degree of the actors).
#' @param breaks default is \code{15L} and it describes the number of cells of the histogram plot for the inter-event times. It  can be specified in the same way as the argument used by the function \code{graphics::hist()} (see ?graphics::hist for more details).
#' @param palette a palette from \code{grDevices::hcl.pals()} (default is the \code{"Purples"} palette).
#' @param n_intervals number of time intervals for time plots (default is \code{10}).
#' @param rev default is TRUE (reverse order of the color specified in \code{palette})
#' @param actors default is the set of actors in the network (see \code{attr(x,"dictionary")[["actors"]]}). The user can specify a subset of actors on which to run the descriptive plots. If the set contains more than 50 actors, then the function will select the 50 most active actors from the set provided.
#' @param pch.degree default is 20. Shape of the points for the degree plots (in-degree, out-degree, total-degree).
#' @param igraph.edge.color color of the edges in visualization of the network with vertices and nodes. The user can specify the hex value of a color, the color name or use the function\code{grDevices::rgb()} which returns the hex value.
#' @param igraph.vertex.color color of the vertices in visualization of the network with vertices and nodes. The user can specify the hex value of a color, the color name or use the function \code{grDevices::rgb()} which returns the hex value.
#' @param ... other graphical parameters
#' 
#' @return no return value, called for plotting descriptives on the relational event history data.
#' 
#' @method plot remify
#' @export
plot.remify <- function(x,
                    which = c(1:5),
                    breaks= 15L,
                    palette = "Purples",
                    n_intervals = 4L,
                    rev = TRUE,
                    actors = attr(x,"dictionary")$actors$actorName,
                    pch.degree = 20,
                    igraph.edge.color = "#4daa89",
                    igraph.vertex.color = "#5AAFC8",
                    ...){

  # checks on input arguments

  # which plot
  selected <- which
  which <- rep(FALSE,5)
  which[selected] <- TRUE

  # breaks, palette, n_intervals and rev
  if(is.null(breaks)) breaks <- 15L
  if(is.null(palette)) palette <- "Purples" else palette <- as.character(palette)
  if(is.null(n_intervals)) n_intervals <- 3L else n_intervals <- as.integer(n_intervals)
  if(is.null(rev)) rev <- TRUE else rev <- as.logical(rev)
  
  # pch.degree
  if(is.null(pch.degree)){
    pch.degree <- 20
  }
  else{
    if((pch.degree > 25) | (pch.degree < 1)){
      pch.degree <- 20
    }
    pch.degree <- as.integer(pch.degree)
  } 

  # igraph.edge.color and igraph.vertex.color
  if(is.null(igraph.edge.color)){
    igraph.edge.color <- "#4daa89"
  }
  if(is.null(igraph.vertex.color)){
    igraph.vertex.color <- "#5AAFC8"
  }
  if(substr(igraph.edge.color,1,1) != "#"){
    # if it is not an hex color then convert it to hex
    edge_rgb <- grDevices::col2rgb(col=igraph.edge.color)
    igraph.edge.color <- rgb(red = edge_rgb[1]/255,green = edge_rgb[2]/255, blue = edge_rgb[3]/255)
  }
  if(substr(igraph.vertex.color,1,1) != "#"){
    vertex_rgb <- grDevices::col2rgb(col=igraph.vertex.color)
    igraph.vertex.color <- rgb(red = vertex_rgb[1]/255,green = vertex_rgb[2]/255, blue = vertex_rgb[3]/255)
  }
  if(nchar(igraph.edge.color) != 7) igraph.edge.color <- "#4daa89"
  if(nchar(igraph.vertex.color) != 7) igraph.vertex.color <- "#5AAFC8"

  # storing some importnat information in advance
  dict <- attr(x,"dictionary")
  ordinal <- attr(x,"ordinal")

  # actors input
  if(is.null(actors)){
    actors <- dict$actors$actorName
  }
  else{
    # check if 'actors' contains names that are not in the dictionary
    actors <- sort(unique(as.character(actors)))
    check_actors <- sapply(actors , function(y) y %in% dict$actors$actorName) 
    if(any(!check_actors)){
      stop("one or more actors' names ('actors') are not found in the remify object 'x'.")
    }
    rm(check_actors)
  }

  # limiting the number of actors to N = 50 (when x$N > 50)
  if((x$N > 50) & (length(actors)>50)){ 
    if(length(actors) < x$N){ 
      # only when length(actors)>50 but less than x$N, the function does its best by selecting the 50 most active actors out of the subset defined by the input "actors"
      events_to_select <- which((x$edgelist$actor1 %in% actors) & (x$edgelist$actor2 %in% actors))
      if(length(events_to_select) == 0){
          stop("no events found when selecting the set of actors (supplied via the argument 'actors').")
      }
      x$edgelist <- x$edgelist[events_to_select,]
      rm(events_to_select)
    }

    # selecting the first 50 most active actors (starting from the most frequent dyads)
    dyads_freq <- table(apply(x$edgelist,1,function(x) paste(x[2],x[3],sep="_")))
    dyads_freq_sorted <- sort(dyads_freq,decreasing = TRUE)
    actors_loc <- 0
    d_loc <- 25
    actors_until_d_loc <- NULL
    while(actors_loc < 50){
        actors_until_d_loc <-sapply(names(dyads_freq_sorted[1:d_loc]), function(y) (unlist(strsplit(x = y,  split = "_",fixed=TRUE))))
        actors_loc <- length(unique(as.vector(actors_until_d_loc)))
        d_loc <- d_loc + 1
       
    }
    # updating vector of actors' names after selecting the 50 most active actors
    actors <- sort(unique(as.vector(actors_until_d_loc)))

    # finding vector of events including only the 50 most active actors and sub-setting the x$edgelist
    events_to_select <- which((x$edgelist$actor1 %in% actors) & (x$edgelist$actor2 %in% actors))
    x$edgelist <- x$edgelist[events_to_select,] 
    x$N <- actors_loc # can be 49-50-51, not always exaclty 50
    x$D <- ifelse(attr(x,"directed"),x$N*(x$N-1),x$N*(x$N-1)/2)

    # free-ing space
    rm(dyads_freq,dyads_freq_sorted,actors_loc,d_loc,actors_until_d_loc,events_to_select)

    # display warning to the UI about the subset of actors
    warning(paste("Too many actors for rendering plots with a good quality: the 50 most active actors are selected (descriptives on dyads and actors may differ from the descriptives conducted on the whole set of actors)"))
  }
  else{ # when x$N <= 50
    if(length(actors) < x$N){ 
      # when length(actors) is less than x$N (analysis on a subset of actors that is smaller than 50 actors)
      x$N <- length(actors)
      x$D <- ifelse(attr(x,"directed"),x$N*(x$N-1),x$N*(x$N-1)/2)
      events_to_select <- which((x$edgelist$actor1 %in% actors) & (x$edgelist$actor2 %in% actors))     
      if(length(events_to_select) == 0){
        stop("no events found when selecting the set of actors (supplied via the argument 'actors').")
      }
      x$edgelist <- x$edgelist[events_to_select,]
      rm(events_to_select)
    }
  }
  
  # dyads, in-degree, out-degree and total-degree
  dyads <- paste(x$edgelist$actor1,x$edgelist$actor2,sep="_")
  if(attr(x,"directed")){
      in_degree <- table(factor(x$edgelist$actor2,levels=actors))
      out_degree <- table(factor(x$edgelist$actor1,levels=actors))
  }
  else{
      total_degree <- table(factor(c(x$edgelist$actor1,x$edgelist$actor2),levels=actors))
  }

  if((which[4L]  & !attr(x,"directed")) | any(which[c(2L,5L)])){
    # X_dyad is used by plot 5 and total_degree is used by plot 2 when the network is undirected
    # [incomplete] matrix of dyad frequencies by [actor1-actor2] with no direction (undirected) taken into account (some dyads may not be included at this stage, only observed ones are included)
    cl <- parallel::makeCluster(attr(x,"ncores"))
    dyad_no_direction_freq <-parallel::parApply(cl = cl, X = x$edgelist, MARGIN = 1,FUN = function(l) {
      dyad_l <- sort(as.character(c(l[2],l[3])))
      paste(dyad_l[1],dyad_l[2],sep="_")
      })
    parallel::stopCluster(cl)
    if(which[4L]  & !attr(x,"directed")){
      dyads <- dyad_no_direction_freq
    }
  }

  if(any(which[c(2L,5L)])){
    # ... frequencies dyads (sorted from large to small frequency values) 
    dyad_freq <- table(dyads)

    # [incomplete] matrix of dyad frequencies by [actor1-actor2] (some dyads may not be included at this stage, only observed one are included)
    if(attr(x,"directed")){
      X <- matrix(NA, nrow=length(dyad_freq),ncol=3) 
      X[,3] <- as.integer(dyad_freq)
      for(d in 1:length(dyad_freq)){
        X[d,1:2] <- unlist(strsplit(x = names(dyad_freq[d]),  split = "_"))
      }
      X <- X[order(X[,1],X[,2]),]
    }

    dyad_no_direction_freq <- table(dyad_no_direction_freq) 
    X_dyad <- matrix(NA, nrow=length(dyad_no_direction_freq),ncol=3) 
    X_dyad[,3] <- as.integer(dyad_no_direction_freq)
    for(d in 1:length(dyad_no_direction_freq)){
      X_dyad[d,1:2] <- unlist(strsplit(x = names(dyad_no_direction_freq[d]),split = "_"))
    }
    X_dyad <- X_dyad[order(X_dyad[,1],X_dyad[,2]),]
    rm(dyad_no_direction_freq)
    if(!attr(x,"directed")){ 
      # only for undirected networks (if undirected, X and X_dyad are the same)
      X <- X_dyad
    }
  }

  if(any(which[c(3L,4L)])){
    time <- x$edgelist$time
    time <- cut(time,breaks = n_intervals)
    y <- tapply(X = x$edgelist$actor1, INDEX = time, FUN = length)
    y[is.na(y)] <- 0
  }

  # important settings for graphics parameters after running the plot function
  # disabling dynamic pages for remify 3.2.0
  #ask_new_page <- devAskNewPage(TRUE)
  #on.exit(expr = devAskNewPage(ask_new_page))
  op <- par(no.readonly = TRUE)
  on.exit(expr = par(op))
  
  ## [[plot 1]] plotting histogram of the waiting times:
    # y-axis = Frequency (freq=TRUE)
    # x-axis = measurement unit of the waiting time
  if(which[1L] & !ordinal){
    time_unit <- NULL
    time_unit <- attr(x$intereventTime,"unit") # [[CHECK!!]] add attribute to x$intereventTime object based on the time scale (default is seconds if time is timestamp, or days if it is a Date )
    #dev.hold()
    hist(x = x$intereventTime, 
      breaks = breaks,
      angle = 45, 
      col = "lavender", 
      border = "darkgray",
      main = paste("Distribution of the inter-event times",collapse=""),
      xlab = ifelse(!is.null(time_unit),paste("waiting time (",time_unit,")",sep="", collapse=""),paste("waiting time")),
      freq = TRUE)
    #dev.flush()
  }

  # [[plot 2]] two-way table of event counts (with marginal distributions on actor1 and actor2, or total degree if undirected network): actor- and tie- oriented networks
  if(which[2L]){
    # actors that interacted either as 'sender' or 'receiver' (or 'actor1' and 'actor2' in case of undirected networks)
    egrid <- expand.grid(actors,actors)
    egrid <- egrid[,2:1]
    if(attr(x,"directed")){
      tile_plot_x_axis_name <- "receiver"
      tile_plot_y_axis_name <- "sender"
      upper_plot_y_axis_name <- "in-degree"
    }
    else{
      tile_plot_x_axis_name <- "actor2"
      tile_plot_y_axis_name <- "actor1"
      upper_plot_y_axis_name <- "total-degree"
    }

    # [complete] matrix of dyad frequencies
    X_out <- data.frame(row = egrid[,1],col = egrid[,2], fill = NA)  
    for(d in 1:dim(X)[1]){
      row_index <- which((X_out$row == X[d,1]) & (X_out$col == X[d,2]))
      X_out$fill[row_index] <- as.integer(X[d,3])
    }
    
    # assigning actors positions on the grid
    X_out[,1:2] <- expand.grid(1:length(actors),1:length(actors)) # for positioning actors on the grid
    X_out[,1:2] <- X_out[,2:1]

    # setting up axes measures
    if(attr(x,"directed")){
      max_upper_plot <- max(unname(in_degree))+2 
      min_upper_plot <- min(unname(in_degree))-1 
      max_out_degree <- max(unname(out_degree))+2 
      min_out_degree <- min(unname(out_degree))-1 
      total_or_in_degree <- in_degree
    }
    else{ 
      # when the network is undirected use 'total_degree' but keep same name to avoid redundant code
      max_upper_plot <- max(unname(total_degree))+2 
      min_upper_plot <- min(unname(total_degree))-1 
      total_or_in_degree <- total_degree
    }

    # creating layout and setting up graphical parameters 
    if(attr(x,"directed")){
      layout_matrix <- matrix(c(3,2,1,4), ncol=2, byrow=TRUE)
    }
    else{
      layout_matrix <- matrix(c(3,2,1,2), ncol=2, byrow=TRUE)
    }
    colors_legend <- unique(sort(X_out$fill))
    bottom_and_left_mai <- max(strwidth(actors, "inch")+0.4, na.rm = TRUE)
    #dev.hold()
    layout(layout_matrix, widths=c(4/5,1/5), heights=c(1/5,4/5))
    par(oma=c(2,2,2,2))
    par(mar =c(6,6,1,1))
    par(mgp=c(6,1,0)) 

    # [1] tile plot
    plot.new()
    plot.window(xlim=c(1,x$N),ylim=c(1,x$N))
    with(X_out,{
      rect(col-0.5,row-0.5,col+0.5,row+0.5,col=hcl.colors(n=max(unique(sort(fill))),palette=palette,rev=rev)[fill],border="#ffffff") 
      segments(x0=c(1:x$N)+0.5,y0=c(1:x$N)-0.5,x1=c(1:x$N)-0.5,y1=c(1:x$N)+0.5,col="gray")
      segments(x0=0.5,y0=0.5,x1=(x$N+0.5),y1=(x$N+0.5),col="gray")
      # actor names
      text(x = c(1:x$N), y = 0, labels = actors, srt = 90, pos = 1, xpd = TRUE,  adj = c(0.5,1), offset = 2.5) 
      text(x = 0, y = c(1:x$N), labels = actors, srt = 0, pos = 2, xpd = TRUE,  adj = c(1,0.5), offset = 0)
      # axes names 
      mtext(text  = tile_plot_x_axis_name, side=1, line=5, outer=FALSE, adj=0, at=floor(x$N/2))
      mtext(text = tile_plot_y_axis_name, side=2, line=5, outer=FALSE, adj=1, at=floor(x$N/2))
    })

    # [2] legend of tile plot
    par(mar=c(0,0,1,1))
    plot(0, 0, type="n", xlim = c(0, 5), ylim = c(0, 7), axes = FALSE, xlab = "", ylab = "")   
    # colors' legend
    rect(xleft = 2, ybottom = seq(0,5,length=max(colors_legend)), xright = 3, ytop = seq(1.25,6.25,length=max(colors_legend)),col = hcl.colors(n=max(colors_legend),palette=palette,rev=rev)[1:max(colors_legend)], border = NA)
    # borders and ticks
    rect(xleft=2,ybottom=0,xright=3,ytop=6.25)
    segments(x0=c(2,2.8),y0=rep(seq(0,6.25,length=3)[2],2),x1=c(2.2,3))
    text(x = rep(3.2,3) , y = seq(0.1,6.25,length=3), labels = c(1,floor(median(colors_legend)),max(colors_legend)), adj = c(0,0.5))
    text(x = 2.5, y = 6.6, labels = "events",adj =c(0.5,0),cex=1.25)

    # [3] line plots in-degree
    par(mar=c(0,6,1,1))
    plot(x=1:x$N, type = 'n', xlim = c(1,x$N), ylim = c(min_upper_plot,max_upper_plot),axes=FALSE,frame.plot=FALSE,xlab="",ylab="")
    title(ylab=upper_plot_y_axis_name, line = 5)
    abline(v=seq(1,x$N,by=1),col = "gray", lty = "dotted", lwd = par("lwd"))
    segments(x0=seq(1,x$N,by=1),y0=0,y1=total_or_in_degree,lwd=2,col="cadetblue3")
    points(x=seq(1,x$N,by=1),y=total_or_in_degree,type="p",pch=19,cex=1,col="cadetblue3")
    axis(side=2)

    # [4] line plots out-degree
    if(attr(x,"directed")){
      par(mar=c(6,0,1,1))
      plot(x = seq(min_out_degree,max_out_degree,length=x$N), y = 1:x$N, type = 'n', xlim = c(min_out_degree,max_out_degree), ylim = c(1,x$N),axes=FALSE,frame.plot=FALSE,xlab="",ylab="")
      title(xlab="out-degree", line = 5)
      abline(h=seq(1,x$N,by=1),col = "gray", lty = "dotted", lwd = par("lwd"))
      segments(x0=0,y0=seq(1,x$N,by=1),x1=as.vector(unname(out_degree)),lwd=2,col="cadetblue3")
      points(x=as.vector(unname(out_degree)),y=seq(1,x$N,by=1),type="p",pch=19,cex=1,col="cadetblue3")
      axis(side=1)
      title(main="Activity plot",outer=TRUE)
      par(op)
    }
    #dev.flush()
  }

  ## [[plot 3]] normalized degrees (per actor and per intervals):
    # for directed networks: normalized in-degree and out-degree
    # for undirected networks: normalized total-degree
    # normalization means that per each intervals the in-/out-/total- degree of each actor is normalized for the minimum and maximum observed value of in-/out-/total- degree across all the actors. The normalization is carried out as follows: (degree-min(degree))/(max(degree)-min(degree))
  if(which[3L]){
    if(attr(x,"directed")){
      # out-degree plot
      tab_s <- tapply(X =x$edgelist$actor1, INDEX = time, FUN = function(w) table(w))
      #dev.hold()
      par(mfrow=c(1,1))
      left_mai <- max(strwidth(actors, "inch")+0.4, na.rm = TRUE)
      bottom_mai <- max(strwidth(labels(time), "inch")+2, na.rm = TRUE)
      par(mai =c(bottom_mai,left_mai,1,1))
      plot(1:length(y),rep(2*x$N,length(y)), type = "n", ylab = "", xlab = "time interval", ylim = c(0,2*x$N), xaxt = "n", yaxt = "n",)  
      axis(side = 1, at = c(1:length(tab_s)))
      axis(side = 2, at = seq(1,2*x$N,by=2), labels = as.character(actors),las=2)
      abline(h = seq(0,2*x$N,by=2), lty=2, col="gray")
      for(l in 1:length(tab_s)){
        if(!is.null(tab_s[[l]])){
          y_loc <- names(tab_s[[l]])
          y_loc <- sapply(1:length(y_loc),function(x) x + (x-1) )
          scaled_y <-  (as.numeric(tab_s[[l]])-min(as.numeric(tab_s[[l]])))/(max(as.numeric(tab_s[[l]]))-min(as.numeric(tab_s[[l]])))
          if(any(is.nan(scaled_y))) scaled_y[is.nan(scaled_y)] <- 1.0
          points(rep(l,length(tab_s[[l]])),y_loc,type="p",pch=pch.degree,cex=3*scaled_y,col = rgb(red = 80/255, green = 199/255, blue = 199/255, alpha=scaled_y*1))
        } 
      }
      title("(normalized) Out-degree per time interval")
      #dev.flush()

      # in-degree plot
      tab_s <- tapply(X =x$edgelist$actor2, INDEX = time, FUN = function(w) table(w)) 
      #dev.hold()
      plot(1:length(y),rep(2*x$N,length(y)), type = "n", ylab = "", xlab = "time interval", ylim = c(0,2*x$N), xaxt = "n", yaxt = "n",)  
      axis(side = 1, at = c(1:length(tab_s)))
      axis(side = 2, at = seq(1,2*x$N,by=2), labels = as.character(actors),las=2)
      abline(h = seq(0,2*x$N,by=2), lty=2, col="gray")
      for(l in 1:length(tab_s)){
        if(!is.null(tab_s[[l]])){
          y_loc <- names(tab_s[[l]])
          y_loc <- sapply(1:length(y_loc),function(x) x + (x-1) )
          scaled_y <- (as.numeric(tab_s[[l]])-min(as.numeric(tab_s[[l]])))/(max(as.numeric(tab_s[[l]]))-min(as.numeric(tab_s[[l]])))
          if(any(is.nan(scaled_y))) scaled_y[is.nan(scaled_y)] <- 1.0
          points(rep(l,length(tab_s[[l]])),y_loc,type="p",pch=pch.degree,cex=3*scaled_y,col = rgb(red = 199/255, green = 121/255, blue = 80/255, alpha=scaled_y*1))
        }
      }
      title("(normalized) In-degree per time interval")  
      #dev.flush()
      par(op)
    }
    else{ # for undirected networks
      # total-degree plot
      time_s <- rep(x$edgelist$time,each=2)
      time_s <- cut(time_s,breaks = n_intervals)
      tab_s <- tapply(X =as.vector((t(as.matrix(x$edgelist[,2:3])))), INDEX = time_s, FUN = function(w) table(w))
      #dev.hold()
      par(mfrow=c(1,1))
      left_mai <- max(strwidth(actors, "inch")+0.4, na.rm = TRUE)
      bottom_mai <- max(strwidth(labels(time), "inch")+2, na.rm = TRUE)
      par(mai =c(bottom_mai,left_mai,1,1))
      plot(1:length(y),rep(2*x$N,length(y)), type = "n", ylab = "", xlab = "time interval", ylim = c(0,2*x$N), xaxt = "n", yaxt = "n",)  
      axis(side = 1, at = c(1:length(tab_s)))
      axis(side = 2, at = seq(1,2*x$N,by=2), labels = as.character(actors),las=2)
      abline(h = seq(0,2*x$N,by=2), lty=2, col="gray")
      for(l in 1:length(tab_s)){
        if(!is.null(tab_s[[l]])){
          y_loc <- names(tab_s[[l]])
          y_loc <- sapply(1:length(y_loc),function(x) x + (x-1) )
          scaled_y <-  (as.numeric(tab_s[[l]])-min(as.numeric(tab_s[[l]])))/(max(as.numeric(tab_s[[l]]))-min(as.numeric(tab_s[[l]])))
          if(any(is.nan(scaled_y))) scaled_y[is.nan(scaled_y)] <- 1.0
          points(rep(l,length(tab_s[[l]])),y_loc,type="p",pch=pch.degree,cex=3*scaled_y,col = rgb(red = 80/255, green = 199/255, blue = 199/255, alpha=scaled_y*1)) 
        } 
      }
      title("(normalized) Total-degree per time interval")
      #dev.flush()
    }
  }

  # [[plot 4]] plots: quantity per time interval
    ## [1] (# events) per time interval
    ## [2] (proportion of active dyads, as observed dyads / D) per time interval
  # if directed network:  
    ## [3] (proportion of active senders, as observed senders / N) per time interval
    ## [4] (proportion of active receivers, as observed receivers / N) per time interval
  if(which[4L]){
    # arranging layout
    layout.matrix <- NULL
    if(attr(x,"directed")){
    layout.matrix <- matrix(c(1,3,2,4), nrow = 2, ncol = 2)
    }
    else{
    layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
    }
    #dev.hold()
    layout(mat = layout.matrix)
    df_spline <- floor(sqrt(n_intervals))
    # [1] plotting (# events) per time interval
    type_plot <- ifelse(n_intervals>3,"p","b")
    plot(1:length(y),y,type=type_plot,cex=0.8,col="#939393",ylab = "# events",xlab = "time interval", xaxt="n", lwd = 1.5, main="Number of events (# events) per time interval")
    if(n_intervals>3) lines(smooth.spline(x=c(1:length(y)),y=as.numeric(y),df=df_spline),col="#cd0303",lwd=2)

    # [2] plotting (proportion of active dyads) per time interval
    prop_dyads <- tapply(X = dyads, INDEX = time, FUN = function(l) length(unique(l))) 
    prop_dyads[is.na(prop_dyads)] <- 0
    prop_dyads <- prop_dyads/x$D
    plot(prop_dyads,type=type_plot,cex=0.8,col="#939393",ylab = "active dyads (observed/total)",xlab = "time interval", xaxt="n", lwd = 1.5, main="Active dyads (observed/total) per time interval")
    if(n_intervals>3) lines(smooth.spline(x=c(1:length(y)),y=as.numeric(prop_dyads),df=df_spline),col="#cd0303",lwd=2)
    
    if(attr(x,"directed")){
      # [3] plotting (proportion of active senders) per time interval   
      s <- tapply(X = x$edgelist$actor1, INDEX = time, FUN = function(l) length(unique(l))) 
      s[is.na(s)] <- 0
      s <- s/x$N
      plot(s,type=type_plot,cex=0.8,col="#939393",ylab = "active senders (observed/total)",xlab = "time interval", xaxt="n", lwd = 1.5,main="Active senders (observed/total) per time interval") 
      if(n_intervals>3) lines(smooth.spline(x=c(1:length(y)),y=as.numeric(s),df=df_spline),col="#cd0303",lwd=2)
      # [4] plotting (proportion of active receivers) per time interval   
      s <- tapply(X = x$edgelist$actor2, INDEX = time, FUN = function(l) length(unique(l))) 
      s[is.na(s)] <- 0
      s <- s/x$N
      plot(s,type=type_plot,cex=0.8,col="#939393",ylab = "active receivers (observed/total)",xlab = "time interval", xaxt="n", lwd = 1.5, main="Active receivers (observed/total) per time interval")  
      if(n_intervals>3) lines(smooth.spline(x=c(1:length(y)),y=as.numeric(s),df=df_spline),col="#cd0303",lwd=2)
    }
    #dev.flush()
  }


  # [[plot 5]] network visualization as nodes and edges
  if(which[5L]){
    requireNamespace(package="igraph",quietly=TRUE)
    X_dyad[is.na(X_dyad)] <- 0
    # [1] network of undirected dyad intensity 
    # defining nodes and links
    popularity_table <- table(factor(c(x$edgelist$actor1,x$edgelist$actor2),levels=actors))
    nodes <- data.frame(name= names(popularity_table),popularity =as.vector(popularity_table)) # popularity = total in- and out-degree
    rm(popularity_table)
    links <- data.frame(from = as.character(X_dyad[,1]),to = as.character(X_dyad[,2]), weight = as.numeric(X_dyad[,3]))
    # creating a graph_from_data_frame
    net_undirected <- igraph::graph_from_data_frame(d=links, vertices=nodes, directed=TRUE) 

    # setting up vertex and edge attributes
    popularity_scale <- ((nodes$popularity-min(nodes$popularity))/(max(nodes$popularity)-min(nodes$popularity)))
    popularity_scale[is.na(popularity_scale)] <- 0
    links_weight <- (links$weight-min(links$weight))/(max(links$weight)-min(links$weight))
    links_weight[is.na(links_weight)] <- 0
    igraph::V(net_undirected)$size <- 10 
    # defining transparency of vertices color based on their popularity (in-degree)
    transparency_popularity_levels <- sapply(round(255*popularity_scale), function(l) {
    hc <-rgb(0,0,0, alpha = l,maxColorValue = 255)
    substr(x=hc, nchar(hc)-1, nchar(hc))
    })
    igraph::V(net_undirected)$color <- sapply(transparency_popularity_levels,function(l) paste(igraph.vertex.color,l,sep=""))
    igraph::V(net_undirected)$frame.color <- NA
    igraph::E(net_undirected)$arrow.mode <- 0
    igraph::E(net_undirected)$width <- 2
    # defining transparency of edges color based on count
    transparency_links_weight_levels <- sapply(round(255*links_weight), function(l) {
    hc <-rgb(0,0,0, alpha = l,maxColorValue = 255)
    substr(x=hc, nchar(hc)-1, nchar(hc))
    })
    igraph::E(net_undirected)$color <- sapply(transparency_links_weight_levels,function(l) paste(igraph.edge.color,l,sep="")) 
    igraph::E(net_undirected)$curved <- 0.1
    igraph::graph_attr(net_undirected, "layout") <- igraph::layout_with_lgl
    if(attr(x,"directed")){
      X[is.na(X)] <- 0
      # [2] network of directed dyad intensity
      # defining nodes and links
      popularity_table <- table(factor(x$edgelist$actor2,levels=actors))
      nodes <- data.frame(name= names(popularity_table),popularity =as.vector(popularity_table)) # popularity = total in-degree
      rm(popularity_table)
      links <- data.frame(from = as.character(X[,1]),to = as.character(X[,2]), weight = as.numeric(X[,3]))

      # creating a graph_from_data_frame
      net_directed <- igraph::graph_from_data_frame(d=links, vertices=nodes, directed=TRUE) 

      # setting up vertex and edge attributes
      popularity_scale <- ((nodes$popularity-min(nodes$popularity))/(max(nodes$popularity)-min(nodes$popularity)))
      links_weight <- (links$weight-min(links$weight))/(max(links$weight)-min(links$weight))
      igraph::V(net_directed)$size <- 10 
      # defining transparency of vertices color based on their popularity (in-degree)
      transparency_popularity_levels <- sapply(round(255*popularity_scale), function(l) {
      hc <-rgb(0,0,0, alpha = ifelse(is.na(l),0,l),maxColorValue = 255)
      substr(x=hc, nchar(hc)-1, nchar(hc))
      })
      igraph::V(net_directed)$color <- sapply(transparency_popularity_levels,function(l) paste(igraph.vertex.color,l,sep=""))
      igraph::V(net_directed)$frame.color <- NA
      igraph::E(net_directed)$arrow.size <- 0.3
      igraph::E(net_directed)$width <- 2
      # defining transparency of edges color based on count
      transparency_links_weight_levels <- sapply(round(255*links_weight), function(l) {
      hc <-rgb(0,0,0, alpha = ifelse(is.na(l),0,l),maxColorValue = 255)
      substr(x=hc, nchar(hc)-1, nchar(hc))
      })
      igraph::E(net_directed)$color <- sapply(transparency_links_weight_levels,function(l) paste(igraph.edge.color,l,sep="")) 
      igraph::E(net_directed)$curved <- 0.1
      igraph::graph_attr(net_directed, "layout") <- igraph::layout_with_lgl
      # plotting network
      #dev.hold()
      par(mfrow=c(1,2))
      plot(net_undirected, main = "Network of events (undirected edges)")
      plot(net_directed, main = "Network of events (directed edges)")
      #dev.flush()
    }
    else{
      # plotting network
      #dev.hold()
      par(mfrow=c(1,1))
      plot(net_undirected, main = "Network of events (undirected edges)")
      #dev.flush()
    }
  }
}

#######################################################################################
#######################################################################################


#######################################################################################
#######################################################################################
###########(END)             Methods for `remify` object             (END)#############
#######################################################################################
#######################################################################################

