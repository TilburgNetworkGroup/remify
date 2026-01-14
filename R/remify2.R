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
remify2 <- function(edgelist,
                   directed = TRUE,
                   ordinal = FALSE,
                   model = c("tie","actor"),
                   actors = NULL,
                   types = NULL,
                   riskset = c("full","active","manual"),
                   origin = NULL,
                   omit_dyad = NULL,
                   ncores = 1L
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

  if((model == "actor") & (directed == FALSE)){
    stop("actor-oriented model can only work with directed networks")
  }

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

  if(is.null(riskset)){
    riskset <- "full"
  }

  riskset  <- match.arg(arg = riskset, choices = c("full", "active", "manual"), several.ok = FALSE)
  active <- FALSE
  if(riskset == "active"){
    active <- TRUE
  }

  # Checking for NA's

  ## NA's in `edgelist` :
  NA_time <- which(is.na(edgelist$time))
  NA_actor1 <- which(is.na(edgelist$actor1))
  NA_actor2 <- which(is.na(edgelist$actor2))
  NA_type <- which(is.na(edgelist$type))
  NA_weight <- which(is.na(edgelist$weight))
  to_remove <- c(NA_time,NA_actor1,NA_actor2,NA_type,NA_weight)
  if(length(to_remove)>0){
    to_remove <- unique(to_remove)
    warning("`edgelist` contains missing data: incomplete events are dropped.") # `edgelist` contains missing data: incomplete events (rows) are dropped.
    if(length(to_remove) == dim(edgelist)[1]){
      stop("`edgelist` object is empty.")
    }
    edgelist <- edgelist[-to_remove,]
    rm(to_remove)
  }

  # Pre-processing relational event history (remifyCpp.cpp)
  out <- tryCatch(remifyCpp2(input_edgelist = edgelist,
                            actors = actors,
                            types = types,
                            directed = directed,
                            ordinal = ordinal,
                            origin = origin,
                            omit_dyad = omit_dyad,
                            model = model,
                            active = active,
                            ncores = ncores),error= function(e) e)

  # handling potential errors coming from C++  - stopping function
  if(any(class(out) %in% c("error"))){
    stop(out$message)
  }

  # handling warning messages on R-console
  if(any(names(out) %in% "warnings")){
    if(length(out$warnings)>=1)
      for(w in 1:length(out$warnings)){
        warning(out$warnings[[w]])
      }
  }
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
  attr(str_out, "ncores") <- ncores
  # instead of using attributes, make them list elements
  str_out$meta <- list(
    with_type = out$with_type,
    weighted = out$weighted,
    directed = directed,
    ordinal = ordinal,
    model = model, # useful because tie and actor models have two different ways for handling changing risksets
    riskset = riskset,
    dictionary = list(actors = out$actorsDictionary, types = out$typesDictionary),
    origin = out$edgelist$time[1]-out$intereventTime[1],
    ncores = ncores
  )

  # ID of actors, types and dyads
  attr(str_out, "dyadID") <- out$dyad
  attr(str_out,"actor1ID") <- out$actor1_ID
  attr(str_out,"actor2ID") <- out$actor2_ID
  if(out$with_type){
    attr(str_out,"typeID") <- out$type_ID
  }
  # build index (event-level ids)
  str_out$index <- list(
    dyadID   = out$dyad,
    actor1ID = out$actor1_ID,
    actor2ID = out$actor2_ID,
    type   = if (out$with_type) out$type_ID else NULL
  )

  # ID's when riskset = "active"
  if(active){
    str_out$activeD <- out$omit_dyad$D_active
    out$omit_dyad$D_active <- NULL
    if(model == "tie"){
      attr(str_out, "dyadIDactive") <- out$omit_dyad$dyadIDactive
      str_out$index$dyadIDactive <- out$omit_dyad$dyadIDactive
      out$omit_dyad$dyadIDactive <- NULL
    }
  }

  # # store dyad mapping via existing encoder (supports types and active riskset)
  # if (model == "tie") {
  #   if (riskset == "full") {
  #     str_out$index$dyad_map <- remify::getDyad(
  #       x = str_out,
  #       dyadID = seq_len(str_out$D),
  #       active = FALSE
  #     )
  #   } else if (riskset == "active") {
  #     str_out$index$dyad_map_active <- remify::getDyad(
  #       x = str_out,
  #       dyadID = seq_len(as.integer(str_out$activeD)[1]),
  #       active = TRUE
  #     )
  #     if (!is.null(out$omit_dyad$dyadIDactive)) {
  #       str_out$index$dyad_map_active$dyadID <- as.integer(out$omit_dyad$dyadIDactive)
  #       str_out$index$dyadID_full_for_active <- as.integer(out$omit_dyad$dyadIDactive)
  #     }
  #   }
  # }

  str_out$omit_dyad <- out$omit_dyad
  evenly_spaced_interevent_time <- NULL
  rows_to_remove <- NULL
  if(!is.null(out$rows_to_remove)){
    if(!ordinal){
      evenly_spaced_interevent_time <- out$evenly_spaced_interevent_time
    }
    rows_to_remove <- out$rows_to_remove
  }
  out <- NULL # free-ing space [[now]]

  # modifying remify object to account for simultaneous events
  if(!is.null(rows_to_remove)){
    if(!ordinal){
      #attribute intereventTime evenly spaced
      attr(str_out, "evenly_spaced_interevent_time") <- evenly_spaced_interevent_time
      # removing zeros from intereventTime
      str_out$intereventTime <- str_out$intereventTime[-rows_to_remove] # updating interevent time vector
    }
    # saving indices of simultaneous events to be removed (for remstimate)
    attr(str_out, "indices_simultaneous_events") <- rows_to_remove
    #str_out$intereventTime and str_out$omit_dyad$time are processed in remstimate depending on method=c("pe","pt") from remstats
    str_out$E <- str_out$M # number of events
    str_out$M <- str_out$M-length(rows_to_remove) # overwrite (lower) number of time points
    time_unique <- unique(str_out$edgelist$time)

    # tie-oriented modeling
    # also store vectorized versions
    str_out$index$dyadIDactive_vec <- str_out$index$dyadIDactive
    str_out$index$dyadID_vec <- str_out$index$dyadID
    str_out$index$actor1ID_vec <- str_out$index$actor1ID
    str_out$index$actor2ID_vec <- str_out$index$actor2ID
    str_out$index$type_vec <- str_out$index$type
    attr(str_out, "dyadIDactive_vec") <- attr(str_out, "dyadIDactive")
    attr(str_out, "dyadID_vec") <- attr(str_out, "dyadID")
    attr(str_out, "actor1ID_vec") <- attr(str_out, "actor1ID")
    attr(str_out, "actor2ID_vec") <- attr(str_out, "actor2ID")
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
    str_out$index$dyadID <- dyad
    str_out$index$actor1ID <- actor1
    str_out$index$actor2ID <- actor2
    if(active){
      attr(str_out,"dyadIDactive") <- dyadIDactive
      str_out$index$dyadIDactive <- dyadIDactive
    }
    if(attr(str_out, "with_type")){
      attr(str_out,"typeID") <- type
      str_out$index$type <- type
    }
    rm(actor1,actor2,type,dyad,dyadIDactive)
  }

  #store the dyad mapping in remify object to avoid the need to getDyad
  if(active){
    str_out$index$dyad_map_active <- getDyad2(
      x = str_out,
      dyadID = seq_len(as.integer(str_out$activeD)[1]),
      active = TRUE
    )
  }else{
    str_out$index$dyad_map <- getDyad2(
      x = str_out,
      dyadID = seq_len(str_out$D),
      active = FALSE
    )
  }

  return(str_out)
}



getDyad2 <- function(x, dyadID, active = FALSE) {

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
  if (active) {
    dyadIDactive_map <- attr(x, "dyadIDactive")
    if (is.list(dyadIDactive_map)) dyadIDactive_map <- attr(x, "dyadIDactive_vec")
    dyadIDactive_map <- as.integer(dyadIDactive_map)

    dyadID_map_full <- attr(x, "dyadID")
    if (is.list(dyadID_map_full)) dyadID_map_full <- attr(x, "dyadID_vec")
    dyadID_map_full <- as.integer(dyadID_map_full)

    for (d in seq_along(dyadID)) {
      pos <- which(dyadIDactive_map == dyadID[d])[1]
      dyadID_full[d] <- dyadID_map_full[pos]
    }
  }
  composition <- getEventsComposition(dyads = dyadID_full, N = x$N, D = x$D,directed = attr(x,"directed"), ncores  = attr(x,"ncores"))

  # if at least one dyad is not found (<NA>,<NA>,<NA>), then throw warning
  if(any(is.na(composition))){
    warning("one or more dyad ID's can't be found in the remify object 'x': dyad ID's must range between 1 and x$D. NA's are returned for such ID's")
  }

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



