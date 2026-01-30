# helper for %||% (put once near top of file, not inside function ideally)
`%||%` <- function(a, b) if (!is.null(a)) a else b



#' @title Process a Relational Event History
#'
#' @description A function that processes raw relational event history data and returns a S3 object of class 'remify' which is used as input in other functions inside 'remverse'.
#'
#' @param edgelist the relational event history. An object of class \code{\link[base]{data.frame}} with first three columns corresponding to time, and actors forming the dyad. The first three columns will be re-named "time", "actor1", "actor2" (where, for directed networks, "actor1" corresponds to the sender and "actor2" to the receiver of the relational event). Optional columns that can be supplied are: `type` and `weight`. If one or both exist in \code{edgelist}, they have to be named accordingly.
#' @param directed logical value indicating whether events are directed (\code{TRUE}) or undirected (\code{FALSE}). (default value is \code{TRUE})
#' @param ordinal logical value indicating whether only the order of events matters in the model (\code{TRUE}) or also the exact timing must be considered in the model (\code{FALSE}). (default value is \code{FALSE}). If \code{TRUE}, then the column "time" of \code{edgelist} is still used to extract the order.
#' @param model can be "tie" or "actor" oriented modeling. This argument plays a fundamental role when \code{omit_dyad} is supplied. Indeed, when actor-oriented modeling, the dynamic risk set will consist of two risk sets objects (senders' and dyads' risk sets). In the tie-oriented model the function will return a dynamic risk set referred at a dyad-level.
#' @param actors [\emph{optional}] character vector of actors' names that may be observed interacting in the network. If \code{NULL} (default), actors' names will be taken from the input edgelist.
#' @param types [\emph{optional}] character vector of event types that may occur in the network. If \code{NULL} (default), types' names will be taken from the input edgelist.
#' @param riskset [\emph{optional}] character value indicating the type of risk set to process: \code{riskset = "full"} (default) consists of all the possible dyadic events given the number of actors (and the number of event types) and it mantains the same structure over time. \code{riskset = "active"} considers at risk only the observed dyads and it mantains the same structure over time. \code{riskset = "manual"}, allows the risk set to have a structure that is user-defined, and it is based on the instructions supplied via the argument \code{omit_dyad}. This type of risk set allows for time-varying risk set, in which, for instance, subset of actors can interact only at specific time windows, or events of a specific type (sentiment) can't be observed within time intervals that are defined by the user.
#' @param manual.riskset [\emph{optional}] When \code{riskset = "manual"}, this argument of class \code{\link[base]{data.frame}} specifies which dyadic riskset to consider through the entire sequence. If observed dyads from the \code{edgelist} are missing, they will be automatically be added.
#' @param origin [\emph{optional}] starting time point of the observation period (default is \code{NULL}). If it is supplied, it must have the same class of the `time` column in the input \code{edgelist}. If unsupplied, the origin
#' is set to the average waiting time in the sequence subtracted from the time of the first event.
#' @param time.units Character string specifying the time unit for converting time values when `edgelist$time` is of class Date or POSIXct; ignored for numeric or integer time. Default is "secs".
#' @param attach_riskset Logical. If \code{TRUE}, attaches a list \code{riskset_info}
#'   to the returned \code{remify} object. The list contains the effective risk set
#'   representation used for estimation (e.g., \code{riskset_idx}, \code{dyadIDactive},
#'   dictionaries, and basic risk set metadata). This is intended to make the
#'   returned object self-describing and easier to inspect/debug.
#' @param riskset_decode Character. Controls how (and whether) the included risk set
#'   dyads are decoded and attached in \code{riskset_info$included}.
#'   \describe{
#'     \item{\code{"labels"}}{Attach a decoded dyad table including actor (and type)
#'       labels (e.g., \code{actor1}, \code{actor2}, and optional \code{type}).}
#'     \item{\code{"ids"}}{Attach a decoded dyad table with integer IDs only
#'       (e.g., \code{actor1ID}, \code{actor2ID}, optional \code{typeID}, and \code{dyadID}).}
#'     \item{\code{"none"}}{Do not attach a decoded dyad table.}
#'   }
#' @param riskset_max_decode Integer. Maximum number of included dyads (i.e.,
#'   \code{length(riskset_idx)} / \code{D_active}) for which \code{riskset_decode="labels"}
#'   is allowed. If the included risk set exceeds this threshold, decoding to labels
#'   is skipped (typically falling back to \code{"ids"} with a warning) to avoid large
#'   memory usage.
#' @param ncores [\emph{optional}] number of cores used in the parallelization of the processing functions. (default is \code{1}).
#' @param omit_dyad Deprecated. Set to \code{NULL}.
#'
#' @return  'remify' S3 object, list of: number of events (`M`), number of actors (`N`), number of event types (if present, `C`), number of dyads (`D`, and also `activeD` if `riskset="active"`), vector of inter-event times (waiting times between two subsequent events), processed input edgelist as `data.frame`, processed `omit_dyad` object as `list`. The function returns also several attributes that make efficient the processing of the data for future analysis. For more details about the function, input arguments, output, attributes and methods, please read \code{vignette(package="remify",topic="remify")}.
#'
#' @details In \code{omit_dyad}, the \code{NA} value can be used to remove multiple objects from the risk set at once with one risk set modification list. For example, to remove all events with sender equal to actor “A” add a list with two objects \code{time = c(NA, NA)} and \code{dyad = data.frame(actor1 = A, actor2 = NA, type = NA)} to the \code{omit_dyad} list. For more details about
#'
#' @export
#'
#'
#'
remify2 <- function(edgelist,
                   directed = TRUE,
                   ordinal = FALSE,
                   model = c("tie","actor"),
                   actors = NULL,
                   types = NULL,
                   riskset = c("full","active","manual"),
                   manual.riskset = NULL,
                   origin = NULL,
                   time.units = c("auto", "secs", "mins",
                             "hours", "days", "weeks"),
                   attach_riskset = TRUE,
                   riskset_decode = c("labels","ids","none"),
                   riskset_max_decode = 200000L,
                   ncores = 1L,
                   omit_dyad = NULL
){

  # (1) Checking for 'edgelist' input object
  if(!is.null(omit_dyad)) {
    warning("`omit_dyad` is decrecated. It is set to `NULL`.")
  }
  omit_dyad <- NULL

  # Make sure edgelist is a data.frame
  if(!is.data.frame(edgelist)){
    stop("`edgelist` must be of class `data.frame`.")
  }

  riskset_decode <- match.arg(riskset_decode, choices = c("labels","ids","none"))

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
    stop("the class of column `time` in  `edgelist` must be one of the following types: numeric, integer, Date, or POSIXct")
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

  # translate time in edgelist to numeric scale
  time.units <- match.arg(time.units, choices = c("auto", "secs", "mins", "hours", "days", "weeks"))

  t <- edgelist$time

  if (inherits(t, c("POSIXct", "POSIXt", "Date"))) {

    if (is.null(origin)) {
      # compute mean waiting time on the original time scale
      mean.waitingtime <- mean(difftime(t[-1], t[-length(t)], units = time.units), na.rm = TRUE)
      origin <- t[1] - mean.waitingtime
      if (!isTRUE(ordinal)){
        message(paste("Note: origin is set to ", origin))
      }
    }

    edgelist$time <- as.numeric(difftime(t, origin, units = time.units))
    origin <- 0

  } else if (is.numeric(t) || is.integer(t)) {

    # numeric time: difftime is not appropriate
    if (is.null(origin)) {
      origin <- 0
      if (!isTRUE(ordinal)){
        message(paste("Note: origin is set to ", origin))
      }
    }

    edgelist$time <- as.numeric(t - origin)  # unit is whatever the numeric scale is
    origin <- 0

  } else {
    stop("Unsupported class for edgelist$time. Use numeric/integer, Date, or POSIXct/POSIXt.")
  }
  if (isTRUE(ordinal)) {
    # Convert numeric time to an integer step index over unique time values.
    # Events with identical times share the same index; indices increase densely.
    tnum <- edgelist$time

    # Defensive: ensure sorted-by-time assumption holds (optional)
    # If your pipeline guarantees ordering already, you can omit this check.
    if (any(is.na(tnum))) stop("edgelist$time contains NA after time translation.")
    if (is.unsorted(tnum, strictly = FALSE)) {
      warning("edgelist$time is not nondecreasing; ordinal time indexing will follow sorted unique times.")
    }

    edgelist$time <- as.integer(match(tnum, sort(unique(tnum))))

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

  # --- Manual inclusion-based risk set (fixed over time) -----------------------
  # Determine typing from edgelist
  with_type <- ("type" %in% names(edgelist))

  # If typed but types not supplied, infer from edgelist
  if (with_type && is.null(types)) {
    types <- sort(unique(as.character(edgelist$type)))
  }

  if(riskset == "manual")
    {

    if (!is.null(omit_dyad)) {
      stop("Provide either `manual.riskset` or `omit_dyad`, not both.")
    }

    if (!is.data.frame(manual.riskset)) stop("`manual.riskset` must be a data.frame when using a 'manual' riskset.")
    if (!all(c("actor1","actor2") %in% names(manual.riskset))) {
      stop("`manual.riskset` must contain columns `actor1` and `actor2` (and optionally `type`).")
    }

    if (!is.null(actors)) {
      message("Note: `actors` is ignored when `riskset = \"manual\"` and `manual.riskset` is used")
      actors <- NULL
    }

    # Canonicalize actor ids for reliable set operations in R
    edgelist$actor1 <- as.character(edgelist$actor1)
    edgelist$actor2 <- as.character(edgelist$actor2)
    manual.riskset$actor1 <- as.character(manual.riskset$actor1)
    manual.riskset$actor2 <- as.character(manual.riskset$actor2)

    # Type handling
    with_type <- !is.null(types) && ("type" %in% names(edgelist))

    if (with_type) {

      if (!("type" %in% names(manual.riskset))) {

        # expand dyads to all types
        manual.riskset <- merge(
          manual.riskset,
          data.frame(type = types, stringsAsFactors = FALSE)
        )

      } else {

        manual.riskset$type <- as.character(manual.riskset$type)

        bad_t <- setdiff(unique(manual.riskset$type), types)
        if (length(bad_t) > 0)
          stop("`manual.riskset$type` contains values not in `types`.")
      }
    }

    # Normalize undirected dyads
    if (!directed) {
      swap <- manual.riskset$actor1 > manual.riskset$actor2
      tmp <- manual.riskset$actor1[swap]
      manual.riskset$actor1[swap] <- manual.riskset$actor2[swap]
      manual.riskset$actor2[swap] <- tmp

      swap2 <- edgelist$actor1 > edgelist$actor2
      tmp2 <- edgelist$actor1[swap2]
      edgelist$actor1[swap2] <- edgelist$actor2[swap2]
      edgelist$actor2[swap2] <- tmp2
    }

    # Ensure observed dyads are included
    if (with_type) {
      if (!("type" %in% names(edgelist))) stop("`edgelist` has no `type` column but `types` was provided.")
      edgelist$type <- as.character(edgelist$type)
      obs <- unique(edgelist[, c("actor1","actor2","type"), drop = FALSE])

      key_inc <- paste(manual.riskset$actor1, manual.riskset$actor2, manual.riskset$type, sep="||")
      key_obs <- paste(obs$actor1, obs$actor2, obs$type, sep="||")
      missing_obs <- !(key_obs %in% key_inc)
      if (any(missing_obs)) {
        manual.riskset <- rbind(manual.riskset, obs[missing_obs, , drop = FALSE])
        warning(sprintf("%d observed dyad-type combinations were added to the manual risk set.", sum(missing_obs)))
      }
    } else {
      obs <- unique(edgelist[, c("actor1","actor2"), drop = FALSE])
      key_inc <- paste(manual.riskset$actor1, manual.riskset$actor2, sep="||")
      key_obs <- paste(obs$actor1, obs$actor2, sep="||")
      missing_obs <- !(key_obs %in% key_inc)
      if (any(missing_obs)) {
        manual.riskset <- rbind(manual.riskset, obs[missing_obs, , drop = FALSE])
        warning(sprintf("%d observed dyads were added to the manual risk set.", sum(missing_obs)))
      }
    }

    # Deduplicate (and warn if duplicates existed)
    if (with_type) {
      key <- paste(manual.riskset$actor1, manual.riskset$actor2, manual.riskset$type, sep="||")
    } else {
      key <- paste(manual.riskset$actor1, manual.riskset$actor2, sep="||")
    }
    ndups <- sum(duplicated(key))
    if (ndups > 0) {
      message(sprintf("Note: `manual.riskset` contained %d duplicate entries; duplicates were removed.", ndups))
      manual.riskset <- manual.riskset[!duplicated(key), , drop = FALSE]
    }

    # manual.riskset is now finalized:
    # - normalized
    # - type-complete
    # - deduplicated
    # - includes all observed dyads

    omit_dyad <- NULL

  }else{
    if(!is.null(manual.riskset)){
      warning("`manual.riskset` is ignored unless `riskset = \"manual\"`.", call. = FALSE)
    }
    manual.riskset <- NULL
  }

  out <- remifyCpp2(
    input_edgelist = edgelist,
    actors = actors,
    types = types,
    directed = directed,
    ordinal = ordinal,
    origin = origin,
    omit_dyad = omit_dyad,
    model = model,
    active = active,                    # reuse “active” outputs
    manual_riskset = manual.riskset,    # new
    ncores = ncores
  )

  if (!isTRUE(directed)) {
    swap <- out$actor1_ID > out$actor2_ID
    if (any(swap)) {
      # swap integer actor IDs used downstream by tomstats/prepare_tomstats
      tmp <- out$actor1_ID[swap]
      out$actor1_ID[swap] <- out$actor2_ID[swap]
      out$actor2_ID[swap] <- tmp

      # also swap the original actor labels in out$edgelist for readability/consistency
      tmpn <- out$edgelist$actor1[swap]
      out$edgelist$actor1[swap] <- out$edgelist$actor2[swap]
      out$edgelist$actor2[swap] <- tmpn
    }
  }

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
                            ,edgelist = out$edgelist,
                            edgelist_id = NA
  )
  ,class="remify")
  attr(str_out, "with_type") <- out$with_type
  attr(str_out, "weighted") <- out$weighted
  attr(str_out, "directed") <- directed
  attr(str_out, "ordinal") <- ordinal
  attr(str_out, "model") <- model # useful because tie and actor models have two different ways for handling changing risksets
  if (riskset == "manual") { # manual should be further treated as if it was 'active' to simply compatibility with remstimate
    attr(str_out, "riskset") <- "active"
    attr(str_out, "riskset_source") <- "manual"
  } else {
    attr(str_out, "riskset") <- riskset
    attr(str_out, "riskset_source") <- riskset
  }
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
  # # build index (event-level ids)
  # str_out$index <- list(
  #   dyadID   = out$dyad,
  #   actor1ID = out$actor1_ID,
  #   actor2ID = out$actor2_ID,
  #   type   = if (out$with_type) out$type_ID else NULL
  # )

  # ID's when riskset = "active"
  if(active || riskset == "manual"){
    str_out$activeD <- out$omit_dyad$D_active
    out$omit_dyad$D_active <- NULL
    if(model == "tie"){
      attr(str_out, "dyadIDactive") <- out$omit_dyad$dyadIDactive
      # str_out$index$dyadIDactive <- out$omit_dyad$dyadIDactive
      # out$omit_dyad$dyadIDactive <- NULL
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
    str_out$indices_simultaneous_events <- rows_to_remove
    #str_out$intereventTime and str_out$omit_dyad$time are processed in remstimate depending on method=c("pe","pt") from remstats
    str_out$E <- str_out$M # number of events
    str_out$M <- str_out$M-length(rows_to_remove) # overwrite (lower) number of time points
    time_unique <- unique(str_out$edgelist$time)

    # tie-oriented modeling
    # also store vectorized versions
    # str_out$index$dyadIDactive_vec <- str_out$index$dyadIDactive
    # str_out$index$dyadID_vec <- str_out$index$dyadID
    # str_out$index$actor1ID_vec <- str_out$index$actor1ID
    # str_out$index$actor2ID_vec <- str_out$index$actor2ID
    # str_out$index$type_vec <- str_out$index$type
    attr(str_out, "dyadIDactive_vec") <- attr(str_out, "dyadIDactive")
    attr(str_out, "dyadID_vec") <- attr(str_out, "dyadID")
    attr(str_out, "actor1ID_vec") <- attr(str_out, "actor1ID")
    attr(str_out, "actor2ID_vec") <- attr(str_out, "actor2ID")
    attr(str_out, "typeID_vec") <- attr(str_out, "typeID")
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
    attr(str_out, "dyadID") <- dyad
    # str_out$index$dyadID <- dyad
    # str_out$index$actor1ID <- actor1
    # str_out$index$actor2ID <- actor2
    if(active){
      attr(str_out,"dyadIDactive") <- dyadIDactive
      # str_out$index$dyadIDactive <- dyadIDactive
    }
    if(attr(str_out, "with_type")){
      attr(str_out,"typeID") <- type
      # str_out$index$type <- type
    }
    rm(actor1,actor2,type,dyad,dyadIDactive)
  }

  #store the dyad mapping in remify object to avoid the need to getDyad
  if (active || riskset == "manual") {
    str_out$index$dyad_map_active <- getDyad2(
      x = str_out,
      dyadID = seq_len(as.integer(str_out$activeD)[1]),
      active = TRUE
    )

    if (riskset == "manual") {
      str_out$index$dyad_map <- NULL
    }

  }else{
    str_out$index$dyad_map <- getDyad2(
      x = str_out,
      dyadID = seq_len(str_out$D),
      active = FALSE
    )
  }

  if (riskset %in% c("active","manual")) {

    # full dyad ids in the (compressed) riskset
    rs <- as.integer(out$omit_dyad$riskset_idx)

    # full map (exists for full/manual currently)
    dm_full <- str_out$index$dyad_map

    # subset and relabel to make dyad_map_active
    dm_active <- dm_full[match(rs, dm_full$dyadID), , drop = FALSE]
    dm_active$dyadIDactive <- seq_len(nrow(dm_active))

    # reorder columns to match active mode
    if ("type" %in% names(dm_active)) {
      dm_active <- dm_active[, c("dyadIDactive","actor1","actor2","type"), drop = FALSE]
    } else {
      dm_active <- dm_active[, c("dyadIDactive","actor1","actor2"), drop = FALSE]
    }

    str_out$index$dyad_map_active <- dm_active

    # optional: drop full map for manual to be structurally identical
    if (riskset == "manual") {
      str_out$index$dyad_map <- NULL
    }
  }
  out <- NULL # free-ing space [[now]]

  # add dyad and actor IDs to edgelist element
  edgelist_id <- data.frame(
    time = sort(unique(unique(str_out$edgelist$time))),
    actor1 = I(attr(str_out, "actor1ID")),
    actor2 = I(attr(str_out, "actor2ID")),
    dyad = I(attr(str_out, "dyadID"))
  )
  if(isTRUE(attr(str_out, "with_type"))){
    edgelist_id$type <- I(attr(str_out,"typeID"))
  }
  if (isTRUE(attr(str_out, "weighted"))) {
    w_by_time <- split(str_out$edgelist$weight, str_out$edgelist$time)
    w_by_time <- unname(w_by_time)
    edgelist_id$weight <- I(w_by_time)
  }
  str_out$edgelist_id <- edgelist_id
  rm(edgelist_id)

  # add riskset info
  decode_dyad_id <- function(dyadID, N, directed = TRUE, C = 1L) {
    dyadID <- as.integer(dyadID)
    d0 <- dyadID - 1L

    if (directed) {
      D0 <- N * (N - 1L)
      type0 <- d0 %/% D0
      r <- d0 - type0 * D0

      a10 <- r %/% (N - 1L)
      which2 <- r - a10 * (N - 1L)
      a20 <- ifelse(which2 >= a10, which2 + 1L, which2)

    } else {
      D0 <- N * (N - 1L) %/% 2L
      type0 <- d0 %/% D0
      r <- d0 - type0 * D0

      b <- 2L * N - 1L
      disc <- b*b - 8L * r
      i0 <- floor((b - sqrt(disc)) / 2)

      cum_i <- i0 * (2L * N - i0 - 1L) %/% 2L
      within <- r - cum_i
      a10 <- i0
      a20 <- i0 + 1L + within
    }

    data.frame(
      actor1ID = a10 + 1L,
      actor2ID = a20 + 1L,
      typeID   = type0 + 1L
    )
  }

  if (isTRUE(attach_riskset)) {

    dict <- attr(str_out, "dictionary")
    actors_df <- dict$actors
    types_df  <- dict$types

    N <- nrow(actors_df)
    with_type <- !is.null(types_df)
    C <- if (with_type) nrow(types_df) else 1L
    directed <- isTRUE(attr(str_out, "directed"))

    D_full <- if (directed) N*(N-1L)*C else (N*(N-1L)%/%2L)*C

    mode <- as.character(attr(str_out, "riskset"))  # "full"/"active"/"manual" (or manual-as-active)

    dyad_full_vec <- as.integer(attr(str_out, "dyadID_vec") %||% unlist(attr(str_out, "dyadID")))

    riskset_idx <- switch(
      mode,
      full   = seq_len(D_full),
      active = sort(unique(dyad_full_vec)),
      manual = as.integer(str_out$omit_dyad$riskset_idx[,1]),
      stop("Unknown riskset mode: ", mode)
    )

    # per-event mapping into included set
    dyadIDactive <- switch(
      mode,
      full   = dyad_full_vec,
      active = match(dyad_full_vec, riskset_idx),
      manual = as.integer(str_out$omit_dyad$dyadIDactive[,1])
    )

    rs <- list(
      mode = mode,
      directed = directed,
      with_type = with_type,
      N = N, C = C,
      D_full = D_full,
      D_active = length(riskset_idx),
      riskset_idx = riskset_idx,
      dyadIDactive = dyadIDactive,
      actors = actors_df,
      types  = types_df
    )

    # ---- decoding policy: labels -> ids fallback when too large -----------------
    n_dyads <- length(riskset_idx)

    decode_eff <- riskset_decode
    if (riskset_decode == "labels" && n_dyads > riskset_max_decode) {
      warning("Risk set has ", n_dyads, " dyads; attaching ID-only dyad table (threshold ",
              riskset_max_decode, " for labels).", call. = FALSE)
      decode_eff <- "ids"
    }

    if (decode_eff %in% c("ids","labels")) {
      comp <- decode_dyad_id(riskset_idx, N = N, directed = directed, C = C)
      comp$dyadID <- riskset_idx

      if (decode_eff == "labels") {
        comp$actor1 <- actors_df$actorName[comp$actor1ID]
        comp$actor2 <- actors_df$actorName[comp$actor2ID]
        if (with_type) comp$type <- types_df$typeName[comp$typeID]
      }

      if (with_type) {
        if (decode_eff == "labels") {
          keep <- c("actor1","actor2","type","dyadID","actor1ID","actor2ID","typeID")
        } else {
          keep <- c("dyadID","actor1ID","actor2ID","typeID")
        }
      } else {
        if (decode_eff == "labels") {
          keep <- c("actor1","actor2","dyadID","actor1ID","actor2ID")
        } else {
          keep <- c("dyadID","actor1ID","actor2ID")
        }
      }

      rs$included <- comp[, keep, drop = FALSE]
      rs$decode <- decode_eff
    } else {
      rs$decode <- "none"
    }

    str_out$riskset_info <- rs
  }

  return(str_out)
}



getDyad2 <- function(x, dyadID, active = FALSE) {

  if (active && attr(x,"riskset") %in% c("full")) {
    stop("'active' = TRUE works only for attr(x,'riskset') in c('active','manual')")
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
    rs <- x$omit_dyad$riskset_idx
    if (is.matrix(rs)) rs <- drop(rs)
    rs <- as.integer(rs)

    if (length(rs) == 0L) stop("omit_dyad$riskset_idx is empty.")

    if (max(dyadID, na.rm = TRUE) > length(rs)) {
      stop("Requested dyadIDactive exceeds available active riskset size.")
    }

    dyadID_full <- rs[dyadID]
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



