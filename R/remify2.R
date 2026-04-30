# helper for %||%
`%||%` <- function(a, b) if (!is.null(a)) a else b



#' @title Process a Relational Event History
#'
#' @description A function that processes raw relational event history data and returns a S3 object of class 'remify' which is used as input in other functions inside 'remverse'.
#'
#' @param edgelist the relational event history. An object of class \code{\link[base]{data.frame}} with first three columns corresponding to time, and actors forming the dyad. The first three columns will be re-named "time", "actor1", "actor2" (where, for directed networks, "actor1" corresponds to the sender and "actor2" to the receiver of the relational event). Optional columns that can be supplied are: `type` and `weight`. If one or both exist in \code{edgelist}, they have to be named accordingly.
#' @param directed logical value indicating whether events are directed (\code{TRUE}) or undirected (\code{FALSE}). (default value is \code{TRUE})
#' @param ordinal logical value indicating whether only the order of events matters in the model (\code{TRUE}) or also the exact timing must be considered in the model (\code{FALSE}). (default value is \code{FALSE}). If \code{TRUE}, then the column "time" of \code{edgelist} is still used to extract the order.
#' @param model either \code{"tie"} (default) or \code{"actor"} oriented modeling. For \code{"tie"}, the riskset is at the dyad level. For \code{"actor"}, the model has two sub-processes: a sender rate model (who sends next?) and a receiver choice model (who does the sender choose?). Actor-oriented modeling requires \code{directed=TRUE}. The returned object includes \code{sender_riskset}, \code{receiver_riskset}, and \code{activeN} (see \code{@return}).
#' @param thin Integer >= 1. Event-time thinning based on unique time points.
#'   Keeps every \code{thin}-th unique event time (after time translation) and
#'   maps each event time to the next kept time point (i.e., ceiling to the kept grid).
#'   This reduces the number of unique time points (and thus memory/computation in later steps).
#' @param actors [\emph{optional}] character vector of actors' names that may be observed interacting in the network. If \code{NULL} (default), actors' names will be taken from the input edgelist.
#' @param riskset [\emph{optional}] character value indicating the type of risk set to process: \code{riskset = "full"} (default) consists of all the possible dyadic events given the number of actors (and the number of event types) and it mantains the same structure over time. \code{riskset = "active"} considers at risk only the observed dyads and it mantains the same structure over time. \code{riskset = "manual"}, allows the risk set to have a structure that is user-defined, and it is based on the instructions supplied via the argument \code{omit_dyad}. This type of risk set allows for time-varying risk set, in which, for instance, subset of actors can interact only at specific time windows, or events of a specific type (sentiment) can't be observed within time intervals that are defined by the user. \code{riskset = "active_saturated"} extends the active riskset by adding the reverse direction for each observed dyad (if A->B is observed, B->A is also at risk) and includes all event types for each observed actor pair (type column is ignored). This reflects the assumption that observing any interaction between two actors implies both directions and all types are possible.
#' @param manual.riskset [\emph{optional}] When \code{riskset = "manual"}, this argument of class \code{\link[base]{data.frame}} specifies which dyadic riskset to consider through the entire sequence. If observed dyads from the \code{edgelist} are missing, they will be automatically be added.
#' @param extend_riskset_by_type logical. \code{FALSE} (default). When event types are present (via
#'   \code{event_type}), controls whether the risk set is expanded over types.
#'   If \code{TRUE} (default when types are present), each actor pair is
#'   duplicated for each event type, so the risk set has size
#'   \eqn{D = N(N-1) \times C} (directed) or \eqn{D = N(N-1)/2 \times C}
#'   (undirected), and the \code{type} column appears in the decoded risk set.
#'   If \code{FALSE}, event type is treated as a mark on events only and does
#'   not expand the risk set: \eqn{D = N(N-1)} (directed) or
#'   \eqn{D = N(N-1)/2} (undirected), and no \code{type} column appears in the
#'   decoded risk set. This argument is ignored when no event types are present.
#' @param event_type Optional. Either \code{NULL} (default) or a single character
#'   string giving the name of the column in \code{edgelist} that contains event
#'   types (marks).
#'
#'   If \code{event_type} is \code{NULL}, \code{remify()} uses \code{edgelist$type}
#'   if it exists; otherwise events are treated as untyped.
#'
#'   If \code{event_type} is a column name, that column is used as the event-type
#'   mark. If a column named \code{type} already exists and \code{event_type != "type"},
#'   the existing \code{edgelist$type} is overridden (with a warning).
#'
#'   When event types are present (via \code{edgelist$type} or \code{event_type}),
#'   the dyadic risk set is extended over types, i.e., each dyad is duplicated for
#'   each event type (dyad \eqn{\times} type).
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
#' @param event_covariates Optional character vector of column names in
#'   \code{edgelist} to retain as additional event-level variables in the
#'   returned \code{reh} object.
#'
#'   These columns are stored as \code{reh$event_covariates} together with the
#'   corresponding \code{time}, \code{actor1}, and \code{actor2} columns (and an
#'   internal \code{.event_id}). This is useful when downstream functions (e.g.,
#'   in \pkg{remstats}) need access to event-level marks/covariates that are not
#'   part of the core \code{reh$edgelist} produced by \code{remify()}.
#'
#'   Note: \code{event_covariates} does not affect risk set construction or type
#'   handling in \code{remify()}; it only preserves additional columns for later
#'   use. Currently there is no further support yet when event_covariates
#'   have been added.
#' @param ncores [\emph{optional}] number of cores used in the parallelization of the processing functions. (default is \code{1}).
#' @param omit_dyad Deprecated. Set to \code{NULL}.
#'
#' @return A \code{remify} S3 object (list) with the following elements:
#'   \itemize{
#'     \item \code{M} number of events (or unique time points if simultaneous events exist).
#'     \item \code{N} number of actors.
#'     \item \code{C} number of event types (1 if untyped).
#'     \item \code{D} number of dyads in the riskset.
#'     \item \code{intereventTime} vector of inter-event waiting times (\code{NULL} if \code{ordinal=TRUE}).
#'     \item \code{edgelist} processed input edgelist as \code{data.frame}.
#'     \item \code{edgelist_id} per-event integer ID summary.
#'     \item \code{meta} list of metadata (model, directed, ordinal, riskset, dictionary, etc.).
#'     \item \code{ids} list of per-event integer IDs (actor1, actor2, dyad, type).
#'     \item \code{index} list of decoded riskset tables (\code{dyad_map} or \code{dyad_map_active} for tie model; \code{sender_map} for actor model).
#'     \item \code{activeD} number of active dyads (tie model, \code{riskset="active"} or \code{"manual"} only).
#'     \item \code{riskset_info} decoded riskset metadata (tie model only, when \code{attach_riskset=TRUE}).
#'   }
#'   For \strong{actor-oriented models} (\code{model="actor"}), the following additional elements are returned:
#'   \itemize{
#'     \item \code{sender_riskset} integer vector of actor IDs allowed to send (all actors for \code{"full"}; observed senders for \code{"active"}; senders in \code{manual.riskset} for \code{"manual"}/\code{"active_saturated"}).
#'     \item \code{receiver_riskset} named list (actor names) of integer vectors of allowed receiver IDs per sender.
#'     \item \code{activeN} number of active senders.
#'     \item \code{index\$sender_map} data.frame with columns \code{senderID} and \code{actorName} for active senders.
#'   }
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
#'        origin = randomREH$origin)
#'
#' # summary
#' summary(tie_randomREH)
#'
#' # visualize descriptive measures of relational event data
#' plot(x = tie_randomREH)
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
#' # visualize
#' plot(actor_randomREH)
#'
#' # ------------------------------------ #
#' # for more information about remify()  #
#' # check: vignette(package="remify")    #
#' # ------------------------------------ #
#'
#'
remify <- function(edgelist,
                   directed = TRUE,
                   ordinal = FALSE,
                   model = c("tie","actor"),
                   thin = 1,
                   actors = NULL,
                   riskset = c("full","active","active_saturated","manual"),
                   manual.riskset = NULL,
                   extend_riskset_by_type = FALSE,
                   event_type = NULL,
                   origin = NULL,
                   time.units = c("auto", "secs", "mins",
                             "hours", "days", "weeks"),
                   attach_riskset = TRUE,
                   riskset_decode = c("labels","ids","none"),
                   riskset_max_decode = 200000L,
                   event_covariates = NULL,
                   ncores = 1L,
                   omit_dyad = NULL
){

  # (1) Checking for 'edgelist' input object
  if(!is.null(omit_dyad)) {
    warning("`omit_dyad` is decrecated. It is set to `NULL`.")
  }
  omit_dyad <- types <- NULL

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

  if (!is.null(event_type)) {
    if (!is.character(event_type) || length(event_type) != 1L) {
      stop("`event_type` must be NULL or a single column name.")
    }
    if (!(event_type %in% names(edgelist))) {
      stop("`event_type` not found in `edgelist`: ", event_type)
    }

    if (event_type != "type") {
      if ("type" %in% names(edgelist)) {
        warning("`event_type = '", event_type,
                "'` overrides existing `edgelist$type` for event typing.",
                call. = FALSE)
      }
      edgelist$type <- edgelist[[event_type]]
    }
  }

  # validate event_covariates
  if (!is.null(event_covariates)) {
    if (!is.character(event_covariates)) stop("`event_covariates` must be a character vector of column names.")
    event_covariates <- unique(event_covariates)
    missing <- setdiff(event_covariates, names(edgelist))
    if (length(missing)) stop("`event_covariates` not found in `edgelist`: ", paste(missing, collapse = ", "))
    event_covariates <- setdiff(event_covariates, c("time","actor1","actor2","type","weight"))
    if (!length(event_covariates)) event_covariates <- NULL
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

  if (model == "actor" && isTRUE(extend_riskset_by_type)) {
    stop("'extend_riskset_by_type = TRUE' is not supported for actor-oriented models. ",
         "In the actor model, event type is modeled as a third stage (given sender and receiver), ",
         "not as part of the receiver riskset. Use 'extend_riskset_by_type = FALSE'.")
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
      # if (!isTRUE(ordinal)){
      #   message(paste("Note: origin is set to ", origin))
      # }
    }

    edgelist$time <- as.numeric(difftime(t, origin, units = time.units))
    origin <- 0

  } else if (is.numeric(t) || is.integer(t)) {

    # numeric time: difftime is not appropriate
    if (is.null(origin)) {
      origin <- 0
      # if (!isTRUE(ordinal)){
      #   message(paste("Note: origin is set to ", origin))
      # }
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

  # --- thinning (event-grid thinning: keep every `thin`-th unique time) --------
  if (!is.null(thin) && length(thin) == 1L) {
    if (!is.numeric(thin) || is.na(thin) || thin < 1) {
      stop("`thin` must be a single numeric value >= 1.")
    }
    thin <- as.integer(thin)

    if (thin > 1L) {
      tt <- edgelist$time
      if (anyNA(tt)) stop("edgelist$time contains NA before thinning.")

      u <- sort(unique(tt))
      keep_idx <- seq.int(from = thin, to = length(u), by = thin)
      kept <- u[keep_idx]

      # map each time to the next kept time >= tt, preserving exact matches
      j <- findInterval(tt, kept, left.open = TRUE) + 1L
      j[j > length(kept)] <- length(kept)
      edgelist$time <- kept[j]

      if (isTRUE(ordinal)) {
        edgelist$time <- as.integer(match(edgelist$time, sort(unique(edgelist$time))))
      }
    }
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
  # if(is.null(riskset)){
  #   riskset <- "full"
  # }
  riskset        <- match.arg(arg = riskset, choices = c("full","active","active_saturated","manual"), several.ok = FALSE)
  riskset_source <- riskset
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

  if(riskset == "manual"){

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

  # capture event covariates before C++
  event_covariates_df <- NULL
  if (!is.null(event_covariates)) {
    event_covariates_df <- edgelist[, c("time","actor1","actor2", event_covariates), drop = FALSE]
    event_covariates_df$.event_id <- seq_len(nrow(event_covariates_df))
  }

  # ---- active_saturated: manual riskset from observed pairs + reversed -------
  if (riskset == "active_saturated") {
    el_pairs <- unique(edgelist[, c("actor1", "actor2")])
    el_rev   <- el_pairs[, c("actor2", "actor1")]
    names(el_rev) <- c("actor1", "actor2")
    sat_pairs <- unique(rbind(el_pairs, el_rev))
    # If typed events, cross with all observed types so all types are at risk
    if ("type" %in% names(edgelist)) {
      all_types <- unique(edgelist$type)
      sat_pairs <- do.call(rbind, lapply(all_types, function(tp) {
        cbind(sat_pairs, type = tp, stringsAsFactors = FALSE)
      }))
    }
    manual.riskset <- unique(sat_pairs)
    riskset <- "manual"
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
    active = active,
    manual_riskset = manual.riskset,
    extend_riskset_by_type = extend_riskset_by_type,
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
  # ---- Build output object (no attributes, everything as list elements) --------

  str_out <- structure(
    list(
      M             = out$M,
      N             = out$N,
      C             = out$C,
      D             = out$D,
      intereventTime = out$intereventTime,
      edgelist      = out$edgelist,
      edgelist_id   = NA   # filled in below
    ),
    class = "remify"
  )

  # ---- $meta: all metadata (replaces all attr() calls) -----------------------
  str_out$meta <- list(
    model            = model,
    directed         = directed,
    ordinal          = ordinal,
    weighted         = out$weighted,
    with_type        = out$with_type,
    with_type_riskset = isTRUE(out$with_type_riskset),
    C_riskset        = out$C_riskset %||% 1L,
    riskset          = if (riskset == "manual") "active" else riskset,
    riskset_source   = riskset_source, #riskset_source,
    origin           = out$edgelist$time[1] - out$intereventTime[1],
    ncores           = ncores,
    dictionary       = list(actors = out$actorsDictionary, types = out$typesDictionary),
    event_covariates = NULL   # filled in below if supplied
  )

  # ---- event covariates -------------------------------------------------------
  if (!is.null(event_covariates_df)) {
    # canonicalize actor order for undirected networks
    if (!isTRUE(directed)) {
      swapx <- event_covariates_df$actor1 > event_covariates_df$actor2
      if (any(swapx)) {
        tmp <- event_covariates_df$actor1[swapx]
        event_covariates_df$actor1[swapx] <- event_covariates_df$actor2[swapx]
        event_covariates_df$actor2[swapx] <- tmp
      }
    }
    extra <- event_covariates_df[, event_covariates, drop = FALSE]
    if (nrow(extra) != nrow(str_out$edgelist)) {
      warning("`event_covariates` could not be attached to `reh$edgelist` (row mismatch). Storing as `reh$event_covariates` instead.",
              call. = FALSE)
      str_out$event_covariates <- event_covariates_df
    } else {
      bad <- intersect(names(extra), names(str_out$edgelist))
      if (length(bad)) {
        stop("`event_covariates` collide with existing `reh$edgelist` columns: ",
             paste(bad, collapse = ", "))
      }
      str_out$edgelist <- cbind(str_out$edgelist, extra)
    }
    str_out$meta$event_covariates <- event_covariates
  }

  # ---- $ids: per-event integer IDs (replaces dyadID/actor1ID/etc. attrs) -----
  str_out$ids <- list(
    dyad       = out$dyad,
    actor1     = out$actor1_ID,
    actor2     = out$actor2_ID,
    type       = if (isTRUE(out$with_type)) out$type_ID else NULL,
    dyad_active = NULL,   # filled in below if active/manual
    # vectorized versions for simultaneous events (filled in below)
    dyad_vec        = NULL,
    actor1_vec      = NULL,
    actor2_vec      = NULL,
    type_vec        = NULL,
    dyad_active_vec = NULL
  )

  # ---- active / manual riskset IDs -------------------------------------------
  if (active || riskset == "manual") {
    if (model == "tie") {
      str_out$activeD <- out$omit_dyad$D_active
      str_out$ids$dyad_active <- out$omit_dyad$dyadIDactive
    }
    out$omit_dyad$D_active <- NULL
  }

  str_out$omit_dyad <- out$omit_dyad

  # ---- actor model risksets ---------------------------------------------------
  # For actor model, build clean sender and receiver risksets in R.
  # These are derived from the edgelist / manual.riskset after C++ processing.
  # sender_riskset: integer vector of actor IDs (1-based) allowed to send
  # receiver_riskset: named list, one element per sender, each a vector of
  #                   receiver actor IDs (1-based) allowed for that sender
  if (model == "actor") {
    actor_ids   <- str_out$meta$dictionary$actors$actorID   # 1..N
    actor_names <- str_out$meta$dictionary$actors$actorName

    if (riskset == "full") {
      # All actors can send; each sender can choose any other actor
      str_out$sender_riskset   <- actor_ids
      str_out$receiver_riskset <- setNames(
        lapply(actor_ids, function(i) actor_ids[actor_ids != i]),
        actor_names
      )

    } else if (active) {
      # Sender riskset: actors that have been observed sending
      # Use risksetSender from C++ output (1 x N binary matrix)
      sender_active_flag <- as.logical(out$omit_dyad$risksetSender[1, ])
      str_out$sender_riskset <- actor_ids[sender_active_flag]

      # Receiver riskset per sender: actors observed as receiver for that sender
      el <- str_out$edgelist
      a1_ids <- str_out$ids$actor1   # integer sender IDs (1-based)
      a2_ids <- str_out$ids$actor2   # integer receiver IDs (1-based)
      str_out$receiver_riskset <- setNames(
        lapply(str_out$sender_riskset, function(s) {
          obs_receivers <- unique(a2_ids[a1_ids == s])
          sort(obs_receivers)
        }),
        actor_names[str_out$sender_riskset]
      )

    } else if (riskset == "manual") {
      # Sender riskset and receiver riskset derived from manual.riskset
      # manual.riskset is a data.frame with actor1, actor2 columns
      if (is.null(manual.riskset)) {
        stop("riskset='manual' requires a manual.riskset data.frame for actor model")
      }
      # Map actor names to IDs
      name_to_id <- setNames(actor_ids, actor_names)
      mr_a1 <- name_to_id[as.character(manual.riskset[[1]])]
      mr_a2 <- name_to_id[as.character(manual.riskset[[2]])]
      str_out$sender_riskset <- sort(unique(mr_a1))
      str_out$receiver_riskset <- setNames(
        lapply(str_out$sender_riskset, function(s) {
          sort(unique(mr_a2[mr_a1 == s]))
        }),
        actor_names[str_out$sender_riskset]
      )
    }

    # activeN: number of active senders
    str_out$activeN <- length(str_out$sender_riskset)

    # index$sender_map: decoded sender table (mirrors tie model's dyad_map_active)
    str_out$index$sender_map <- data.frame(
      senderID   = str_out$sender_riskset,
      actorName  = str_out$meta$dictionary$actors$actorName[str_out$sender_riskset],
      stringsAsFactors = FALSE
    )

    # omit_dyad: keep as empty list for structural consistency with tie model
    str_out$omit_dyad <- list()
  }

  # ---- simultaneous events ---------------------------------------------------
  evenly_spaced_interevent_time <- NULL
  rows_to_remove <- NULL
  if (!is.null(out$rows_to_remove)) {
    if (!ordinal) {
      evenly_spaced_interevent_time <- out$evenly_spaced_interevent_time
    }
    rows_to_remove <- out$rows_to_remove
  }

  if (!is.null(rows_to_remove)) {
    # store simultaneous event info in its own sublist
    str_out$simultaneous <- list(
      indices                    = rows_to_remove,
      interevent_evenly_spaced   = evenly_spaced_interevent_time
    )
    # also keep top-level for remstimate compatibility
    str_out$indices_simultaneous_events <- rows_to_remove

    if (!ordinal) {
      str_out$intereventTime <- str_out$intereventTime[-rows_to_remove]
    }

    str_out$E <- str_out$M
    str_out$M <- str_out$M - length(rows_to_remove)
    time_unique <- unique(str_out$edgelist$time)

    # save flat vectorized versions before converting to per-time-point lists
    str_out$ids$actor1_vec      <- str_out$ids$actor1
    str_out$ids$actor2_vec      <- str_out$ids$actor2
    str_out$ids$type_vec        <- str_out$ids$type
    if (model == "tie") {
      str_out$ids$dyad_vec        <- str_out$ids$dyad
      str_out$ids$dyad_active_vec <- str_out$ids$dyad_active
    }

    # convert to per-time-point lists
    actor1_list      <- vector("list", length(time_unique))
    actor2_list      <- vector("list", length(time_unique))
    dyad_list        <- vector("list", length(time_unique))
    type_list        <- vector("list", length(time_unique))
    dyad_active_list <- vector("list", length(time_unique))

    for (m in seq_along(time_unique)) {
      idx <- which(str_out$edgelist$time == time_unique[m])
      actor1_list[[m]] <- str_out$ids$actor1[idx]
      actor2_list[[m]] <- str_out$ids$actor2[idx]
      if (model == "tie") {
        dyad_list[[m]] <- str_out$ids$dyad[idx]
        if (active || riskset == "manual") {
          dyad_active_list[[m]] <- str_out$ids$dyad_active[idx]
        }
      }
      if (isTRUE(str_out$meta$with_type)) {
        type_list[[m]] <- str_out$ids$type[idx]
      }
    }

    str_out$ids$actor1 <- actor1_list
    str_out$ids$actor2 <- actor2_list
    if (model == "tie") {
      str_out$ids$dyad <- dyad_list
      if (active || riskset == "manual") {
        str_out$ids$dyad_active <- dyad_active_list
      }
    }
    if (isTRUE(str_out$meta$with_type)) {
      str_out$ids$type <- type_list
    }
  }

  # ---- dyad map (index) — tie model only ------------------------------------
  if (model == "tie" && (active || riskset == "manual")) {
    # When ext=FALSE but events have types, C++ collapses typed->untyped in
    # riskset_idx. getDyad2() would decode wrong actor/type. Instead decode
    # riskset_idx as untyped, then expand with observed types from edgelist.
    if ((active || riskset == "manual") && isTRUE(str_out$meta$with_type) && !isTRUE(str_out$meta$with_type_riskset)) {
      # Decode actor pairs from riskset_idx using the pure-R formula (same as
      # decode_dyad_id below). getEventsComposition is broken for undirected networks.
      N_loc  <- str_out$N
      rs_raw <- str_out$omit_dyad$riskset_idx
      rs_idx <- as.integer(if (is.matrix(rs_raw)) rs_raw[, 1L] else rs_raw)
      dict_loc <- str_out$meta$dictionary
      d0 <- rs_idx - 1L
      if (isTRUE(str_out$meta$directed)) {
        D0 <- N_loc * (N_loc - 1L)
        r  <- d0 %% D0
        a10 <- r %/% (N_loc - 1L)
        w   <- r - a10 * (N_loc - 1L)
        a20 <- ifelse(w >= a10, w + 1L, w)
      } else {
        b   <- 2L * N_loc - 1L
        disc <- b * b - 8L * d0
        i0  <- floor((b - sqrt(disc)) / 2)
        cum_i <- (i0 * (2L * N_loc - i0 - 1L)) %/% 2L
        a10 <- i0
        a20 <- i0 + 1L + (d0 - cum_i)
      }
      actor1_u <- dict_loc$actors$actorName[a10 + 1L]
      actor2_u <- dict_loc$actors$actorName[a20 + 1L]
      el       <- str_out$edgelist
      el_key   <- paste(el$actor1, el$actor2, sep = "|")
      rows <- lapply(seq_along(rs_idx), function(i) {
        key       <- paste(actor1_u[i], actor2_u[i], sep = "|")
        types_obs <- unique(el$type[el_key == key])
        if (length(types_obs) == 0L) return(NULL)
        data.frame(actor1 = actor1_u[i], actor2 = actor2_u[i],
                   type = types_obs, stringsAsFactors = FALSE)
      })
      active_typed <- do.call(rbind, rows[!sapply(rows, is.null)])
      active_typed$dyadIDactive <- seq_len(nrow(active_typed))
      str_out$index$dyad_map_active <- active_typed[, c("dyadIDactive","actor1","actor2","type")]
    } else {
      str_out$index$dyad_map_active <- getDyad2(
        x = str_out,
        dyadID = seq_len(as.integer(str_out$activeD)[1]),
        active = TRUE
      )
    }
    if (riskset == "manual") {
      str_out$index$dyad_map <- str_out$index$dyad_map_active
    }
  } else if (model == "tie") {
    str_out$index$dyad_map <- getDyad2(
      x = str_out,
      dyadID = seq_len(str_out$D),
      active = FALSE
    )
  }

  out <- NULL  # free memory

  # ---- edgelist_id (per-time-point summary) -----------------------------------
  # For actor model, ids$dyad is NULL (C++ does not compute dyad IDs)
  edgelist_id <- data.frame(
    time   = sort(unique(str_out$edgelist$time)),
    actor1 = I(str_out$ids$actor1),
    actor2 = I(str_out$ids$actor2)
  )
  if (model == "tie" && !is.null(str_out$ids$dyad)) {
    edgelist_id$dyad <- I(str_out$ids$dyad)
  }
  if (isTRUE(str_out$meta$with_type)) {
    edgelist_id$type <- I(str_out$ids$type)
  }
  if (isTRUE(str_out$meta$weighted)) {
    w_by_time <- split(str_out$edgelist$weight, str_out$edgelist$time)
    edgelist_id$weight <- I(unname(w_by_time))
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
      D0 <- (N * (N - 1L)) %/% 2L
      type0 <- d0 %/% D0
      r <- d0 - type0 * D0

      b <- 2L * N - 1L
      disc <- b*b - 8L * r
      i0 <- floor((b - sqrt(disc)) / 2)

      cum_i <- (i0 * (2L * N - i0 - 1L)) %/% 2L
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

  if (isTRUE(attach_riskset) && model == "tie") {

    dict      <- str_out$meta$dictionary
    actors_df <- dict$actors
    types_df  <- dict$types

    N <- nrow(actors_df)
    with_type         <- isTRUE(str_out$meta$with_type)           # events are typed
    with_type_riskset <- isTRUE(str_out$meta$with_type_riskset)   # riskset is typed
    C <- if (with_type_riskset) nrow(types_df) else 1L
    directed  <- isTRUE(str_out$meta$directed)

    D_full <- str_out$D

    mode <- str_out$meta$riskset_source

    dyad_full_vec <- as.integer(str_out$ids$dyad_vec %||% unlist(str_out$ids$dyad))

    # cat("riskset:", riskset, "\n")
    # cat("mode:", mode, "\n")
    # cat("meta names:", names(str_out$meta), "\n")

    riskset_idx <- switch(
      mode,
      full             = seq_len(D_full),
      active           = sort(unique(dyad_full_vec)),
      manual           = ,
      active_saturated = as.integer(str_out$omit_dyad$riskset_idx[, 1]),
      stop("Unknown riskset mode: ", mode)
    )

    dyadIDactive <- switch(
      mode,
      full             = dyad_full_vec,
      active           = match(dyad_full_vec, riskset_idx),
      manual           = ,
      active_saturated = match(dyad_full_vec, riskset_idx)
    )

    rs <- list(
      mode = mode,
      directed = directed,
      with_type = with_type_riskset,
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
        if (with_type_riskset) comp$type <- types_df$typeName[comp$typeID]
      }

      if (with_type_riskset) {
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

  str_out$meta$riskset_source <- riskset_source

  return(str_out)
}



getDyad2 <- function(x, dyadID, active = FALSE) {

  if (active && x$meta$riskset %in% c("full")) {
    stop("'active' = TRUE works only for riskset in c('active','manual')")
  }
  if (!is.numeric(dyadID) & !is.integer(dyadID)) {
    stop("'dyadID' must be a numeric (or integer) vector")
  }
  out <- NULL
  dyadID <- as.integer(dyadID)

  # check for duplicates in dyadID
  length_orig <- length(dyadID)
  dyadID <- unique(dyadID)
  if (length_orig > length(dyadID)) {
    warning("'dyadID' contains ID's that are repeated more than once. Such ID's will be processed once")
  }

  dict_loc <- x$meta$dictionary
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

  # Decode dyad IDs to actor pairs using pure-R formula.
  # getEventsComposition is broken for undirected networks (always returns [1,1,1]).
  N_loc <- x$N
  C_loc <- if (isTRUE(x$meta$with_type_riskset)) nrow(x$meta$dictionary$types) else 1L
  directed_loc <- isTRUE(x$meta$directed)
  d0 <- dyadID_full - 1L
  if (directed_loc) {
    D0    <- N_loc * (N_loc - 1L)
    type0 <- d0 %/% D0
    r     <- d0 - type0 * D0
    a10   <- r %/% (N_loc - 1L)
    w     <- r - a10 * (N_loc - 1L)
    a20   <- ifelse(w >= a10, w + 1L, w)
  } else {
    D0    <- (N_loc * (N_loc - 1L)) %/% 2L
    type0 <- d0 %/% D0
    r     <- d0 - type0 * D0
    b     <- 2L * N_loc - 1L
    disc  <- b * b - 8L * r
    i0    <- floor((b - sqrt(disc)) / 2)
    cum_i <- (i0 * (2L * N_loc - i0 - 1L)) %/% 2L
    a10   <- i0
    a20   <- i0 + 1L + (r - cum_i)
  }

  # Use with_type_riskset (not with_type) to decide whether to include a type
  # column. When with_type=TRUE but with_type_riskset=FALSE (ext=FALSE), the
  # dyad IDs are untyped so type decoding is meaningless.
  if (isTRUE(x$meta$with_type_riskset)) {
    out <- data.frame(
      dyadID = dyadID,
      actor1 = dict_loc$actors$actorName[a10 + 1L],
      actor2 = dict_loc$actors$actorName[a20 + 1L],
      type   = dict_loc$types$typeName[type0 + 1L]
    )
  } else {
    out <- data.frame(
      dyadID = dyadID,
      actor1 = dict_loc$actors$actorName[a10 + 1L],
      actor2 = dict_loc$actors$actorName[a20 + 1L]
    )
  }

  if (active) names(out)[1] <- "dyadIDactive"

  return(out)
}



