
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
summary.remify <- function(object, ...) {

  # Support both old remify (attributes) and new remify2 ($meta) structure
  if (!is.null(object$meta)) {
    .model             <- object$meta$model
    .with_type         <- isTRUE(object$meta$with_type)
    .with_type_riskset <- isTRUE(object$meta$with_type_riskset)
    .riskset           <- if (!is.null(object$meta$riskset_source)) object$meta$riskset_source else object$meta$riskset
    .directed          <- object$meta$directed
    .ordinal           <- object$meta$ordinal
    .weighted          <- object$meta$weighted
    .origin            <- object$meta$origin
    .C                 <- if (!is.null(object$C)) object$C else 1L  # NULL for untyped edgelists
  } else {
    .model             <- attr(object, "model")
    .with_type         <- isTRUE(attr(object, "with_type"))
    .with_type_riskset <- FALSE
    .riskset           <- attr(object, "riskset")
    .directed          <- attr(object, "directed")
    .ordinal           <- attr(object, "ordinal")
    .weighted          <- attr(object, "weighted")
    .origin            <- attr(object, "origin")
    .C                 <- if (!is.null(object$C)) object$C else 1L
  }

  title <- "Relational Event Network"
  model <- paste0("(processed for ", .model, "-oriented modeling):")

  # Events
  if (is.null(object$E)) {
    events <- paste0("\t> events = ", object$M)
  } else {
    events <- paste0("\t> events = ", object$E, " (time points = ", object$M, ")")
  }

  # Actors
  actors <- paste0("\t> actors = ", object$N)

  # Types — always shown for consistency, even when C = 1
  types <- paste0("\t> (event) types = ", .C)

  # Riskset block
  riskset_line <- paste0("\t> riskset = ", .riskset)
  riskset_details <- character(0)

  if (.riskset == "active" || !is.null(object$activeD)) {
    # Active riskset
    D_active <- object$activeD
    D_full   <- object$D
    if (.with_type_riskset) {
      # D_full is already the typed total; untyped = D_full / C
      D_pairs <- D_full / .C
      riskset_details <- c(riskset_details,
        paste0("\t\t>> active dyads = ", D_active,
               " (full risk set size = ", D_full, " typed dyads, ", D_pairs, " actor pairs)"))
    } else {
      riskset_details <- c(riskset_details,
        paste0("\t\t>> active dyads = ", D_active,
               " (full risk set size = ", D_full / max(.C, 1L), " actor pairs)"))
    }
    # Per-type counts for active/manual when typed
    if (.with_type && .C > 1L && !is.null(object$riskset_info$included)) {
      type_counts <- table(object$riskset_info$included$type)
      type_str <- paste(names(type_counts), type_counts, sep = "=", collapse = ", ")
      riskset_details <- c(riskset_details,
        paste0("\t\t>> per type: ", type_str))
    }
  } else if (.riskset == "manual") {
    D_total <- object$D
    if (.with_type_riskset && .C > 1L) {
      D_pairs <- D_total / .C
      riskset_details <- c(riskset_details,
        paste0("\t\t>> included dyads = ", D_total,
               " (", .C, " types x ", D_pairs, " actor pairs)"))
      # Per-type counts
      if (!is.null(object$riskset_info$included)) {
        type_counts <- table(object$riskset_info$included$type)
        type_str <- paste(names(type_counts), type_counts, sep = "=", collapse = ", ")
        riskset_details <- c(riskset_details,
          paste0("\t\t>> per type: ", type_str))
      }
    } else {
      riskset_details <- c(riskset_details,
        paste0("\t\t>> included dyads = ", D_total))
    }
  } else {
    # Full riskset
    D_total <- object$D
    if (.with_type_riskset && .C > 1L) {
      D_pairs <- D_total / .C
      riskset_details <- c(riskset_details,
        paste0("\t\t>> included dyads = ", D_total,
               " (", .C, " types x ", D_pairs, " actor pairs)"))
    } else {
      riskset_details <- c(riskset_details,
        paste0("\t\t>> included dyads = ", D_total))
    }
  }

  riskset_block <- c(riskset_line, riskset_details)

  # extend_riskset_by_type note — only shown when types > 1
  ext_note <- NULL
  if (.with_type && .C > 1L) {
    ext_note <- paste0("\t\t>> extend_riskset_by_type = ", .with_type_riskset)
  }

  directed <- paste0("\t> directed = ", .directed)
  ordinal  <- paste0("\t> ordinal = ",  .ordinal)
  weighted <- paste0("\t> weighted = ", .weighted)

  # Time info
  time_length    <- NULL
  interevent_time <- NULL
  time <- object$edgelist$time

  if (!.ordinal) {
    tlen <- time[length(time)] - .origin
    time_length <- paste0("\t> time length ~ ", round(tlen), " ",
                          attr(tlen, "units"))

    min_iet <- min(object$intereventTime)
    max_iet <- max(object$intereventTime)
    units_minmax <- NULL
    if (inherits(time, "Date")) {
      units_minmax <- "days"
    } else if (!is.numeric(time) && !is.integer(time)) {
      units_minmax <- "seconds"
    }
    interevent_time <- paste0("\t> interevent time \n\t\t >> minimum ~ ",
                              round(min_iet, 4), " ", units_minmax,
                              "\n\t\t >> maximum ~ ",
                              round(max_iet, 4), " ", units_minmax, "\n")
  }

  out <- c(title, model, events, actors, types,
           riskset_block, ext_note,
           directed, ordinal, weighted,
           time_length, interevent_time)
  out <- out[!sapply(out, is.null)]
  cat(paste(out, collapse = "\n"))
}



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
