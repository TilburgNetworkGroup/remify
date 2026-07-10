# ─────────────────────────────────────────────────────────────────────────────
# remify_durem.R
# Duration Relational Event Model — remify_durem class
#
# Entry point: remify(edgelist, duration = TRUE, ...)
# Returns an object of class c("remify_durem", "remify") that carries the
# original duration edgelist and all DuREM-specific settings in a $durem slot,
# while the base remify object (built from start events) provides the actor
# dictionary, risk set structure, and metadata used downstream by remstats and
# remstimate.
# ─────────────────────────────────────────────────────────────────────────────

# internal, shared
.remify_make_time_map <- function(t, time_units, origin = NULL) {
  if (inherits(t, c("POSIXct", "POSIXt", "Date"))) {
    to_num <- function(a, b) {
      if (time_units %in% c("months", "years")) {
        d <- as.numeric(difftime(a, b, units = "days"))
        d / (if (time_units == "years") 365.25 else 365.25 / 12)
      } else as.numeric(difftime(a, b, units = time_units))
    }
    if (is.null(origin)) {
      ref <- t[1]; off <- mean(to_num(t[-1], t[-length(t)]), na.rm = TRUE)
    } else { ref <- origin; off <- 0 }
    function(x) to_num(x, ref) + off
  } else if (is.numeric(t) || is.integer(t)) {
    o <- if (is.null(origin)) 0 else origin
    function(x) as.numeric(x - o)
  } else stop("Unsupported class for edgelist$time. Use numeric/integer, Date, or POSIXct/POSIXt.")
}

# ── Column-name normalisation ─────────────────────────────────────────────────

#' Normalise duration edgelist column names
#'
#' Accepts both legacy names (start_time / sender / receiver / end_time) and
#' the current remverse standard (time / actor1 / actor2 / end), plus an
#' alternative \code{duration} column from which \code{end} is derived as
#' \code{time + duration}.
#'
#' @param edgelist A \code{data.frame}.
#' @return The same \code{data.frame} with columns renamed to the standard and,
#'   if applicable, \code{end} computed from \code{duration}.
#' @keywords internal
.durem_normalize_edgelist <- function(edgelist) {
    nms <- names(edgelist)

    # start time
    if ("start_time" %in% nms && !"time" %in% nms)
        names(edgelist)[names(edgelist) == "start_time"] <- "time"

    # first actor
    if ("sender" %in% nms && !"actor1" %in% nms)
        names(edgelist)[names(edgelist) == "sender"] <- "actor1"

    # second actor
    if ("receiver" %in% nms && !"actor2" %in% nms)
        names(edgelist)[names(edgelist) == "receiver"] <- "actor2"

    # positional fallback for the start triple, mirroring remify():
    # if a standard name is still absent, assume its canonical column position.
    if (!"time"   %in% names(edgelist)) names(edgelist)[1] <- "time"
    if (!"actor1" %in% names(edgelist)) names(edgelist)[2] <- "actor1"
    if (!"actor2" %in% names(edgelist)) names(edgelist)[3] <- "actor2"

    # end time — prefer "end", fall back to "end_time", or compute from duration
    if ("end_time" %in% names(edgelist) && !"end" %in% names(edgelist))
        names(edgelist)[names(edgelist) == "end_time"] <- "end"

    if (!"end" %in% names(edgelist) && "duration" %in% names(edgelist))
        edgelist$end <- edgelist$time + edgelist$duration

    # validate required columns are now present
    required <- c("time", "actor1", "actor2", "end")
    missing_cols <- setdiff(required, names(edgelist))
    if (length(missing_cols) > 0L)
        stop(
            "edgelist is missing required column(s): ",
            paste(missing_cols, collapse = ", "), ".\n",
            "Expected: time (or start_time), actor1 (or sender), ",
            "actor2 (or receiver), end (or end_time, or derived from duration)."
        )

    edgelist
}


# ── Constructor (called by remify() when duration = TRUE) ────────────────────

#' Internal constructor for \code{remify_durem} objects
#'
#' Called by \code{\link{remify}} when \code{duration = TRUE}. Normalises and
#' validates the duration edgelist, builds a standard \code{remify} object from
#' the start events (to obtain the actor dictionary and risk set structure), and
#' attaches a \code{$durem} slot with all duration-specific metadata.
#'
#' @inheritParams remify
#' @param directed_end Logical. \code{FALSE} (default) means the end process is
#'   undirected: either actor can terminate the event and only a combined
#'   dyad-level end rate is modelled (TOM end). \code{TRUE} means the end
#'   process is directed: a \code{who_ended} column in the edgelist records
#'   which actor terminated each event. When \code{who_ended} is present, actor-
#'   level end rates are separately estimable (competing risks / AOM end). When
#'   \code{who_ended} is absent, actor1 is assumed to terminate all events and a
#'   message is issued.
#' @param type_exclusive Logical. Only relevant when
#'   \code{extend_riskset_by_type = TRUE}. If \code{TRUE}, an active event of
#'   any type is a hard block on starting events of all other types for the same
#'   dyad. Default \code{FALSE}.
#' @return An object of class \code{c("remify_durem", "remify")}.
#' @keywords internal
.remify_durem_init <- function(
    edgelist,
    directed               = TRUE,
    ordinal                = FALSE,
    model                  = "tie",
    aggregate_time         = 1L,
    actors                 = NULL,
    riskset                = c("full", "active", "active_saturated", "manual"),
    manual_riskset         = NULL,
    extend_riskset_by_type = FALSE,
    event_type             = NULL,
    origin                 = NULL,
    time_units             = c("auto", "secs", "mins", "hours", "days", "weeks", "months", "years"),
    attach_riskset         = TRUE,
    riskset_decode         = c("labels", "ids", "none"),
    riskset_max_decode     = 200000L,
    event_attributes       = NULL,
    ncores                 = 1L,
    omit_dyad              = NULL,
    directed_end           = FALSE,
    type_exclusive         = FALSE
) {
  # ── 1. Normalise column names ─────────────────────────────────────────────
  edgelist <- .durem_normalize_edgelist(edgelist)
  # Put start and end on the SAME numeric axis before remify rescales starts.
  .map <- .remify_make_time_map(edgelist$time, match.arg(time_units), origin)
  edgelist$time <- .map(edgelist$time)
  edgelist$end  <- .map(edgelist$end)

  # ── 2. Validate ───────────────────────────────────────────────────────────
  riskset <- match.arg(riskset)

  # Remove self-loops (actor1 == actor2) before anything else, mirroring
  # remify's own behaviour.  Do this on the full edgelist so that the row
  # counts of edgelist, start_el, and base_reh$edgelist stay in sync.
  self_loops <- edgelist$actor1 == edgelist$actor2
  if (any(self_loops)) {
    warning(
      "Self-loops detected in the duration edgelist (actor1 == actor2). ",
      sum(self_loops), " row(s) removed."
    )
    edgelist <- edgelist[!self_loops, , drop = FALSE]
    rownames(edgelist) <- NULL
  }

  # end >= time for non-censored events
  complete <- !is.na(edgelist$end)
  if (any(edgelist$end[complete] < edgelist$time[complete]))
    stop("End time cannot be before start time.")

  # model restriction
  model <- match.arg(model, c("tie", "actor"))
  if (model == "actor")
    stop("AOM-DuREM is not yet implemented. Use model = \"tie\".")

  # type_exclusive without extend_riskset_by_type is a no-op
  if (isTRUE(type_exclusive) && !isTRUE(extend_riskset_by_type))
    warning(
      "`dur_type_exclusive = TRUE` has no effect when ",
      "`extend_riskset_by_type = FALSE`. The dyad-level risk set ",
      "already enforces mutual exclusion across types."
    )

  # directed_end = TRUE without who_ended: actor1 assumed to always terminate
  if (isTRUE(directed_end) && !"who_ended" %in% names(edgelist))
    message(
      "Note: `directed_end = TRUE` but no `who_ended` column found. ",
      "Actor contributions to the end rate are not separately identifiable; ",
      "actor1 is assumed to terminate all events."
    )

  # who_ended values must be "actor1" or "actor2" if column is present
  if ("who_ended" %in% names(edgelist)) {
    valid_enders <- edgelist$who_ended[!is.na(edgelist$who_ended)]
    bad <- !valid_enders %in% c("actor1", "actor2")
    if (any(bad))
      stop(
        "`who_ended` column must contain only \"actor1\", \"actor2\", ",
        "or NA. Found: ",
        paste(unique(valid_enders[bad]), collapse = ", ")
      )
  }

  # validate event_attributes
  if (!is.null(event_attributes)) {
    if (!is.character(event_attributes)) stop("`event_attributes` must be a character vector of column names.")
    event_attributes <- unique(event_attributes)
    missing <- setdiff(event_attributes, names(edgelist))
    if (length(missing)) stop("`event_attributes` not found in `edgelist`: ", paste(missing, collapse = ", "))
    event_attributes <- setdiff(event_attributes, c("time","actor1","actor2","type","weight"))
    if (!length(event_attributes)) event_attributes <- NULL
  }

  # ── 3. Build start edgelist for the base remify object ────────────────────
  # Only start events are passed to remify() so the actor dictionary, N,
  # and risk set structure reflect the start process. Optional type, weight
  # and event atributes columns are forwarded
  start_cols <- c("time", "actor1", "actor2")
  if ("type"   %in% names(edgelist)) start_cols <- c(start_cols, "type")
  if ("weight" %in% names(edgelist)) start_cols <- c(start_cols, "weight")
  if (!is.null(event_attributes)) {start_cols <- c(start_cols, event_attributes)}
  start_el <- edgelist[, start_cols, drop = FALSE]

  # ── 4. Call remify() on start events ─────────────────────────────────────
  # directed controls start directionality (TRUE = actor1 initiates,
  # FALSE = either actor); directed_end is stored in $durem for the end model.
  base_reh <- remify(
    edgelist               = start_el,
    directed               = directed,
    ordinal                = FALSE, #handled later
    model                  = model,
    aggregate_time         = aggregate_time,
    actors                 = actors,
    riskset                = riskset,
    manual_riskset         = manual_riskset,
    extend_riskset_by_type = extend_riskset_by_type,
    event_type             = event_type,
    origin                 = origin,
    time_units             = time_units,
    attach_riskset         = attach_riskset,
    riskset_decode         = riskset_decode,
    riskset_max_decode     = riskset_max_decode,
    event_attributes       = event_attributes,
    ncores                 = ncores#,
#    omit_dyad              = omit_dyad
  )

  # ── 5. Add end (and who_ended) to $edgelist ──────────────────────────────
  # The base remify $edgelist has time/actor1/actor2 (and type/weight if
  # present). We append the end column so the full duration edgelist is
  # accessible at the standard location.
  #
  # NOTE: $edgelist_id is intentionally NOT given an end column. remify
  # collapses simultaneous events into a single merged row in $edgelist_id
  # (e.g. two events at t=15 become one row with actor1 = "2, 1"), so its row
  # count can be less than $edgelist. All DuREM duration logic reads from
  # $edgelist and $edgelist_dual; $edgelist_id is only needed by remstats C++
  # for integer actor/dyad lookups and does not require duration columns.
  #
  # who_ended stays as character — no integer encoding needed.
  base_reh$edgelist$end <- edgelist$end
  if ("who_ended" %in% names(edgelist))
    base_reh$edgelist$who_ended <- edgelist$who_ended

  # ── 5b. Build and attach dual-event edgelist ──────────────────────────────
  # The dual edgelist represents the full event history as a sequence of
  # "start" and "end" events sorted chronologically.  Stored on the object so
  # that .remstats_durem() can consume it directly without rebuilding it on
  # every call.
  #
  # Columns:
  #   time     – event time (el$time for start rows, el$end for end rows)
  #   actor1, actor2 – dyad (same on both rows of an event)
  #   status   – "start" or "end"
  #   duration – precomputed el$end - el$time + 1; 1 for censored start rows
  #   weight   – base event weight (user's weight column or 1)
  #   [type]   – user's event-type column carried through unchanged
  #
  # Recompute 'complete' here in case edgelist was trimmed by alignment above.
  complete    <- !is.na(edgelist$end)
  has_type    <- "type"   %in% names(edgelist)
  has_weight  <- "weight" %in% names(edgelist)
  base_weight <- if (has_weight) edgelist$weight else rep(1, nrow(edgelist))
  dur         <- ifelse(is.na(edgelist$end), 1,
                        edgelist$end - edgelist$time)

  #check overlapping events of the same dyad
  .check_duration_overlap(edgelist, type_exclusive = type_exclusive)

  start_rows_dual <- data.frame(
    time     = edgelist$time,
    actor1   = edgelist$actor1,
    actor2   = edgelist$actor2,
    status   = "start",
    duration = dur,
    stringsAsFactors = FALSE
  )
  end_rows_dual <- data.frame(
    time     = edgelist$end[complete],
    actor1   = edgelist$actor1[complete],
    actor2   = edgelist$actor2[complete],
    status   = "end",
    duration = dur[complete],
    stringsAsFactors = FALSE
  )
  # weight column only when user supplied one; otherwise omitted (remify
  # treats a missing weight column as uniform weight = 1)
  if (has_weight) {
    start_rows_dual$weight <- base_weight
    end_rows_dual$weight   <- base_weight[complete]
  }
  if (has_type) {
    start_rows_dual$type <- edgelist$type
    end_rows_dual$type   <- edgelist$type[complete]
  }
  if (!is.null(event_attributes)) {
    for (ea in event_attributes) {
      start_rows_dual[[ea]] <- edgelist[[ea]]
      end_rows_dual[[ea]]   <- edgelist[[ea]][complete]
    }
  }
  # who_ended column — only when directed_end = TRUE:
  #   • if user supplied a who_ended column: use those values for end rows
  #   • if directed_end = TRUE but no who_ended column: assume actor1
  #     (consistent with the message issued in section 2)
  # Both start_rows_dual and end_rows_dual must carry the column (with NA
  # on start rows) so that rbind() sees matching column sets.
  if (isTRUE(directed_end)) {
    start_rows_dual$who_ended <- NA_character_
    end_rows_dual$who_ended <-
      if ("who_ended" %in% names(edgelist))
        edgelist$who_ended[complete]
    else
      rep("actor1", sum(complete))
  }
  start_rows_dual$.eidx <- seq_len(nrow(edgelist))
  end_rows_dual$.eidx   <- which(complete)

  edgelist_dual <- rbind(start_rows_dual, end_rows_dual)
  edgelist_dual <- edgelist_dual[order(edgelist_dual$time), ]
  rownames(edgelist_dual) <- NULL
  base_reh$edgelist_dual <- edgelist_dual

  if (isTRUE(ordinal)) {
    # Convert numeric time to an integer step index over unique time values.
    # Events with identical times share the same index; indices increase densely.
    all_times <- sort(unique(edgelist_dual$time))
    tmap <- setNames(seq_along(all_times), as.character(all_times))

    base_reh$edgelist_dual$time <- unname(tmap[as.character(base_reh$edgelist_dual$time)])
    base_reh$edgelist$time      <- unname(tmap[as.character(base_reh$edgelist$time)])
    base_reh$edgelist_id$time <- unname(tmap[as.character(base_reh$edgelist_id$time)])
    ok <- !is.na(base_reh$edgelist$end)
    base_reh$edgelist$end[ok]   <- unname(tmap[as.character(base_reh$edgelist$end[ok])])

    # Ordinal duration from remapped main edgelist
    ord_dur <- base_reh$edgelist$end - base_reh$edgelist$time
    base_reh$edgelist_dual$duration <- ord_dur[base_reh$edgelist_dual$.eidx]
  #  base_reh$edgelist_dual$.eidx    <- NULL  # remove temp column
    base_reh$intereventTime <- NULL
    base_reh$meta$ordinal <- TRUE
  }
  #base_reh$edgelist_dual$.eidx <- NULL

  # ── 6. Attach DuREM slot — only info not already on the base object ───────
  # Redundant fields intentionally omitted:
  #   n_events               → $M
  #   has_types              → $meta$with_type
  #   has_weights            → $meta$weighted
  #   extend_riskset_by_type → $meta$with_type_riskset
  base_reh$durem <- list(
    dur_directed_end  = directed_end,
    dur_type_exclusive = type_exclusive,
    has_who_ended = "who_ended" %in% names(edgelist),
    has_censored  = anyNA(edgelist$end),
    n_complete    = sum(!is.na(edgelist$end)),
    n_censored    = sum(is.na(edgelist$end))
  )

  # ── 7. Set class ──────────────────────────────────────────────────────────
  class(base_reh) <- c("remify_durem", class(base_reh))

  if (any(edgelist$duration == 0, na.rm = TRUE))
    warning(sum(edgelist$duration == 0), " event(s) with duration = 0; ",
            "treated as instantaneous (start only, event end will not be modeled).",
            call. = FALSE)

  base_reh

}

# ── Overlap check for duration models ────────────────────────────────────────
# Call this from .remify_durem_init() after computing end times.
# el must have columns: actor1, actor2, time, end, and optionally type.

.check_duration_overlap <- function(el, type_exclusive = TRUE) {
  n <- nrow(el)
  if (n < 2L) return(invisible(NULL))

  has_type <- "type" %in% names(el)
  same_type_violations <- integer(0)
  cross_type_violations <- integer(0)

  for (i in seq_len(n)) {
    # Skip instantaneous events (already warned separately)
    if (!is.na(el$end[i]) && el$end[i] == el$time[i]) next

    # Find other events for the same base dyad that were active when i started
    same_dyad <- which(
      el$actor1 == el$actor1[i] &
        el$actor2 == el$actor2[i] &
        seq_len(n) != i &
        el$time < el$time[i] &
        el$end > el$time[i]
    )
    if (length(same_dyad) == 0L) next

    if (has_type) {
      same_t <- same_dyad[el$type[same_dyad] == el$type[i]]
      cross_t <- same_dyad[el$type[same_dyad] != el$type[i]]
      if (length(same_t) > 0L) same_type_violations <- c(same_type_violations, i)
      if (length(cross_t) > 0L && type_exclusive) cross_type_violations <- c(cross_type_violations, i)
    } else {
      same_type_violations <- c(same_type_violations, i)
    }
  }

  same_type_violations <- unique(same_type_violations)
  cross_type_violations <- unique(cross_type_violations)

  if (length(same_type_violations) > 0L) {
    if (length(same_type_violations) > 1L) {
    warning(
      length(same_type_violations), " events start while another event ",
      "for the same dyad is already active. Check events: ",
      paste(same_type_violations, collapse = ", "), ".",
      call. = FALSE
    )
    }else{
      warning(
        length(same_type_violations), " event starts while another event ",
        "for the same dyad is already active. Check event: ",
        paste(same_type_violations, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  if (length(cross_type_violations) > 0L) {
    if (length(cross_type_violations) > 1L) {
    warning(
      length(cross_type_violations), " events start while an event of a ",
      "different type for the same dyad is already active. With ",
      "dur_type_exclusive = TRUE, these dyads should not be at risk. ",
      "Consider setting dur_type_exclusive = FALSE if concurrent events of ",
      "different types are expected. Check events: ",
      paste(cross_type_violations, collapse = ", "), ".",
      call. = FALSE
    )
    }else{
      warning(
        length(cross_type_violations), " event starts while an event of a ",
        "different type for the same dyad is already active. With ",
        "dur_type_exclusive = TRUE, this dyad should not be at risk. ",
        "Consider setting dur_type_exclusive = FALSE if concurrent events of ",
        "different types are expected. Check event: ",
        paste(cross_type_violations, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  invisible(unique(c(same_type_violations, cross_type_violations)))
}

# ── S3 methods ────────────────────────────────────────────────────────────────

#' Test whether an object is a \code{remify_durem}
#'
#' @param x Any R object.
#' @return \code{TRUE} if \code{x} inherits from \code{"remify_durem"}.
#' @export
is.remify_durem <- function(x) inherits(x, "remify_durem")


#' Print method for \code{remify_durem}
#'
#' Mirrors \code{print.remify}: delegates to \code{summary.remify_durem}.
#'
#' @param x A \code{remify_durem} object.
#' @param ... Passed to \code{summary.remify_durem}.
#' @export
print.remify_durem <- function(x, ...) {
    summary(object = x, ...)
}


#' Summary method for \code{remify_durem}
#'
#' Produces output in the same format as \code{summary.remify}, with the
#' model line extended to read "(processed for tie-oriented modeling with
#' duration)" and DuREM-specific fields (start/end direction, duration
#' statistics) appended at the end.
#'
#' @param object A \code{remify_durem} object.
#' @param ... Ignored.
#' @export
summary.remify_durem <- function(object, ...) {
    d   <- object$durem
    el  <- object$edgelist   # contains time, actor1, actor2, end, [who_ended]

    .model             <- object$meta$model
    .with_type         <- isTRUE(object$meta$with_type)
    .with_type_riskset <- isTRUE(object$meta$with_type_riskset)
    .riskset           <- object$meta$riskset %||% object$meta$riskset_source
    .directed          <- object$meta$directed
    .ordinal           <- object$meta$ordinal
    .weighted          <- object$meta$weighted
    .origin            <- object$meta$origin
    .C                 <- if (!is.null(object$C)) object$C else 1L

    # ── Title ─────────────────────────────────────────────────────────────────
    title <- "Relational Event Network"
    model <- paste0("(processed for ", .model, "-oriented modeling with duration):")

    # ── Events (modified: show censoring) ─────────────────────────────────────
    if (d$has_censored) {
        events <- paste0("\t> events = ", object$M,
                         " (complete: ", d$n_complete,
                         " | right-censored: ", d$n_censored, ")")
    } else {
        if (is.null(object$E)) {
            events <- paste0("\t> events = ", object$M)
        } else {
            events <- paste0("\t> events = ", object$E,
                             " (time points = ", object$M, ")")
        }
    }

    # ── Standard fields (verbatim from summary.remify) ────────────────────────
    actors <- paste0("\t> actors = ", object$N)
    types  <- paste0("\t> (event) types = ", .C)

    riskset_block <- character(0)
    ext_note      <- NULL

    riskset_line    <- paste0("\t> riskset = ", .riskset)
    riskset_details <- character(0)

    if (.riskset %in% c("active", "manual")) {
        D_active <- object$activeD
        D_full   <- object$D
        if (.with_type_riskset) {
            D_pairs <- D_full / .C
            riskset_details <- c(riskset_details,
                paste0("\t\t>> active dyads = ", D_active,
                       " (full risk set size = ", D_full,
                       " typed dyads, ", D_pairs, " actor pairs)"))
        } else {
            riskset_details <- c(riskset_details,
                paste0("\t\t>> active dyads = ", D_active,
                       " (full risk set size = ", D_full, " actor pairs)"))
        }
    } else {
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

    if (.with_type && .C > 1L)
        ext_note <- paste0("\t\t>> extend_riskset_by_type = ", .with_type_riskset)

    directed <- paste0("\t> directed = ", .directed)
    ordinal  <- paste0("\t> ordinal = ",  .ordinal)
    weighted <- paste0("\t> weighted = ", .weighted)

    time_length     <- NULL
    interevent_time <- NULL
    time <- el$time

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
        interevent_time <- paste0(
            "\t> interevent time \n\t\t >> minimum ~ ", round(min_iet, 4),
            " ", units_minmax,
            "\n\t\t >> maximum ~ ", round(max_iet, 4),
            " ", units_minmax)
    }

    # ── DuREM-specific fields ─────────────────────────────────────────────────
    start_lbl <- if (isTRUE(.directed))
        "directed (actor1 initiates)"
    else
        "undirected (either actor)"

    end_lbl <- if (!isTRUE(d$directed_end))
        "undirected (combined dyad-level rate)"
    else if (d$has_who_ended)
        "directed (who_ended observed)"
    else
        "directed (actor1 assumed to terminate)"

    dur_block <- NULL
    if (d$n_complete > 0L && !.ordinal) {
        durs <- el$end[!is.na(el$end)] - el$time[!is.na(el$end)]
        units_dur <- NULL
        if (inherits(time, "Date")) {
            units_dur <- "days"
        } else if (!is.numeric(time) && !is.integer(time)) {
            units_dur <- "seconds"
        }
        dur_block <- paste0(
            "\t\t>> event duration (complete events) \n\t\t\t >>> minimum ~ ",
            round(min(durs),    4), " ", units_dur,
            "\n\t\t\t >>> median  ~ ", round(median(durs), 4), " ", units_dur,
            "\n\t\t\t >>> maximum ~ ", round(max(durs),    4), " ", units_dur, "\n")
    }

    type_excl <- if (isTRUE(d$dur_type_exclusive))
        "\t\t>> dur_type_exclusive = TRUE"
    else
        NULL

    duration_section <- c(
        "\t> duration",
        paste0("\t\t>> start = ", start_lbl),
        paste0("\t\t>> end   = ", end_lbl),
        dur_block,
        type_excl
    )

    # ── Assemble and print ────────────────────────────────────────────────────
    out <- c(title, model, events, actors, types,
             riskset_block, ext_note,
             directed, ordinal, weighted,
             time_length, interevent_time,
             duration_section)
    out <- out[!sapply(out, is.null)]
    cat(paste(out, collapse = "\n"))

    invisible(object)
}
