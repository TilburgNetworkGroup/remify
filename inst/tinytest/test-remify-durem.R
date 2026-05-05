## Tests for remify_durem — the remify() duration dispatch
##
## Covers:
##   - remify(duration = TRUE) returns the right class and $durem slot
##   - Column-name normalisation (legacy names, duration column)
##   - Validation errors (end < start, bad who_ended values)
##   - directed / directed_end / type_exclusive argument semantics
##   - Right-censored events (NA in end column)
##   - is.remify_durem(), print(), summary() run without error

library(tinytest)

# ── minimal test edgelists ─────────────────────────────────────────────────────

el <- data.frame(
    time   = c(1, 3, 6, 9),
    actor1 = c("A", "B", "A", "C"),
    actor2 = c("B", "C", "C", "A"),
    end    = c(2, 9, 8, 11)
)

el_legacy <- data.frame(
    start_time = c(1, 3, 6),
    sender     = c("A", "B", "A"),
    receiver   = c("B", "C", "C"),
    end_time   = c(2, 5, 8)
)

el_dur_col <- data.frame(
    time     = c(1, 3, 6),
    actor1   = c("A", "B", "A"),
    actor2   = c("B", "C", "C"),
    duration = c(1, 2, 2)          # end = time + duration
)

el_who <- data.frame(
    time      = c(1, 3, 6),
    actor1    = c("A", "B", "A"),
    actor2    = c("B", "C", "C"),
    end       = c(2, 5, 8),
    who_ended = c("actor1", "actor2", "actor1")
)

el_typed <- data.frame(
    time   = c(1, 3, 6, 9),
    actor1 = c("A", "B", "A", "C"),
    actor2 = c("B", "C", "C", "A"),
    end    = c(2, 5, 8, 11),
    type   = c("X", "Y", "X", "Y")
)

el_cens <- data.frame(
    time   = c(1, 3, 6),
    actor1 = c("A", "B", "A"),
    actor2 = c("B", "C", "C"),
    end    = c(2, NA, 8)           # one right-censored event
)

# ── 1. Class and basic slot structure ──────────────────────────────────────────

reh <- remify(el, duration = TRUE)

expect_inherits(reh, "remify_durem",
    info = "duration=TRUE: inherits remify_durem")
expect_inherits(reh, "remify",
    info = "duration=TRUE: also inherits remify")
expect_true(is.remify_durem(reh),
    info = "is.remify_durem() returns TRUE")

expect_true(!is.null(reh$durem),
    info = "$durem slot is present")

expect_equal(reh$durem$n_complete, 4L, info = "$durem$n_complete (no censoring)")
expect_equal(reh$durem$n_censored, 0L, info = "$durem$n_censored (no censoring)")

expect_false(reh$durem$directed_end,   info = "directed_end defaults to FALSE")
expect_false(reh$durem$type_exclusive, info = "type_exclusive defaults to FALSE")
expect_false(reh$durem$has_who_ended,  info = "has_who_ended FALSE when no who_ended column")
expect_false(reh$durem$has_censored,   info = "has_censored FALSE when no NA ends")

# fields that moved to base remify slots
expect_equal(reh$M, 4L,                              info = "n_events now lives in $M")
expect_false(isTRUE(reh$meta$with_type),             info = "has_types now lives in $meta$with_type")
expect_false(isTRUE(reh$meta$weighted),              info = "has_weights now lives in $meta$weighted")
expect_false(isTRUE(reh$meta$with_type_riskset),     info = "extend_riskset_by_type now in $meta$with_type_riskset")

# end column present in $edgelist and $edgelist_id
expect_true("end" %in% names(reh$edgelist),          info = "end column in $edgelist")
expect_equal(reh$edgelist$end, el$end,               info = "$edgelist$end matches input")

# ── 2. Column-name normalisation — legacy names ────────────────────────────────

reh_leg <- remify(el_legacy, duration = TRUE)

expect_true(all(c("time", "actor1", "actor2", "end") %in% names(reh_leg$edgelist)),
    info = "legacy column names normalised to standard")
expect_false("start_time" %in% names(reh_leg$durem$edgelist),
    info = "start_time removed after normalisation")
expect_false("sender" %in% names(reh_leg$durem$edgelist),
    info = "sender removed after normalisation")

# ── 3. Column-name normalisation — duration column ────────────────────────────

reh_dur <- remify(el_dur_col, duration = TRUE)

expect_true("end" %in% names(reh_dur$edgelist),
    info = "end derived from duration column")
expect_equal(reh_dur$edgelist$end,
             el_dur_col$time + el_dur_col$duration,
    info = "end = time + duration")

# ── 4. Validation — end before start ──────────────────────────────────────────

el_bad <- el
el_bad$end[2] <- el_bad$time[2] - 1   # end < start

expect_error(
    remify(el_bad, duration = TRUE),
    pattern = "End time cannot be before start time",
    info = "end < start triggers error"
)

# ── 5. Validation — invalid who_ended values ──────────────────────────────────

el_bad_who <- el_who
el_bad_who$who_ended[1] <- "both"

expect_error(
    remify(el_bad_who, duration = TRUE),
    pattern = "who_ended",
    info = "invalid who_ended value triggers error"
)

# ── 6. directed_end = TRUE with who_ended column ──────────────────────────────

reh_who <- remify(el_who, duration = TRUE, directed_end = TRUE)

expect_true(reh_who$durem$directed_end,   info = "directed_end stored as TRUE")
expect_true(reh_who$durem$has_who_ended,  info = "has_who_ended TRUE when column present")

# ── 7. directed_end = TRUE without who_ended column — message issued ──────────

expect_message(
    remify(el, duration = TRUE, directed_end = TRUE),
    pattern = "who_ended",
    info = "directed_end=TRUE without who_ended column issues a message"
)

# ── 8. directed = FALSE (undirected start) ────────────────────────────────────

reh_ud <- remify(el, duration = TRUE, directed = FALSE)

expect_false(isTRUE(reh_ud$directed),
    info = "directed=FALSE stored on base remify object")
expect_inherits(reh_ud, "remify_durem",
    info = "undirected start still produces remify_durem")

# ── 9. Right-censored events ──────────────────────────────────────────────────

reh_cens <- remify(el_cens, duration = TRUE)

expect_true(reh_cens$durem$has_censored,          info = "has_censored TRUE when NA end present")
expect_equal(reh_cens$durem$n_censored, 1L,       info = "n_censored = 1")
expect_equal(reh_cens$durem$n_complete, 2L,       info = "n_complete = 2")

# ── 10. type_exclusive warning when extend_riskset_by_type = FALSE ────────────

expect_warning(
    remify(el, duration = TRUE, type_exclusive = TRUE,
           extend_riskset_by_type = FALSE),
    pattern = "type_exclusive",
    info = "type_exclusive=TRUE without extend_riskset_by_type warns"
)

# ── 11. type_exclusive = TRUE with extend_riskset_by_type = TRUE ─────────────

reh_excl <- remify(el_typed, duration = TRUE,
                   type_exclusive         = TRUE,
                   extend_riskset_by_type = TRUE)

expect_true(reh_excl$durem$type_exclusive,              info = "type_exclusive stored in $durem")
expect_true(isTRUE(reh_excl$meta$with_type_riskset),   info = "extend_riskset_by_type in $meta$with_type_riskset")

# ── 12. Event types stored correctly ──────────────────────────────────────────

reh_typed <- remify(el_typed, duration = TRUE)

expect_true(isTRUE(reh_typed$meta$with_type),       info = "with_type TRUE when type column present")
expect_true("type" %in% names(reh_typed$edgelist),  info = "type column in $edgelist")

# ── 13. who_ended in $edgelist when present ───────────────────────────────────

expect_true("who_ended" %in% names(reh_who$edgelist),
    info = "who_ended column added to $edgelist")

# ── 14. model warning fires for duration = TRUE ───────────────────────────────

expect_warning(
    remify(el, duration = TRUE),
    pattern = "model.*tie",
    info = "model default warning fires when duration = TRUE and model not specified"
)

# ── 15. is.remify_durem on a plain remify object ─────────────────────────────

reh_plain <- remify(el[, c("time", "actor1", "actor2")])

expect_false(is.remify_durem(reh_plain),
    info = "is.remify_durem FALSE for a plain remify object")

# ── 17. print() and summary() run without error ───────────────────────────────

expect_silent(capture.output(print(reh)),
    info = "print.remify_durem runs without error")
expect_silent(capture.output(summary(reh)),
    info = "summary.remify_durem runs without error")

# print with directed_end and who_ended
expect_silent(capture.output(print(reh_who)),
    info = "print.remify_durem: directed_end + who_ended")

# print with censored events
expect_silent(capture.output(print(reh_cens)),
    info = "print.remify_durem: right-censored events shown")
