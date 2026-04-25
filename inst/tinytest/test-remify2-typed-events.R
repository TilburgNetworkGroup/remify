# inst/tinytest/test-remify2-typed-events.R
#
# Tests for typed events (event_type argument) and the
# extend_riskset_by_type flag, which controls whether event types
# are also used to define the riskset.
#
# Naming convention used below:
#   ext = TRUE  -> typed riskset  (D = D_dyad * C)
#   ext = FALSE -> untyped riskset (D = D_dyad, C_riskset = 1)

library(tinytest)

# ---- helpers -------------------------------------------------------

make_typed_edgelist <- function(directed = TRUE, simultaneous = FALSE) {
  set.seed(42)
  actors <- sprintf("%03d", c(101, 103, 104, 105, 107, 109))
  M  <- 30L
  a1 <- sample(actors, M, replace = TRUE)
  a2 <- sample(actors, M, replace = TRUE)
  while (any(a1 == a2)) {
    bad <- which(a1 == a2)
    a2[bad] <- sample(actors, length(bad), replace = TRUE)
  }
  if (!directed) {
    lo <- pmin(a1, a2); hi <- pmax(a1, a2); a1 <- lo; a2 <- hi
  }
  df <- data.frame(
    time   = cumsum(sample(5:100, M, replace = TRUE)),
    actor1 = a1,
    actor2 = a2,
    type   = sample(c("social", "work"), M, replace = TRUE),
    stringsAsFactors = FALSE
  )
  if (simultaneous) {
    df$time[5] <- df$time[6]   # one pair of simultaneous events
  }
  df
}

h   <- make_typed_edgelist(directed = TRUE)
N   <- length(unique(c(h$actor1, h$actor2)))
D_dyad_directed <- N * (N - 1L)
C   <- 2L   # "social" and "work"


# ====================================================================
# 1. FULL riskset, extend_riskset_by_type = TRUE (default)
# ====================================================================

reh_ext <- remify::remify(
  edgelist = h, model = "tie", riskset = "full",
  event_type = "type", extend_riskset_by_type = TRUE,
  attach_riskset = TRUE, riskset_decode = "labels"
)

# meta flags
expect_true(isTRUE(reh_ext$meta$with_type),
  info = "ext=TRUE: events are typed")
expect_true(isTRUE(reh_ext$meta$with_type_riskset),
  info = "ext=TRUE: riskset is typed")
expect_equal(reh_ext$meta$C_riskset, C,
  info = "ext=TRUE: C_riskset equals number of types")

# dimensions
expect_equal(reh_ext$C, C,
  info = "ext=TRUE: C equals number of event types")
expect_equal(reh_ext$D, D_dyad_directed * C,
  info = "ext=TRUE: D = D_dyad * C")

# riskset_info
expect_true("type" %in% names(reh_ext$riskset_info$included),
  info = "ext=TRUE: riskset_info has type column")
expect_equal(nrow(reh_ext$riskset_info$included), D_dyad_directed * C,
  info = "ext=TRUE: riskset_info has D_dyad * C rows")
expect_equal(sort(unique(reh_ext$riskset_info$included$type)),
             c("social", "work"),
  info = "ext=TRUE: both types present in riskset_info")

# edgelist retains type column
expect_true("type" %in% names(reh_ext$edgelist),
  info = "ext=TRUE: edgelist still has type column")

# dyadID consistency
dyad_vec <- as.integer(reh_ext$ids$dyad)
expect_true(all(dyad_vec >= 1L) && all(dyad_vec <= reh_ext$D),
  info = "ext=TRUE: all dyadIDs within 1..D")


# ====================================================================
# 2. FULL riskset, extend_riskset_by_type = FALSE
# ====================================================================

reh_no <- remify::remify(
  edgelist = h, model = "tie", riskset = "full",
  event_type = "type", extend_riskset_by_type = FALSE,
  attach_riskset = TRUE, riskset_decode = "labels"
)

# meta flags
expect_true(isTRUE(reh_no$meta$with_type),
  info = "ext=FALSE: events are still typed")
expect_false(isTRUE(reh_no$meta$with_type_riskset),
  info = "ext=FALSE: riskset is NOT typed")
expect_equal(reh_no$meta$C_riskset, 1L,
  info = "ext=FALSE: C_riskset = 1")

# dimensions
expect_equal(reh_no$C, C,
  info = "ext=FALSE: C (event types) still equals C")
expect_equal(reh_no$D, D_dyad_directed,
  info = "ext=FALSE: D = D_dyad only (no type expansion)")

# riskset_info: NO type column
expect_false("type" %in% names(reh_no$riskset_info$included),
  info = "ext=FALSE: riskset_info has no type column")
expect_equal(nrow(reh_no$riskset_info$included), D_dyad_directed,
  info = "ext=FALSE: riskset_info has D_dyad rows")

# edgelist still has type column (events are typed regardless)
expect_true("type" %in% names(reh_no$edgelist),
  info = "ext=FALSE: edgelist still has type column")

# dyadID consistency
dyad_vec_no <- as.integer(reh_no$ids$dyad)
expect_true(all(dyad_vec_no >= 1L) && all(dyad_vec_no <= reh_no$D),
  info = "ext=FALSE: all dyadIDs within 1..D_dyad")

# ids$type is populated (events are typed)
expect_false(is.null(reh_no$ids$type),
  info = "ext=FALSE: ids$type is not NULL")
expect_equal(length(reh_no$ids$type), nrow(h),
  info = "ext=FALSE: ids$type has one entry per event")


# ====================================================================
# 3. D differs between ext=TRUE and ext=FALSE; C is the same
# ====================================================================

expect_equal(reh_ext$D, reh_no$D * C,
  info = "D(ext=TRUE) = D(ext=FALSE) * C")
expect_equal(reh_ext$C, reh_no$C,
  info = "C is the same regardless of extend_riskset_by_type")


# ====================================================================
# 4. ACTIVE riskset, extend_riskset_by_type = TRUE
# ====================================================================

reh_act_ext <- remify::remify(
  edgelist = h, model = "tie", riskset = "active",
  event_type = "type", extend_riskset_by_type = TRUE,
  attach_riskset = TRUE, riskset_decode = "labels"
)

expect_true(isTRUE(reh_act_ext$meta$with_type_riskset),
  info = "active ext=TRUE: riskset is typed")
expect_true(reh_act_ext$D <= D_dyad_directed * C,
  info = "active ext=TRUE: active D <= full D")
expect_true("type" %in% names(reh_act_ext$riskset_info$included),
  info = "active ext=TRUE: riskset_info has type column")

# every observed (actor1, actor2, type) combo must be in the riskset
obs_keys  <- paste(h$actor1, h$actor2, h$type, sep = "||")
rs_inc    <- reh_act_ext$riskset_info$included
rs_keys   <- paste(rs_inc$actor1, rs_inc$actor2, rs_inc$type, sep = "||")
expect_true(all(unique(obs_keys) %in% rs_keys),
  info = "active ext=TRUE: all observed typed dyads in riskset")

# dyadID mapping holds: riskset_idx[dyadIDactive] == dyadID_vec
dyad_full <- as.integer(reh_act_ext$ids$dyad)
rs        <- reh_act_ext$riskset_info
expect_equal(rs$riskset_idx[rs$dyadIDactive], dyad_full,
  info = "active ext=TRUE: dyadID mapping consistent")


# ====================================================================
# 5. ACTIVE riskset, extend_riskset_by_type = FALSE
# ====================================================================

reh_act_no <- remify::remify(
  edgelist = h, model = "tie", riskset = "active",
  event_type = "type", extend_riskset_by_type = FALSE,
  attach_riskset = TRUE, riskset_decode = "labels"
)

expect_false(isTRUE(reh_act_no$meta$with_type_riskset),
  info = "active ext=FALSE: riskset is NOT typed")
expect_true(reh_act_no$D <= D_dyad_directed,
  info = "active ext=FALSE: active D <= D_dyad")
expect_false("type" %in% names(reh_act_no$riskset_info$included),
  info = "active ext=FALSE: riskset_info has no type column")

# every observed (actor1, actor2) pair must be in the riskset
obs_pair_keys <- paste(h$actor1, h$actor2, sep = "||")
rs_inc_no     <- reh_act_no$riskset_info$included
rs_pair_keys  <- paste(rs_inc_no$actor1, rs_inc_no$actor2, sep = "||")
expect_true(all(unique(obs_pair_keys) %in% rs_pair_keys),
  info = "active ext=FALSE: all observed actor pairs in riskset")

# dyadID mapping holds
dyad_full_no <- as.integer(reh_act_no$ids$dyad)
rs_no        <- reh_act_no$riskset_info
expect_equal(rs_no$riskset_idx[rs_no$dyadIDactive], dyad_full_no,
  info = "active ext=FALSE: dyadID mapping consistent")

# active D is smaller when untyped (same or fewer unique pairs than typed dyads)
expect_true(reh_act_no$activeD <= reh_act_ext$activeD,
  info = "active ext=FALSE: activeD <= activeD of typed riskset")


# ====================================================================
# 6. MANUAL riskset, extend_riskset_by_type = TRUE
# ====================================================================

manual_rs <- unique(h[, c("actor1", "actor2")])[1:5, ]

reh_man_ext <- suppressWarnings(remify::remify(
  edgelist = h, model = "tie", riskset = "manual",
  manual.riskset = manual_rs,
  event_type = "type", extend_riskset_by_type = TRUE,
  attach_riskset = TRUE, riskset_decode = "labels"
))

expect_true(isTRUE(reh_man_ext$meta$with_type_riskset),
  info = "manual ext=TRUE: riskset is typed")
expect_true("type" %in% names(reh_man_ext$riskset_info$included),
  info = "manual ext=TRUE: riskset_info has type column")

# manual dyads × types should be the base of the riskset
expected_n <- nrow(manual_rs) * C
expect_true(reh_man_ext$activeD >= expected_n,
  info = "manual ext=TRUE: activeD >= manual_dyads * C (observed may add more)")


# ====================================================================
# 7. MANUAL riskset, extend_riskset_by_type = FALSE
# ====================================================================

reh_man_no <- suppressWarnings(remify::remify(
  edgelist = h, model = "tie", riskset = "manual",
  manual.riskset = manual_rs,
  event_type = "type", extend_riskset_by_type = FALSE,
  attach_riskset = TRUE, riskset_decode = "labels"
))

expect_false(isTRUE(reh_man_no$meta$with_type_riskset),
  info = "manual ext=FALSE: riskset is NOT typed")
expect_false("type" %in% names(reh_man_no$riskset_info$included),
  info = "manual ext=FALSE: riskset_info has no type column")
expect_true(reh_man_no$activeD >= nrow(manual_rs),
  info = "manual ext=FALSE: activeD >= number of manual dyads")


# ====================================================================
# 8. No event types (event_type = NULL): extend_riskset_by_type ignored
# ====================================================================

h_untyped <- h[, c("time", "actor1", "actor2")]

reh_untyped <- remify::remify(
  edgelist = h_untyped, model = "tie", riskset = "full",
  event_type = NULL, extend_riskset_by_type = TRUE,  # flag ignored
  attach_riskset = TRUE, riskset_decode = "labels"
)

expect_false(isTRUE(reh_untyped$meta$with_type),
  info = "no types: with_type = FALSE")
expect_false(isTRUE(reh_untyped$meta$with_type_riskset),
  info = "no types: with_type_riskset = FALSE")
expect_equal(reh_untyped$D, D_dyad_directed,
  info = "no types: D = D_dyad (no type expansion)")
expect_false("type" %in% names(reh_untyped$riskset_info$included),
  info = "no types: riskset_info has no type column")
expect_true(is.null(reh_untyped$ids$type),
  info = "no types: ids$type is NULL")


# ====================================================================
# 9. Simultaneous events + typed riskset
# ====================================================================

hS <- make_typed_edgelist(directed = TRUE, simultaneous = TRUE)

reh_sim_ext <- remify::remify(
  edgelist = hS, model = "tie", riskset = "active",
  event_type = "type", extend_riskset_by_type = TRUE,
  attach_riskset = TRUE, riskset_decode = "ids"
)

expect_false(is.null(reh_sim_ext$simultaneous$indices),
  info = "sim ext=TRUE: simultaneous$indices detected")
expect_false(is.null(reh_sim_ext$ids$dyad_vec),
  info = "sim ext=TRUE: ids$dyad_vec present")
expect_false(is.null(reh_sim_ext$ids$type_vec),
  info = "sim ext=TRUE: ids$type_vec present")
expect_true(is.list(reh_sim_ext$ids$dyad),
  info = "sim ext=TRUE: ids$dyad is list (per time point)")
expect_true(is.list(reh_sim_ext$ids$type),
  info = "sim ext=TRUE: ids$type is list (per time point)")

# M < E (fewer time points than events due to simultaneous)
expect_true(reh_sim_ext$M < reh_sim_ext$E,
  info = "sim ext=TRUE: M < E")

# dyadID mapping still holds using vectorized ids
dyad_sim <- as.integer(reh_sim_ext$ids$dyad_vec)
rs_sim   <- reh_sim_ext$riskset_info
expect_equal(rs_sim$riskset_idx[rs_sim$dyadIDactive], dyad_sim,
  info = "sim ext=TRUE: dyadID mapping consistent with dyad_vec")


# ====================================================================
# 10. Simultaneous events + untyped riskset
# ====================================================================

reh_sim_no <- remify::remify(
  edgelist = hS, model = "tie", riskset = "active",
  event_type = "type", extend_riskset_by_type = FALSE,
  attach_riskset = TRUE, riskset_decode = "ids"
)

expect_false(isTRUE(reh_sim_no$meta$with_type_riskset),
  info = "sim ext=FALSE: riskset not typed")
expect_false(is.null(reh_sim_no$ids$type_vec),
  info = "sim ext=FALSE: ids$type_vec still present (events are typed)")
expect_false(is.null(reh_sim_no$simultaneous$indices),
  info = "sim ext=FALSE: simultaneous$indices detected")

dyad_sim_no <- as.integer(reh_sim_no$ids$dyad_vec)
rs_sim_no   <- reh_sim_no$riskset_info
expect_equal(rs_sim_no$riskset_idx[rs_sim_no$dyadIDactive], dyad_sim_no,
  info = "sim ext=FALSE: dyadID mapping consistent with dyad_vec")
