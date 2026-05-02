# inst/tinytest/test-remify2-thin.R
#
# Tests for the `thin` argument of remify().
#
# thin = k keeps every k-th unique time point (1st, 2nd, ..., k-th, 2k-th, ...)
# and remaps all events to the next kept time >= their original time.
# thin = 1 (default) is a strict no-op.

library(tinytest)

# ---- helper --------------------------------------------------------

make_el <- function(M = 40L, typed = FALSE, seed = 1L) {
  set.seed(seed)
  actors <- letters[1:6]
  a1 <- sample(actors, M, replace = TRUE)
  a2 <- sample(actors, M, replace = TRUE)
  while (any(a1 == a2)) {
    bad <- which(a1 == a2)
    a2[bad] <- sample(actors, length(bad), replace = TRUE)
  }
  df <- data.frame(
    time   = cumsum(sample(1:10, M, replace = TRUE)),
    actor1 = a1,
    actor2 = a2,
    stringsAsFactors = FALSE
  )
  if (typed) df$type <- sample(c("X", "Y"), M, replace = TRUE)
  df
}

h <- make_el(M = 40L, typed = FALSE)
M_orig <- nrow(h)
U_orig <- length(unique(h$time))   # number of unique time points before thinning


# ====================================================================
# 1. thin = 1 is a strict no-op
# ====================================================================

reh1 <- remify(h, model = "tie", aggregate_time = 1L)
reh0 <- remify(h, model = "tie")          # default thin = 1

expect_equal(nrow(reh1$edgelist), M_orig,
  info = "thin=1: number of events unchanged")
expect_equal(reh1$edgelist$time, reh0$edgelist$time,
  info = "thin=1: times identical to default")
expect_equal(reh1$M, reh0$M,
  info = "thin=1: M (time points) identical to default")


# ====================================================================
# 2. thin = 2 keeps every 2nd unique time point
# ====================================================================

reh2 <- remify(h, model = "tie", aggregate_time = 2L)

u_all  <- sort(unique(h$time))
kept_2 <- u_all[seq(2L, length(u_all), by = 2L)]

# all times in the output must be in the kept set
expect_true(all(reh2$edgelist$time %in% kept_2),
  info = "thin=2: all output times are kept time points")

# number of events is unchanged (all events survive, just remapped)
expect_equal(nrow(reh2$edgelist), M_orig,
  info = "thin=2: number of events unchanged")

# number of unique time points in output equals length(kept_2)
expect_equal(length(unique(reh2$edgelist$time)), length(kept_2),
  info = "thin=2: unique time points = length(kept)")

# M in the reh object reflects the thinned time grid
expect_equal(reh2$M, length(kept_2),
  info = "thin=2: reh$M equals number of kept time points")

# each event is mapped to the smallest kept time >= its original time
for (i in seq_len(M_orig)) {
  t_orig   <- h$time[i]
  t_out    <- reh2$edgelist$time[i]
  eligible <- kept_2[kept_2 >= t_orig]
  expect_equal(t_out, min(eligible),
    info = paste("thin=2: event", i, "mapped to nearest kept time"))
}


# ====================================================================
# 3. thin = 3
# ====================================================================

reh3 <- remify(h, model = "tie", aggregate_time = 3L)

kept_3 <- u_all[seq(3L, length(u_all), by = 3L)]

expect_true(all(reh3$edgelist$time %in% kept_3),
  info = "thin=3: all output times are kept time points")
expect_equal(reh3$M, length(kept_3),
  info = "thin=3: reh$M equals number of kept time points")

# larger thin -> fewer time points
expect_true(reh3$M <= reh2$M,
  info = "thin=3: fewer time points than thin=2")
expect_true(reh2$M <= reh1$M,
  info = "thin=2: fewer time points than thin=1")


# ====================================================================
# 4. thin = U_orig: only the last unique time point is kept
# ====================================================================

reh_U <- remify(h, model = "tie", aggregate_time = U_orig)

kept_U <- u_all[seq(U_orig, length(u_all), by = U_orig)]  # just u_all[U_orig]

expect_true(all(reh_U$edgelist$time %in% kept_U),
  info = "thin=U: all events remapped to the single kept time")
expect_equal(reh_U$M, 1L,
  info = "thin=U: only one time point remains")
expect_equal(nrow(reh_U$edgelist), M_orig,
  info = "thin=U: all events survive")


# ====================================================================
# 5. thin = 1 with typed events: no interaction effect
# ====================================================================

hT <- make_el(M = 30L, typed = TRUE)

reh_t1 <- remify(hT, model = "tie", event_type = "type", aggregate_time = 1L)
reh_t0 <- remify(hT, model = "tie", event_type = "type")

expect_equal(reh_t1$edgelist$time, reh_t0$edgelist$time,
  info = "thin=1 typed: times unchanged")
expect_equal(reh_t1$edgelist$type, reh_t0$edgelist$type,
  info = "thin=1 typed: types unchanged")


# ====================================================================
# 6. thin = 2 with typed events: type column preserved after remapping
# ====================================================================

reh_t2 <- remify(hT, model = "tie", event_type = "type",
                 aggregate_time = 2L, extend_riskset_by_type = TRUE)

u_T     <- sort(unique(hT$time))
kept_T2 <- u_T[seq(2L, length(u_T), by = 2L)]

expect_true(all(reh_t2$edgelist$time %in% kept_T2),
  info = "thin=2 typed: all output times are kept time points")
expect_equal(reh_t2$edgelist$type, hT$type,
  info = "thin=2 typed: type column unchanged by thinning")
expect_false(is.null(reh_t2$ids$type),
  info = "thin=2 typed: ids$type populated")


# ====================================================================
# 7. Error handling: invalid thin values
# ====================================================================

expect_error(
  remify(h, model = "tie", aggregate_time = 0L),
  pattern = "thin.*>=.*1|>=.*1.*thin",
  info = "thin=0: error"
)

expect_error(
  remify(h, model = "tie", aggregate_time = -1L),
  pattern = "thin.*>=.*1|>=.*1.*thin",
  info = "thin=-1: error"
)

expect_error(
  remify(h, model = "tie", aggregate_time = NA_integer_),
  pattern = "thin.*>=.*1|>=.*1.*thin",
  info = "thin=NA: error"
)
