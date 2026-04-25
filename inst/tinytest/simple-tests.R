# inst/tinytest/test-remify2-types-riskset.R

if (requireNamespace("remstats", quietly = TRUE)) {
  data("history", package = "remstats")
} else {
  stop("Package 'remstats' required for tests.")
}

N_ACTORS <- length(sort(unique(c(history$actor1, history$actor2))))
D_DYAD   <- N_ACTORS * (N_ACTORS - 1L)
C_TYPES  <- length(sort(unique(as.character(history$setting))))
TYPES    <- sort(unique(as.character(history$setting)))

key3 <- function(a1, a2, ty) paste(a1, a2, ty, sep = "||")
key2 <- function(a1, a2)     paste(a1, a2, sep = "||")

# 1) FULL untyped: exact dyad universe size and no type col
reh <- remify::remify(history, model="tie", riskset="full", event_type=NULL)
expect_false(isTRUE(reh$meta$with_type))
expect_false("type" %in% names(reh$index$dyad_map))
expect_equal(nrow(reh$index$dyad_map), D_DYAD)

# 2) FULL typed: dyad universe replicated over all observed types (exact coverage)
reh <- remify::remify(history, model="tie", riskset="full", event_type="setting")
dm <- reh$index$dyad_map
expect_true(isTRUE(reh$meta$with_type))
expect_equal(nrow(dm), D_DYAD * C_TYPES)
expect_equal(sort(unique(dm$type)), TYPES)

# For each type, the set of (actor1,actor2) pairs must be identical and of size D_DYAD
for (ty in TYPES) {
  sub <- dm[dm$type == ty, ]
  expect_equal(nrow(sub), D_DYAD)
  expect_equal(length(unique(key2(sub$actor1, sub$actor2))), D_DYAD)
}

# 3) ACTIVE typed: exact equality to observed dyad×type combos
reh <- remify::remify(history, model="tie", riskset="active", event_type="setting")
dm <- reh$index$dyad_map
obs <- unique(history[, c("actor1","actor2","setting")])
expect_equal(
  sort(key3(dm$actor1, dm$actor2, dm$type)),
  sort(key3(obs$actor1, obs$actor2, obs$setting))
)

# 4) MANUAL dyads-only, typed: must equal Cartesian product (manual dyads) × (types)
mr <- unique(history[, c("actor1","actor2")])
reh <- remify::remify(history, model="tie", riskset="manual", manual.riskset=mr, event_type="setting")
dm <- reh$index$dyad_map
expect_true(isTRUE(reh$meta$with_type))
expect_false(anyNA(dm))

expected_keys <- as.vector(outer(
  key2(mr$actor1, mr$actor2),
  TYPES,
  paste, sep="||"
))
actual_keys <- key3(dm$actor1, dm$actor2, dm$type)

expect_equal(sort(actual_keys), sort(expected_keys))

# 5) MANUAL dyads-only, untyped: must equal exactly the manual dyads
reh <- remify::remify(history, model="tie", riskset="manual", manual.riskset=mr, event_type=NULL)
dm <- reh$index$dyad_map
expect_false(isTRUE(reh$meta$with_type))
expect_equal(sort(key2(dm$actor1, dm$actor2)), sort(key2(mr$actor1, mr$actor2)))
