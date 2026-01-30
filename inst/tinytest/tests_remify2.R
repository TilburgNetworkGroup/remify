library(tinytest)

# ---- helpers ----------------------------------------------------

make_edgelist <- function(typed = FALSE, directed = TRUE, simultaneous = FALSE, weighted = TRUE) {
  set.seed(1)

  actors <- sprintf("%03d", c(101,103,104,105,107,109,111,112,113,115))
  n <- length(actors)

  M <- 33
  time <- cumsum(sample(10:200, M, replace = TRUE))
  if (simultaneous) {
    time[7:8] <- time[9]
    time[4] <- time[5]
  }

  # draw dyads (avoid self-loops)
  a1 <- sample(actors, M, replace = TRUE)
  a2 <- sample(actors, M, replace = TRUE)
  while (any(a1 == a2)) {
    idx <- which(a1 == a2)
    a2[idx] <- sample(actors, length(idx), replace = TRUE)
  }

  if (!directed) {
    lo <- pmin(a1, a2)
    hi <- pmax(a1, a2)
    a1 <- lo
    a2 <- hi
  }

  df <- data.frame(time = time, actor1 = a1, actor2 = a2, stringsAsFactors = FALSE)

  if (typed) {
    df$type <- sample(c("social","work"), M, replace = TRUE)
  }
  if (weighted) {
    df$weight <- runif(M, 0.5, 3.0)
  }

  df
}

get_dyad_vec <- function(reh) {
  dv <- attr(reh, "dyadID_vec")
  if (!is.null(dv)) return(as.integer(dv))
  as.integer(unlist(attr(reh, "dyadID")))
}

get_a1_vec <- function(reh) {
  v <- attr(reh, "actor1ID_vec")
  if (!is.null(v)) return(as.integer(v))
  as.integer(unlist(attr(reh, "actor1ID")))
}
get_a2_vec <- function(reh) {
  v <- attr(reh, "actor2ID_vec")
  if (!is.null(v)) return(as.integer(v))
  as.integer(unlist(attr(reh, "actor2ID")))
}
get_t_vec <- function(reh) {
  v <- attr(reh, "typeID_vec")
  if (!is.null(v)) return(as.integer(v))
  tv <- attr(reh, "typeID")
  if (is.null(tv)) return(NULL)
  as.integer(unlist(tv))
}

# ---- 1) basic structure ----------------------------------------

h <- make_edgelist(typed = FALSE, directed = TRUE, simultaneous = FALSE)

reh <- remify::remify2(
  edgelist = h,
  model = "tie",
  riskset = "full",
  attach_riskset = TRUE,
  riskset_decode = "ids"
)

expect_true(inherits(reh, "remify"))
expect_true(is.data.frame(reh$edgelist))
expect_true(is.numeric(reh$intereventTime))

expect_true(isTRUE(attr(reh, "directed")))
expect_true(!isTRUE(attr(reh, "with_type")))
expect_true(isTRUE(attr(reh, "weighted")))
expect_equal(attr(reh, "riskset"), "full")
expect_equal(attr(reh, "model"), "tie")

dict <- attr(reh, "dictionary")
expect_true(is.data.frame(dict$actors))
expect_true(is.null(dict$types))

# ---- 2) dyad indexing consistency --------------------------------
# Check dyadID is within 1..D (full)
dyad <- get_dyad_vec(reh)
expect_true(all(dyad >= 1L))
expect_true(all(dyad <= reh$D))

# Check no self-loop in actor IDs (event-level)
a1 <- get_a1_vec(reh); a2 <- get_a2_vec(reh)
expect_true(all(a1 != a2))

# ---- 3) active riskset contract ---------------------------------
rehA <- remify::remify2(
  edgelist = h,
  model = "tie",
  riskset = "active",
  attach_riskset = TRUE,
  riskset_decode = "labels"
)

rsA <- rehA$riskset_info
dyadA <- get_dyad_vec(rehA)

expect_equal(rsA$mode, "active")
expect_equal(rsA$riskset_idx, sort(unique(dyadA)))
expect_equal(rsA$riskset_idx[rsA$dyadIDactive], dyadA)

# ---- 4) manual riskset adds observed dyads -----------------------
manual <- h[1:10, c("actor1","actor2")]

capture_warnings <- function(expr) {
  warns <- character()
  val <- withCallingHandlers(
    expr,
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = val, warnings = warns)
}

res <- capture_warnings(
  remify::remify2(
    edgelist = h,
    model = "tie",
    riskset = "manual",
    manual.riskset = manual,
    attach_riskset = TRUE,
    riskset_decode = "ids"
  )
)

expect_true(any(grepl("observed dyad", res$warnings)))

rehM <- res$value
rsM <- rehM$riskset_info
dyadM <- get_dyad_vec(rehM)

expect_true(all(dyadM %in% rsM$riskset_idx))
expect_equal(rsM$riskset_idx[rsM$dyadIDactive], dyadM)
expect_true(any(grepl("observed dyad", res$warnings)))
expect_true(any(grepl("observed dyad", res$warnings, fixed = TRUE)))

# ---- 5) typed contract ------------------------------------------
hT <- make_edgelist(typed = TRUE, directed = TRUE, simultaneous = FALSE)

rehT <- remify::remify2(
  edgelist = hT,
  model = "tie",
  riskset = "active",
  attach_riskset = TRUE,
  riskset_decode = "labels"
)

dictT <- attr(rehT, "dictionary")
expect_true(is.data.frame(dictT$types))
expect_true(all(c("typeName","typeID") %in% names(dictT$types)))

rsT <- rehT$riskset_info
expect_true(rsT$with_type)
expect_true("type" %in% names(rsT$included))

# mapping still holds (now dyadIDs include type blocks)
dyadT <- get_dyad_vec(rehT)
expect_equal(rsT$riskset_idx[rsT$dyadIDactive], dyadT)

# ---- 6) simultaneous events -------------------------------------
hS <- make_edgelist(typed = TRUE, directed = TRUE, simultaneous = TRUE)

rehS <- remify::remify2(
  edgelist = hS,
  model = "tie",
  riskset = "active",
  attach_riskset = TRUE,
  riskset_decode = "ids"
)

expect_true(!is.null(attr(rehS, "indices_simultaneous_events")))
expect_true(!is.null(attr(rehS, "dyadID_vec")))

rsS <- rehS$riskset_info
dyadS <- get_dyad_vec(rehS)
expect_equal(rsS$riskset_idx[rsS$dyadIDactive], dyadS)

# ---- 7) decode fallback labels->ids ------------------------------
expect_warning(
  rehF <- remify::remify2(
    edgelist = hT,
    model = "tie",
    riskset = "active",
    attach_riskset = TRUE,
    riskset_decode = "labels",
    riskset_max_decode = 5L
  ),
  pattern = "ID-only dyad table|threshold",
  info = "labels->ids fallback"
)

expect_equal(rehF$riskset_info$decode, "ids")
expect_true(all(c("dyadID","actor1ID","actor2ID","typeID") %in% names(rehF$riskset_info$included)))

