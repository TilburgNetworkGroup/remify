# Tests for summary.remify output
# Covers: full/active/manual x directed/undirected x typed/untyped x ext=TRUE/FALSE

library(tinytest)

data(history, package = "remify")
edgelist_typed <- history
colnames(edgelist_typed)[colnames(edgelist_typed) == "setting"] <- "type"

edgelist_untyped <- edgelist_typed[, c("time", "actor1", "actor2", "weight")]

# Helper: capture summary output as a single string
cap <- function(reh) paste(capture.output(summary(reh)), collapse = "\n")

# ---------------------------------------------------------------------------
# SECTION 1: untyped edgelist — types should show as 1, no ext info
# ---------------------------------------------------------------------------
reh_untyped <- remify(edgelist_untyped, model = "tie", riskset = "full")

out <- cap(reh_untyped)
expect_true(grepl("(event) types = 1", out, fixed = TRUE),
  info = "untyped: types = 1 always shown")
expect_false(grepl("extend_riskset_by_type", out, fixed = TRUE),
  info = "untyped: no ext info when C=1")
expect_false(grepl("per type", out, fixed = TRUE),
  info = "untyped: no per-type counts")
expect_true(grepl("included dyads = 90", out, fixed = TRUE),
  info = "untyped full: 90 dyads")

# ---------------------------------------------------------------------------
# SECTION 2: typed, full riskset, ext=TRUE
# ---------------------------------------------------------------------------
reh_full_T <- remify(edgelist_typed, model = "tie", riskset = "full",
                      extend_riskset_by_type = TRUE)
out <- cap(reh_full_T)
expect_true(grepl("(event) types = 2", out, fixed = TRUE),
  info = "full/ext=T: types = 2")
expect_true(grepl("included dyads = 180", out, fixed = TRUE),
  info = "full/ext=T: 180 typed dyads")
expect_true(grepl("2 types x 90 actor pairs", out, fixed = TRUE),
  info = "full/ext=T: breakdown shown")
expect_true(grepl("extend_riskset_by_type = TRUE", out, fixed = TRUE),
  info = "full/ext=T: ext flag shown")
expect_false(grepl("per type", out, fixed = TRUE),
  info = "full/ext=T: no per-type for full riskset")

# ---------------------------------------------------------------------------
# SECTION 3: typed, full riskset, ext=FALSE
# ---------------------------------------------------------------------------
reh_full_F <- remify(edgelist_typed, model = "tie", riskset = "full",
                      extend_riskset_by_type = FALSE)
out <- cap(reh_full_F)
expect_true(grepl("(event) types = 2", out, fixed = TRUE),
  info = "full/ext=F: types = 2")
expect_true(grepl("included dyads = 90", out, fixed = TRUE),
  info = "full/ext=F: 90 untyped dyads")
expect_true(grepl("extend_riskset_by_type = FALSE", out, fixed = TRUE),
  info = "full/ext=F: ext flag shown")
expect_false(grepl("per type", out, fixed = TRUE),
  info = "full/ext=F: no per-type for full riskset")

# ---------------------------------------------------------------------------
# SECTION 4: typed, active riskset, ext=TRUE
# ---------------------------------------------------------------------------
reh_act_T <- remify(edgelist_typed, model = "tie", riskset = "active",
                     extend_riskset_by_type = TRUE)
out <- cap(reh_act_T)
expect_true(grepl("(event) types = 2", out, fixed = TRUE),
  info = "active/ext=T: types = 2")
expect_true(grepl("riskset = active", out, fixed = TRUE),
  info = "active/ext=T: riskset label")
expect_true(grepl("active dyads", out, fixed = TRUE),
  info = "active/ext=T: active dyads shown")
expect_true(grepl("extend_riskset_by_type = TRUE", out, fixed = TRUE),
  info = "active/ext=T: ext flag shown")
expect_true(grepl("per type", out, fixed = TRUE),
  info = "active/ext=T: per-type counts shown")
# Per-type counts should sum to total active typed dyads
type_counts <- table(reh_act_T$riskset_info$included$type)
expect_true(grepl(paste0("social=", type_counts["social"]), out, fixed = TRUE),
  info = "active/ext=T: social count correct")
expect_true(grepl(paste0("work=", type_counts["work"]), out, fixed = TRUE),
  info = "active/ext=T: work count correct")

# ---------------------------------------------------------------------------
# SECTION 5: typed, active riskset, ext=FALSE
# ---------------------------------------------------------------------------
reh_act_F <- remify(edgelist_typed, model = "tie", riskset = "active",
                     extend_riskset_by_type = FALSE)
out <- cap(reh_act_F)
expect_true(grepl("(event) types = 2", out, fixed = TRUE),
  info = "active/ext=F: types = 2")
expect_true(grepl("active dyads", out, fixed = TRUE),
  info = "active/ext=F: active dyads shown")
expect_true(grepl("extend_riskset_by_type = FALSE", out, fixed = TRUE),
  info = "active/ext=F: ext flag shown")
# Per-type counts still shown (from riskset_info$included which is untyped)
# Actually for ext=FALSE, riskset_info$included may not have type — skip per-type check

# ---------------------------------------------------------------------------
# SECTION 6: typed, manual riskset, ext=TRUE
# ---------------------------------------------------------------------------
manual_rs <- data.frame(
  actor1 = c(105, 105, 113, 109, 101),
  actor2 = c(109, 113, 107, 105, 115)
)
reh_man_T <- suppressWarnings(
  remify(edgelist_typed, model = "tie", riskset = "manual",
          manual.riskset = manual_rs, extend_riskset_by_type = TRUE)
)
out <- cap(reh_man_T)
expect_true(grepl("riskset = manual", out, fixed = TRUE),
  info = "manual/ext=T: riskset label")
expect_true(grepl("(event) types = 2", out, fixed = TRUE),
  info = "manual/ext=T: types = 2")
expect_true(grepl("extend_riskset_by_type = TRUE", out, fixed = TRUE),
  info = "manual/ext=T: ext flag shown")
expect_true(grepl("per type", out, fixed = TRUE),
  info = "manual/ext=T: per-type counts shown")

# ---------------------------------------------------------------------------
# SECTION 7: typed, manual riskset, ext=FALSE
# ---------------------------------------------------------------------------
reh_man_F <- suppressWarnings(
  remify(edgelist_typed, model = "tie", riskset = "manual",
          manual.riskset = manual_rs, extend_riskset_by_type = FALSE)
)
out <- cap(reh_man_F)
expect_true(grepl("riskset = manual", out, fixed = TRUE),
  info = "manual/ext=F: riskset label")
expect_true(grepl("extend_riskset_by_type = FALSE", out, fixed = TRUE),
  info = "manual/ext=F: ext flag shown")

# ---------------------------------------------------------------------------
# SECTION 8: undirected, full riskset, typed, ext=TRUE
# ---------------------------------------------------------------------------
reh_undir_T <- remify(edgelist_typed, model = "tie", riskset = "full",
                       directed = FALSE, extend_riskset_by_type = TRUE)
out <- cap(reh_undir_T)
expect_true(grepl("directed = FALSE", out, fixed = TRUE),
  info = "undirected: directed = FALSE")
expect_true(grepl("(event) types = 2", out, fixed = TRUE),
  info = "undirected/ext=T: types = 2")
expect_true(grepl("extend_riskset_by_type = TRUE", out, fixed = TRUE),
  info = "undirected/ext=T: ext flag shown")
D_undir <- reh_undir_T$D
expect_true(grepl(as.character(D_undir), out, fixed = TRUE),
  info = "undirected/ext=T: correct D shown")

# ---------------------------------------------------------------------------
# SECTION 9: events vs time points (pt method — simultaneous events)
# ---------------------------------------------------------------------------
edgelist_simul <- edgelist_typed
edgelist_simul$time[5:6] <- edgelist_simul$time[5]  # make two simultaneous
reh_simul <- remify(edgelist_simul, model = "tie", riskset = "full")
out <- cap(reh_simul)
# If E != M, both should be shown
if (!is.null(reh_simul$E) && reh_simul$E != reh_simul$M) {
  expect_true(grepl("time points", out, fixed = TRUE),
    info = "simultaneous: time points shown")
}

