# Tests for remify() actor-oriented model
# Covers: full/active/manual riskset, output structure,
#         sender_riskset, receiver_riskset, index$sender_map

library(tinytest)
library(remify)

data(randomREH, package = "remify")

el20  <- randomREH$edgelist[1:20, ]
el40  <- randomREH$edgelist[1:40, ]
actors <- randomREH$actors
origin <- randomREH$origin + 20

# ---------------------------------------------------------------------------
# SECTION 1: basic checks — directed=FALSE and model="actor" must error
# ---------------------------------------------------------------------------
expect_error(
  remify(edgelist = el20, actors = actors, directed = FALSE,
          model = "actor"),
  info = "actor model requires directed=TRUE"
)

expect_error(
  remify(edgelist = el20, actors = actors, directed = TRUE, extend_riskset_by_type = TRUE,
          model = "actor"),
  info = "actor model requires extend_riskset_by_type=TRUE"
)

# ---------------------------------------------------------------------------
# SECTION 2: full riskset
# ---------------------------------------------------------------------------
reh_full <- remify(edgelist = el20, actors = actors,
                    directed = TRUE, ordinal = FALSE,
                    origin = origin, model = "actor",
                    riskset = "full")

# class
expect_inherits(reh_full, "remify", info = "full: class remify")

# meta
expect_equal(reh_full$meta$model,   "actor", info = "full: model=actor")
expect_equal(reh_full$meta$riskset, "full",  info = "full: riskset=full")

# dimensions
expect_equal(reh_full$M, 20L, info = "full: M=20")
expect_equal(reh_full$N, 20L, info = "full: N=20")

# sender riskset = all 20 actors
expect_equal(length(reh_full$sender_riskset), 20L,
             info = "full: sender_riskset length=N")
expect_equal(sort(reh_full$sender_riskset), 1:20,
             info = "full: sender_riskset = 1..N")

# receiver riskset: list of N elements, each N-1 receivers
expect_equal(length(reh_full$receiver_riskset), 20L,
             info = "full: receiver_riskset length=N")
expect_true(all(lengths(reh_full$receiver_riskset) == 19L),
            info = "full: each sender has N-1=19 receivers")

# no sender sends to themselves
for (i in seq_along(reh_full$sender_riskset)) {
  s <- reh_full$sender_riskset[i]
  expect_false(s %in% reh_full$receiver_riskset[[i]],
               info = paste("full: sender", s, "not in own receiver list"))
}

# activeN = N for full riskset
expect_equal(reh_full$activeN, 20L, info = "full: activeN=N")

# index$sender_map
expect_true(!is.null(reh_full$index$sender_map),
            info = "full: sender_map present")
expect_equal(nrow(reh_full$index$sender_map), 20L,
             info = "full: sender_map has N rows")
expect_true(all(c("senderID","actorName") %in%
                  names(reh_full$index$sender_map)),
            info = "full: sender_map has correct columns")

# omit_dyad is empty list
expect_equal(length(reh_full$omit_dyad), 0L,
             info = "full: omit_dyad is empty")

# ids$dyad is NULL for actor model
expect_true(is.null(reh_full$ids$dyad),
            info = "full: ids$dyad NULL for actor model")

# ---------------------------------------------------------------------------
# SECTION 3: active riskset
# ---------------------------------------------------------------------------
reh_act <- remify(edgelist = el20, actors = actors,
                   directed = TRUE, ordinal = FALSE,
                   origin = origin, model = "actor",
                   riskset = "active")

expect_inherits(reh_act, "remify", info = "active: class remify")
expect_equal(reh_act$meta$model, "actor", info = "active: model=actor")

# sender riskset = observed senders only
n_unique_senders <- length(unique(el20$actor1))
expect_equal(reh_act$activeN, n_unique_senders,
             info = "active: activeN = number of unique senders")
expect_equal(length(reh_act$sender_riskset), n_unique_senders,
             info = "active: sender_riskset length = unique senders")

# all sender_riskset entries are valid actor IDs
expect_true(all(reh_act$sender_riskset >= 1L &
                  reh_act$sender_riskset <= reh_act$N),
            info = "active: sender IDs in valid range")

# receiver_riskset is a named list with one entry per active sender
expect_equal(length(reh_act$receiver_riskset), n_unique_senders,
             info = "active: receiver_riskset length = active senders")

# each sender's receivers are a subset of observed receivers for that sender
# use dictionary actor names (sorted), not randomREH$actors (unsorted)
dict_names <- reh_act$meta$dictionary$actors$actorName
for (i in seq_along(reh_act$sender_riskset)) {
  s_id   <- reh_act$sender_riskset[i]
  s_name <- dict_names[s_id]
  obs_recv <- unique(el20$actor2[el20$actor1 == s_name])
  obs_recv_ids <- match(obs_recv, dict_names)
  recv_in_rs <- reh_act$receiver_riskset[[i]]
  expect_true(all(recv_in_rs %in% obs_recv_ids),
              info = paste("active: receivers for sender", s_name,
                           "are observed receivers"))
}

# index$sender_map has activeN rows
expect_equal(nrow(reh_act$index$sender_map), n_unique_senders,
             info = "active: sender_map rows = activeN")

# omit_dyad is empty
expect_equal(length(reh_act$omit_dyad), 0L,
             info = "active: omit_dyad empty")

# activeN < N (not all actors sent in 20 events)
expect_true(reh_act$activeN < reh_act$N,
            info = "active: activeN < N for short sequence")

# ---------------------------------------------------------------------------
# SECTION 4: manual riskset
# ---------------------------------------------------------------------------
reh_man <- remify(edgelist = el20, actors = actors,
                   directed = TRUE, ordinal = TRUE,
                   origin = origin, model = "actor",
                   riskset = "manual",
                   manual.riskset = el40[, -1])  # actor1, actor2, type

expect_inherits(reh_man, "remify", info = "manual: class remify")
expect_equal(reh_man$meta$model,          "actor",  info = "manual: model=actor")
expect_equal(reh_man$meta$riskset_source, "manual", info = "manual: riskset_source=manual")

# sender_riskset derived from manual.riskset actor1 column
man_senders <- unique(el40$actor1)
expect_equal(reh_man$activeN, length(man_senders),
             info = "manual: activeN = unique senders in manual.riskset")

# receiver_riskset respects manual constraints
for (i in seq_along(reh_man$sender_riskset)) {
  s_id   <- reh_man$sender_riskset[i]
  s_name <- reh_man$meta$dictionary$actors$actorName[s_id]
  allowed_recv_names <- unique(el40$actor2[el40$actor1 == s_name])
  allowed_recv_ids   <- match(allowed_recv_names,
                               reh_man$meta$dictionary$actors$actorName)
  recv_in_rs <- reh_man$receiver_riskset[[i]]
  expect_true(all(recv_in_rs %in% allowed_recv_ids),
              info = paste("manual: receivers for", s_name,
                           "match manual.riskset"))
}

# index$sender_map present
expect_true(!is.null(reh_man$index$sender_map),
            info = "manual: sender_map present")

# omit_dyad empty
expect_equal(length(reh_man$omit_dyad), 0L,
             info = "manual: omit_dyad empty")

# ---------------------------------------------------------------------------
# SECTION 5: ordinal=TRUE works
# ---------------------------------------------------------------------------
reh_ord <- remify(edgelist = el20, actors = actors,
                   directed = TRUE, ordinal = TRUE,
                   origin = origin, model = "actor",
                   riskset = "full")

expect_true(is.null(reh_ord$intereventTime),
            info = "ordinal: intereventTime NULL")
expect_equal(reh_ord$meta$ordinal, TRUE, info = "ordinal: meta$ordinal=TRUE")
expect_equal(length(reh_ord$sender_riskset), 20L,
             info = "ordinal: sender_riskset intact")

# ---------------------------------------------------------------------------
# SECTION 6: types present
# ---------------------------------------------------------------------------
# With typed events, receiver_riskset should still work correctly
reh_typed <- remify(edgelist = el20, actors = actors,
                     directed = TRUE, ordinal = FALSE,
                     origin = origin, model = "actor",
                     riskset = "full")

expect_equal(reh_typed$C, 3L, info = "typed: C=3 event types")
# receiver_riskset is type-agnostic (actor level, not dyad×type)
expect_equal(length(reh_typed$receiver_riskset), 20L,
             info = "typed: receiver_riskset still N entries")

# ---------------------------------------------------------------------------
# SECTION 6b: extend_riskset_by_type
# ---------------------------------------------------------------------------
reh_ext_false <- remify(edgelist = el20, actors = actors,
                         directed = TRUE, ordinal = TRUE,
                         origin = origin, model = "actor",
                         riskset = "manual",
                         manual.riskset = el40[, -1],
                         extend_riskset_by_type = FALSE)

expect_false(reh_ext_false$meta$with_type_riskset,
             info = "extend=FALSE: with_type_riskset=FALSE")
expect_equal(reh_ext_false$meta$C_riskset, 1L,
             info = "extend=FALSE: C_riskset=1")

# ---------------------------------------------------------------------------
# SECTION 6c: active_saturated riskset
# ---------------------------------------------------------------------------
reh_sat <- remify(edgelist = el20, actors = actors,
                   directed = TRUE, ordinal = FALSE,
                   origin = origin, model = "actor",
                   riskset = "active_saturated")

expect_inherits(reh_sat, "remify", info = "saturated: class remify")
expect_equal(reh_sat$meta$model,          "actor",  info = "saturated: model=actor")
expect_equal(reh_sat$meta$riskset_source, "manual", info = "saturated: stored as manual")

# Every observed sender should also appear as a receiver and vice versa
obs_senders   <- unique(el20$actor1)
obs_receivers <- unique(el20$actor2)
dict_names    <- reh_sat$meta$dictionary$actors$actorName

# For each observed A->B pair, B should be in A's receiver riskset
for (i in seq_len(nrow(el20))) {
  s_name <- el20$actor1[i]
  r_name <- el20$actor2[i]
  s_id   <- match(s_name, dict_names)
  r_id   <- match(r_name, dict_names)
  recv   <- reh_sat$receiver_riskset[[s_name]]
  expect_true(r_id %in% recv,
              info = paste("saturated: observed receiver", r_name,
                           "in riskset of", s_name))
}

# Saturation: for each observed A->B, A should also be in B's receiver riskset
# (i.e., B->A is at risk)
for (i in seq_len(nrow(el20))) {
  s_name <- el20$actor1[i]
  r_name <- el20$actor2[i]
  s_id   <- match(s_name, dict_names)
  if (r_name %in% names(reh_sat$receiver_riskset)) {
    recv_of_r <- reh_sat$receiver_riskset[[r_name]]
    expect_true(s_id %in% recv_of_r,
                info = paste("saturated: reverse", s_name,
                             "in riskset of", r_name))
  }
}

# active_saturated should have >= active riskset size
reh_act2 <- remify(edgelist = el20, actors = actors,
                    directed = TRUE, ordinal = FALSE,
                    origin = origin, model = "actor",
                    riskset = "active")
expect_true(reh_sat$activeN >= reh_act2$activeN,
            info = "saturated: activeN >= active (reverse senders added)")

# All types at risk — with_type_riskset should be TRUE (no type filtering)
# Since manual.riskset has no type column, ext=TRUE applies all types
# (depends on extend_riskset_by_type default)
expect_equal(reh_sat$meta$riskset_source, "manual",
             info = "saturated: internally uses manual riskset")

# ---------------------------------------------------------------------------
# SECTION 7: print/summary works without error
# ---------------------------------------------------------------------------
expect_silent(capture.output(print(reh_full)),
              info = "print: full riskset no error")
expect_silent(capture.output(print(reh_act)),
              info = "print: active riskset no error")
expect_silent(capture.output(print(reh_man)),
              info = "print: manual riskset no error")

