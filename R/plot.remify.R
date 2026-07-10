# =============================================================================
# plot.remify  —  thin dispatcher + one internal function per plot
#
# Structure:
#   plot.remify()      resolves object structure, validates args, subsets actors
#                      (the 50-actor cap) ONCE, then dispatches on `which`.
#   .plot_*()          one isolated function per plot. Each owns and restores
#                      its own par()/layout() state, so plots compose instead
#                      of stomping each other's device settings.
#
#
#
#
#
# =============================================================================


# ---- shared helpers ---------------------------------------------------------

# per-event direction-less dyad strings ("min_max"), via the parallel path
.no_dir_dyads <- function(x, ncores) {
  cl <- parallel::makeCluster(ncores)
  out <- parallel::parApply(cl = cl, X = x$edgelist, MARGIN = 1, FUN = function(l) {
    dyad_l <- sort(as.character(c(l[2], l[3])))
    paste(dyad_l[1], dyad_l[2], sep = "\r")
  })
  parallel::stopCluster(cl)
  out
}

# (incomplete) dyad-frequency matrices X (directed) and X_dyad (direction-less),
# replicating the original which[c(2,5)] construction
.dyad_matrices <- function(x, directed, ncores) {
  dyads <- paste(x$edgelist$actor1, x$edgelist$actor2, sep = "\r")
  ndd   <- .no_dir_dyads(x, ncores)

  dyad_freq <- table(dyads)
  X <- NULL
  if (directed) {
    X <- matrix(NA, nrow = length(dyad_freq), ncol = 3)
    X[, 3] <- as.integer(dyad_freq)
    for (d in 1:length(dyad_freq)) {
      X[d, 1:2] <- unlist(strsplit(x = names(dyad_freq[d]), split = "\r", fixed = TRUE))
    }
    X <- X[order(X[, 1], X[, 2]), ]
  }

  ndd_freq <- table(ndd)
  X_dyad <- matrix(NA, nrow = length(ndd_freq), ncol = 3)
  X_dyad[, 3] <- as.integer(ndd_freq)
  for (d in 1:length(ndd_freq)) {
    X_dyad[d, 1:2] <- unlist(strsplit(x = names(ndd_freq[d]), split = "\r", fixed = TRUE))
  }
  X_dyad <- X_dyad[order(X_dyad[, 1], X_dyad[, 2]), ]

  if (!directed) X <- X_dyad
  list(X = X, X_dyad = X_dyad)
}


# ---- [plot 1] inter-event time histogram ------------------------------------
.plot_interevent <- function(x, breaks) {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))

  iet <- x$intereventTime
  time_unit <- attr(iet, "unit")
  xlab <- if (!is.null(time_unit)) {
    paste0("waiting time (", time_unit, ")")
  } else {
    "waiting time"
  }

  hist(iet, breaks = breaks, col = "lavender", border = "darkgray", freq = FALSE,
       xlab = xlab, ylab = "density",
       main = "Distribution of the inter-event times")

  rate <- 1 / mean(iet)
  xs <- seq(0, max(iet), length.out = 200)
  lines(xs, dexp(xs, rate = rate), col = "#cd0303", lwd = 2, lty = 2)
  legend("topright", legend = "exponential (same mean)",
         col = "#cd0303", lty = 2, lwd = 2, bty = "n", cex = 0.9)
}


# ---- [plot 2] activity plot (tile + degree marginals) -----------------------
.plot_activity <- function(x, actors, directed, palette, rev, ncores) {
  op <- par(no.readonly = TRUE); on.exit({ layout(1); par(op) })
  el <- x$edgelist

  # degrees + activity ordering (most active first)
  deg_out <- as.integer(table(factor(el$actor1, levels = actors)))
  deg_in  <- as.integer(table(factor(el$actor2, levels = actors)))
  ord  <- order(deg_out + deg_in, decreasing = TRUE)
  acts <- actors[ord]
  N    <- length(acts)

  # directed count matrix M[sender, receiver] (symmetric if undirected)
  M <- as.matrix(table(factor(el$actor1, levels = acts),
                       factor(el$actor2, levels = acts)))
  if (!directed) { M <- M + t(M); diag(M) <- 0L }

  maxc <- max(M, 1L)
  pal  <- grDevices::hcl.colors(maxc, palette = palette, rev = rev)

  # layout: heatmap dominant, marginals flush, legend top-right
  #   2 4
  #   1 3
  layout(matrix(c(2, 4, 1, 3), 2, 2, byrow = TRUE), widths = c(4, 1), heights = c(1, 4))
  #par(oma = c(0, 0, 2, 0))
  par(oma = c(1.5, 0, 2, 0))
  # margin in lines: scale with label length but shrink hard as actors grow,
  # so layout()-shrunk panels never overflow the device
  lab <- min(max(nchar(acts)) * 0.45 + 1.2, max(2.5, 8 - N / 12))

  # [1] heatmap
  par(mar = c(lab, lab, 0.4, 0.4), mgp = c(2, 0.6, 0))
  plot.new(); plot.window(xlim = c(0.5, N + 0.5), ylim = c(N + 0.5, 0.5), xaxs = "i", yaxs = "i")
  for (s in seq_len(N)) for (r in seq_len(N)) {
    col <- if (acts[s] == acts[r]) "#eeeeee" else if (M[s, r] > 0) pal[M[s, r]] else "#ffffff"
    rect(r - 0.5, s - 0.5, r + 0.5, s + 0.5, col = col, border = "#ffffff", lwd = 0.5)
  }
  # riskset outline: red border on in-riskset dyads (omitted when full).
  # the informative case is at-risk-but-unobserved cells (white fill + red edge),
  # e.g. the saturated reverse direction under active_saturated/manual.
  rinfo <- x$riskset_info
  if (!is.null(rinfo) && !identical(rinfo$mode, "full")) {
    inc <- rinfo$included
    rr <- match(inc$actor1, acts)   # sender -> row
    cc <- match(inc$actor2, acts)   # receiver -> col
    ok <- !is.na(rr) & !is.na(cc)
    rect(cc[ok] - 0.5, rr[ok] - 0.5, cc[ok] + 0.5, rr[ok] + 0.5,
         col = NA, border = "#cd0303", lwd = 1.2)
  }
  axis(1, at = seq_len(N), labels = acts, las = 2, tick = FALSE, line = -0.5, cex.axis = 0.8)
  axis(2, at = seq_len(N), labels = acts, las = 2, tick = FALSE, line = -0.5, cex.axis = 0.8)
  mtext(if (directed) "receiver" else "actor", side = 1, line = lab - 1)
  mtext(if (directed) "sender"   else "actor", side = 2, line = lab - 1)

  # [2] top marginal: in-/total-degree, aligned to columns
  cd <- colSums(M)
  par(mar = c(0, lab, 1, 0.4), mgp = c(2, 0.6, 0))
  plot.new(); plot.window(xlim = c(0.5, N + 0.5), ylim = c(0, max(cd, 1)), xaxs = "i")
  rect(seq_len(N) - 0.35, 0, seq_len(N) + 0.35, cd, col = "#5AAFC8", border = NA)
  axis(2, las = 2, cex.axis = 0.7)
  mtext(if (directed) "in-degree" else "total-degree", side = 3, line = -0.3, cex = 0.85)

  # [3] right marginal: out-degree, aligned to rows (directed only)
  if (directed) {
    rd <- rowSums(M); yc <- seq_len(N)
    par(mar = c(lab, 0, 0.4, 1), mgp = c(2, 0.6, 0))
    plot.new(); plot.window(xlim = c(0, max(rd, 1)), ylim = c(N + 0.5, 0.5), yaxs = "i")
    rect(0, yc - 0.35, rd, yc + 0.35, col = "#5AAFC8", border = NA)
    axis(1, cex.axis = 0.7)
    mtext("out-degree", side = 4, line = -0.3, cex = 0.85)
  } else { plot.new() }   # keep region empty so heatmap stays aligned to top marginal

  # [4] colour legend
  par(mar = c(lab, 1, 1, 2))
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, maxc))
  rect(0, 0:(maxc - 1), 1, 1:maxc, col = pal, border = NA)
  rect(0, 0, 1, maxc, border = "#888888")
  ticks <- unique(round(c(1, maxc / 2, maxc)))
  axis(4, at = ticks - 0.5, labels = ticks, las = 2, cex.axis = 0.7, tick = FALSE)
  mtext("events", side = 3, line = 0.1, cex = 0.85)

  title(main = "Activity plot", outer = TRUE)

  if (!is.null(x$riskset_info) && !identical(x$riskset_info$mode, "full")) {
    par(xpd = NA)                      # allow drawing in the outer margin
    usr <- par("usr")                  # current panel's user coords
    text(x = grconvertX(0.99, "ndc", "user"),
         y = grconvertY(0.01, "ndc", "user"),
         labels = "red outline = dyad in riskset",
         adj = c(1, 0), cex = 0.75, col = "#cd0303")
  }
}


# ---- [plot 3] normalized degree per time interval ---------------------------
.plot_normdegree <- function(x, actors, directed, n_intervals, palette, rev) {
  op <- par(no.readonly = TRUE); on.exit({ layout(1); par(op) })
  el <- x$edgelist

  # activity ordering (most active at top), consistent with plot 2
  deg_tot <- as.integer(table(factor(c(el$actor1, el$actor2), levels = actors)))
  acts <- actors[order(deg_tot, decreasing = TRUE)]
  N <- length(acts)

  intv <- cut(el$time, breaks = n_intervals)
  K <- nlevels(intv)
  lab <- min(max(nchar(acts)) * 0.45 + 1.2, max(2.5, 8 - N / 12))
  pal <- grDevices::hcl.colors(64, palette = palette, rev = rev)

  br   <- seq(min(el$time), max(el$time), length.out = K + 1L)
  mids <- (br[-1L] + br[-length(br)]) / 2

  if (directed) {
    Mout <- as.matrix(table(factor(el$actor1, levels = acts), intv))
    Min  <- as.matrix(table(factor(el$actor2, levels = acts), intv))
    gmax <- max(Mout, Min, 1L)
  } else {
    Mtot <- as.matrix(table(factor(c(el$actor1, el$actor2), levels = acts),
                            rep(intv, 2L)))
    gmax <- max(Mtot, 1L)
  }

  draw_hm <- function(M, main_txt) {
    par(mar = c(4, lab, 2.5, 0.5), mgp = c(2, 0.6, 0))
    plot.new(); plot.window(xlim = c(0.5, K + 0.5), ylim = c(0.5, N + 0.5), xaxs = "i", yaxs = "i")
    for (a in seq_len(N)) for (k in seq_len(K)) {
      v <- M[a, k]
      col <- if (v > 0) pal[max(1L, round(v / gmax * length(pal)))] else "#ffffff"
      rect(k - 0.5, a - 0.5, k + 0.5, a + 0.5, col = col, border = "#ffffff", lwd = 0.5)
    }
    axis(1, at = seq_len(K), labels = round(mids), tick = FALSE, line = -0.5, cex.axis = 0.8)
    axis(2, at = seq_len(N), labels = acts, las = 2, tick = FALSE, line = -0.5, cex.axis = 0.8)
    mtext("time interval", side = 1, line = 2.2, cex = 0.85)
    title(main = main_txt, font.main = 1, cex.main = 1)
  }

  draw_bar <- function() {
    par(mar = c(4, 0.5, 2.5, 3))
    plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, gmax))
    edges <- seq(0, gmax, length.out = length(pal) + 1L)
    rect(0, edges[-length(edges)], 1, edges[-1L], col = pal, border = NA)
    rect(0, 0, 1, gmax, border = "#888888")
    ticks <- unique(round(c(0, gmax / 2, gmax)))
    axis(4, at = ticks, labels = ticks, las = 2, cex.axis = 0.7, tick = FALSE)
    mtext("events", side = 3, line = 0.2, cex = 0.8)
  }

  if (directed) {
    layout(matrix(c(1, 3, 2, 3), 2, 2, byrow = TRUE), widths = c(5, 1))
    draw_hm(Mout, "Out-degree per time interval")
    draw_hm(Min,  "In-degree per time interval")
    draw_bar()
  } else {
    layout(matrix(c(1, 2), 1, 2), widths = c(5, 1))
    draw_hm(Mtot, "Total-degree per time interval")
    draw_bar()
  }
}

# ---- [plot 4] quantities per time interval ----------------------------------
.plot_intervals <- function(x, directed, n_intervals, ncores) {
  op <- par(no.readonly = TRUE); on.exit({ layout(1); par(op) })
  el <- x$edgelist

  intv <- cut(el$time, breaks = n_intervals)
  K <- nlevels(intv)
  xi <- seq_len(K)

  n_events <- as.integer(tapply(el$actor1, intv, length)); n_events[is.na(n_events)] <- 0L

  dyads <- if (directed) paste(el$actor1, el$actor2, sep = "_") else .no_dir_dyads(x, ncores)
  p_dyad <- as.numeric(tapply(dyads, intv, function(l) length(unique(l)))); p_dyad[is.na(p_dyad)] <- 0
  p_dyad <- p_dyad / x$D

  # interval midpoints for an informative x-axis (real time, not 1..K)
  br <- seq(min(el$time), max(el$time), length.out = K + 1L)
  mids <- (br[-1L] + br[-length(br)]) / 2

  if (directed) layout(matrix(c(1, 2), 2, 1)) else layout(matrix(c(1, 2), 2, 1))
  par(mar = c(2.5, 4.5, 2.5, 1), mgp = c(2.4, 0.7, 0))

  # [top] event count per interval
  plot(xi, n_events, type = "h", lwd = 6, lend = 1, col = "#5AAFC8",
       xaxt = "n", xlab = "", ylab = "# events", ylim = c(0, max(n_events) * 1.05),
       main = "Event volume per time interval")
  axis(1, at = xi, labels = round(mids), cex.axis = 0.8)

  # [bottom] active fractions, overlaid
  par(mar = c(4, 4.5, 2.5, 1))
  plot(xi, p_dyad, type = "o", pch = 16, lwd = 1.8, col = "#5AAFC8",
       xaxt = "n", xlab = "time", ylab = "active fraction (observed/total)",
       ylim = c(0, 1), main = "Active fractions per time interval")
  axis(1, at = xi, labels = round(mids), cex.axis = 0.8)

  leg_lab <- "dyads"; leg_col <- "#5AAFC8"
  if (directed) {
    p_send <- as.numeric(tapply(el$actor1, intv, function(l) length(unique(l)))); p_send[is.na(p_send)] <- 0
    p_recv <- as.numeric(tapply(el$actor2, intv, function(l) length(unique(l)))); p_recv[is.na(p_recv)] <- 0
    p_send <- p_send / x$N; p_recv <- p_recv / x$N
    lines(xi, p_send, type = "o", pch = 17, lwd = 1.8, col = "#cd7f32")
    lines(xi, p_recv, type = "o", pch = 15, lwd = 1.8, col = "#7a5f9e")
    leg_lab <- c("dyads", "senders", "receivers"); leg_col <- c("#5AAFC8", "#cd7f32", "#7a5f9e")
  }
  legend("topright", legend = leg_lab, col = leg_col, pch = c(16, 17, 15)[seq_along(leg_lab)],
         lwd = 1.8, bty = "n", cex = 0.85, horiz = TRUE)
}


# ---- [plot 5] network visualization -----------------------------------------
.plot_network <- function(x, actors, directed, ncores, igraph.edge.color, igraph.vertex.color) {
  op <- par(no.readonly = TRUE); on.exit({ layout(1); par(op) })
  requireNamespace(package = "igraph", quietly = TRUE)
  mats <- .dyad_matrices(x, directed, ncores)

  # one network only: directed object -> directed network (in-degree colouring);
  # undirected object -> undirected network (total-degree colouring)
  if (directed) {
    M <- mats$X
    pop_table <- table(factor(x$edgelist$actor2, levels = actors))               # in-degree
    main_txt <- "Network of events (directed edges)"
  } else {
    M <- mats$X_dyad
    pop_table <- table(factor(c(x$edgelist$actor1, x$edgelist$actor2), levels = actors))  # total-degree
    main_txt <- "Network of events (undirected edges)"
  }
  M[is.na(M)] <- 0

  nodes <- data.frame(name = names(pop_table), popularity = as.vector(pop_table))
  links <- data.frame(from = as.character(M[, 1]), to = as.character(M[, 2]), weight = as.numeric(M[, 3]))
  net <- igraph::graph_from_data_frame(d = links, vertices = nodes, directed = directed)

  # intensity scaling for nodes (popularity) and edges (weight)
  pop_scale <- (nodes$popularity - min(nodes$popularity)) / (max(nodes$popularity) - min(nodes$popularity))
  pop_scale[is.na(pop_scale)] <- 0
  w_scale <- (links$weight - min(links$weight)) / (max(links$weight) - min(links$weight))
  w_scale[is.na(w_scale)] <- 0
  w_scale <- 0.25 + 0.75 * w_scale   # floor at ~25% opacity, ceiling at 100%

  alpha_hex <- function(s) sapply(round(255 * s), function(l) {
    hc <- rgb(0, 0, 0, alpha = ifelse(is.na(l), 0, l), maxColorValue = 255)
    substr(hc, nchar(hc) - 1, nchar(hc))
  })

  igraph::V(net)$size  <- 12
  igraph::V(net)$color <- paste0(igraph.vertex.color, alpha_hex(pop_scale))
  igraph::V(net)$frame.color <- "black"
  igraph::E(net)$color <- paste0(igraph.edge.color, alpha_hex(w_scale))
  igraph::E(net)$width <- 1 + 3 * w_scale   # ~1px for least active, ~4px for most active
  igraph::E(net)$curved <- 0.1
  if (directed) igraph::E(net)$arrow.size <- 0.4 else igraph::E(net)$arrow.mode <- 0

  # labels beside nodes, not on top of them
  igraph::V(net)$label.dist   <- 1.4
  igraph::V(net)$label.degree <- pi / 2     # below the node; use -pi/2 for above, 0 for right
  igraph::V(net)$label.cex    <- 0.85
  igraph::V(net)$label.color  <- "black"
  igraph::graph_attr(net, "layout") <- igraph::layout_with_lgl

  par(mfrow = c(1, 1), mar = c(1, 1, 2.5, 1))
  plot(net, main = main_txt)
}


# ---- [plot 6] counting process N(t) (NEW) -----------------------------------
# cumulative event count against real time, with a constant-rate reference line.
# un-binned: ignores n_intervals/breaks by design. ordinal data is skipped at
# dispatch (cumulative count vs event index is just y = x and uninformative).
.plot_counting <- function(x) {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mfrow = c(1, 1), mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))

  t <- x$edgelist$time
  n <- length(t)
  time_unit <- attr(x$intereventTime, "unit")
  xlab <- if (!is.null(time_unit)) paste0("time (", time_unit, ")") else "time"

  plot(x = c(0, t), y = 0:n, type = "s", col = "#5AAFC8", lwd = 2,
       xlab = xlab, ylab = "cumulative # events  N(t)", main = "Counting process N(t)")
  abline(a = 0, b = n / t[n], col = "#cd0303", lty = 2)  # constant-rate baseline
}

# ---- [plot 7] dyad event raster (NEW) ---------------------------------------
# one row per dyad, one tick per event. directed: A->B and B->A are kept as
# adjacent rows (grouped by underlying pair) so reciprocity reads as two
# neighbouring rows firing in sequence; inertia reads as horizontal clustering.
# un-binned (raw event times). capped at the top-k most active dyads.
.plot_raster <- function(x, directed, top_k = 30L) {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  el <- x$edgelist

  if (directed) {
    dlab <- paste(el$actor1, el$actor2, sep = " \u2192 ")
    gkey <- apply(el[, 2:3], 1, function(l) paste(sort(as.character(l)), collapse = "|"))
    dfreq <- table(dlab); gfreq <- table(gkey)
    ud <- unique(data.frame(dlab, gkey, stringsAsFactors = FALSE))
    ud$gf <- as.integer(gfreq[ud$gkey]); ud$df <- as.integer(dfreq[ud$dlab])
    # group freq desc, pair adjacency via gkey tiebreak, direction freq desc
    ud <- ud[order(-ud$gf, ud$gkey, -ud$df), ]
    row_order <- ud$dlab
  } else {
    dlab <- apply(el[, 2:3], 1, function(l) paste(sort(as.character(l)), collapse = " \u2014 "))
    row_order <- names(sort(table(dlab), decreasing = TRUE))
  }

  row_order <- head(row_order, top_k)
  keep <- dlab %in% row_order
  t_k <- el$time[keep]; lab_k <- dlab[keep]
  lev <- rev(row_order)               # most active ends up at the top
  yrow <- match(lab_k, lev)
  ncap <- length(lev)

  left_lines <- min(max(nchar(lev)) * 0.55 + 3, 12)
  par(mar = c(5, left_lines, 3, 2), mgp = c(3, 1, 0))
  plot(NA, xlim = range(t_k), ylim = c(0.5, ncap + 0.5),
       xlab = "time", ylab = "", yaxt = "n", main = "Dyad event raster")
  abline(h = seq_len(ncap), col = "gray90")
  axis(side = 2, at = seq_len(ncap), labels = lev, las = 2, cex.axis = 0.7, tick = FALSE)
  segments(x0 = t_k, y0 = yrow - 0.35, y1 = yrow + 0.35, col = "#5AAFC8", lwd = 1.1)

  if (length(table(dlab)) > top_k) {
    warning(sprintf("more than %d dyads; showing the %d most active.", top_k, top_k))
  }
}

# ---- [plot 8] dyad recurrence-time distribution (NEW) -----------------------
# per-dyad gaps between successive events, pooled across dyads. excess mass at
# short gaps (above the same-mean exponential) = temporal clustering -> memory
# effects (window/decay) are warranted; flat against exponential -> plain
# inertia suffices. directed uses directed dyads (inertia is directed repetition);
# undirected uses unordered pairs. ordinal: gap is the event-index distance.
.plot_recurrence <- function(x, directed, ordinal, breaks) {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  el <- x$edgelist

  if (directed) {
    key <- paste(el$actor1, el$actor2, sep = "\r")
  } else {
    key <- apply(el[, 2:3], 1, function(l) paste(sort(as.character(l)), collapse = "\r"))
  }

  # gaps within each dyad (sort within dyad in case of ties / reordering)
  gaps <- unlist(lapply(split(el$time, key), function(tt) diff(sort(tt))), use.names = FALSE)
  gaps <- gaps[is.finite(gaps)]

  if (length(gaps) == 0) {
    warning("no dyad recurs more than once; recurrence-time plot is empty.")
    plot.new(); title("Dyad recurrence times (none observed)")
    return(invisible(NULL))
  }

  par(mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))
  xlab <- if (ordinal) "recurrence time (event-index gap)" else "recurrence time"
  hist(gaps, breaks = breaks, col = "lavender", border = "darkgray", freq = FALSE,
       xlab = xlab, ylab = "density", main = "Distribution of dyad recurrence times")

  if (!ordinal) {
    rate <- 1 / mean(gaps)
    xs <- seq(0, max(gaps), length.out = 200)
    lines(xs, dexp(xs, rate = rate), col = "#cd0303", lwd = 2, lty = 2)
    legend("topright", legend = "exponential (same mean)",
           col = "#cd0303", lty = 2, lwd = 2, bty = "n", cex = 0.9)
  }
}


# ---- [plot 9] dyad concentration (Lorenz curve) (NEW) -----------------------
# cumulative share of events vs cumulative share of dyads (over all D possible
# dyads, so never-active dyads count as zeros). diagonal = events spread evenly;
# strong bow toward bottom-right = concentration on few dyads -> frailty is
# motivated and per-dyad effects are harder to identify. gini reported in-corner.
.plot_concentration <- function(x, directed) {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  el <- x$edgelist

  if (directed) {
    key <- paste(el$actor1, el$actor2, sep = "\r")
  } else {
    key <- apply(el[, 2:3], 1, function(l) paste(sort(as.character(l)), collapse = "\r"))
  }
  obs <- as.integer(table(key))

  # pad with never-active dyads (count 0) up to the full riskset size x$D
  counts <- c(obs, rep(0L, max(0L, x$D - length(obs))))
  counts <- sort(counts)                          # ascending for Lorenz
  n <- length(counts)

  cum <- cumsum(counts) / sum(counts)
  fx  <- (0:n) / n
  fy  <- c(0, cum)

  gini <- 1 - sum((fy[-1] + fy[-length(fy)]) * diff(fx))   # 1 - 2*area under curve

  par(mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))
  plot(fx, fy, type = "l", col = "#5AAFC8", lwd = 2,
       xlim = c(0, 1), ylim = c(0, 1), #asp = 1,
       xlab = "cumulative share of dyads",
       ylab = "cumulative share of events",
       main = "Event concentration wrt dyads (Lorenz curve)")
  abline(0, 1, col = "#cd0303", lty = 2)           # line of perfect equality
  polygon(c(fx, 1), c(fy, 0), col = "#5AAFC833", border = NA)
  legend("topleft", legend = sprintf("Gini = %.3f", gini), bty = "n", cex = 0.95)
}

# ---- [plot 10] reciprocity scatter (directed only) (NEW) --------------------
# for each unordered pair, count(i->j) vs count(j->i). diagonal = balanced
# exchange (reciprocity effect has little to explain); points near an axis =
# one-directional flow (reciprocity is informative). point area ~ total pair
# volume. directed histories only.
.plot_reciprocity <- function(x) {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  el <- x$edgelist

  dir_key <- paste(el$actor1, el$actor2, sep = "\r")
  dfreq <- table(dir_key)

  # unordered pair -> (ab, ba) counts
  pair <- apply(el[, 2:3], 1, function(l) paste(sort(as.character(l)), collapse = "\r"))
  upair <- unique(pair)
  ab <- ba <- integer(length(upair))
  for (i in seq_along(upair)) {
    a <- strsplit(upair[i], "\r", fixed = TRUE)[[1]]
    ab[i] <- as.integer(dfreq[paste(a[1], a[2], sep = "\r")]); if (is.na(ab[i])) ab[i] <- 0L
    ba[i] <- as.integer(dfreq[paste(a[2], a[1], sep = "\r")]); if (is.na(ba[i])) ba[i] <- 0L
  }
  # order within pair irrelevant; fold to (max, min) so each pair plots once
  hi <- pmax(ab, ba); lo <- pmin(ab, ba)
  tot <- hi + lo
  mx <- max(hi, 1)

  par(mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))
  plot(lo, hi, xlim = c(0, mx), ylim = c(0, mx), #asp = 1,
       xlab = "events in quieter direction",
       ylab = "events in more active direction",
       main = "Reciprocity scatter (per dyad)",
       pch = 21, bg = "#5AAFC899", col = "#5AAFC8",
       cex = 0.8 + 2 * sqrt(tot / max(tot)))
  abline(0, 1, col = "#cd0303", lty = 2)   # perfect balance
}

# ---- [plot 11] duration distribution (durem only) (NEW) ---------------------
# spell length = end - time. exponential same-mean overlay (interval only),
# matching plot 8's style. ordinal: duration is an event-index gap.
.plot_duration <- function(x, ordinal, breaks) {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))
  el <- x$edgelist
  dur <- el$end - el$time
  n_cens <- sum(is.na(dur))
  dur <- dur[is.finite(dur)]
  if (length(dur) == 0) { plot.new(); title("Spell durations (none observed)"); return(invisible(NULL)) }
  xlab <- if (ordinal) "duration (event-index gap)" else "duration"
  hist(dur, breaks = breaks, col = "lavender", border = "darkgray", freq = FALSE,
       xlab = xlab, ylab = "density", main = "Distribution of event durations")
  if (!ordinal) {
    rate <- 1 / mean(dur)
    xs <- seq(0, max(dur), length.out = 200)
    lines(xs, dexp(xs, rate = rate), col = "#cd0303", lwd = 2, lty = 2)
    legend("topright", legend = "exponential (same mean)", col = "#cd0303", lty = 2, lwd = 2, bty = "n", cex = 0.9)
  }
  if (n_cens > 0) mtext(sprintf("%d censored spell(s) excluded", n_cens), side = 3, line = 0.2, cex = 0.8, col = "#888888")
}

# ---- [plot 12] concurrency over time (durem only) (NEW) ---------------------
# count of simultaneously-active spells. +1 at start, -1 at end; end-inclusive
# via start-before-end tie ordering (your end >= t convention). censored spells
# stay active to observation end.
.plot_concurrency <- function(x, ordinal) {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  par(mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))
  el <- x$edgelist
  starts <- el$time; ends <- el$end
  tmax <- max(c(starts, ends), na.rm = TRUE)
  ends[is.na(ends)] <- tmax
  n <- length(starts)
  ev <- data.frame(t = c(starts, ends), d = c(rep(1L, n), rep(-1L, n)), o = c(rep(0L, n), rep(1L, n)))
  ev <- ev[order(ev$t, ev$o), ]
  active <- cumsum(ev$d)
  xlab <- if (ordinal) "time (event index)" else "time"
  plot(c(min(starts), ev$t), c(0, active), type = "s", col = "#5AAFC8", lwd = 2,
       xlab = xlab, ylab = "concurrently active spells", main = "Concurrency over time")
  abline(h = max(active), col = "#cd0303", lty = 3)
  mtext(sprintf("peak = %d", max(active)), side = 3, line = 0.2, cex = 0.85, col = "#888888")
}

# ---- [plot 13] event spells (Gantt) (durem only) (NEW) ----------------------
# one lane per dyad, a segment per spell from start to end. capped at top-k
# most active dyads. coloured by type when the history is typed.
.plot_gantt <- function(x, directed, top_k = 30L) {
  op <- par(no.readonly = TRUE); on.exit(par(op))
  el <- x$edgelist
  starts <- el$time; ends <- el$end
  tmax <- max(c(starts, ends), na.rm = TRUE)
  censored <- is.na(ends); ends[censored] <- tmax

  if (directed) {
    dlab <- paste(el$actor1, el$actor2, sep = " \u2192 ")
  } else {
    dlab <- apply(el[, c("actor1", "actor2")], 1, function(l) paste(sort(as.character(l)), collapse = " \u2014 "))
  }
  freq <- sort(table(dlab), decreasing = TRUE)
  keep_lab <- head(names(freq), top_k)
  sel <- dlab %in% keep_lab
  lev <- rev(keep_lab)
  yrow <- match(dlab, lev)

  has_type <- !is.null(el$type)
  if (has_type) {
    types <- sort(unique(el$type))
    tcol <- grDevices::hcl.colors(max(length(types), 2L), palette = "Dark 3")[seq_along(types)]
    names(tcol) <- types
    seg_col <- tcol[as.character(el$type)]
  } else {
    seg_col <- rep("#5AAFC8", nrow(el))
  }

  left_lines <- min(max(nchar(lev)) * 0.55 + 2, 14)
  par(mar = c(4.5, left_lines, 3, 1), mgp = c(2.5, 0.7, 0))
  plot(NA, xlim = range(c(starts, ends)), ylim = c(0.5, length(lev) + 0.5),
       xlab = "time", ylab = "", yaxt = "n", main = "Event spells (Gantt)")
  abline(h = seq_along(lev), col = "gray92")
  axis(2, at = seq_along(lev), labels = lev, las = 2, tick = FALSE, cex.axis = 0.7)
  idx <- which(sel)
  segments(x0 = starts[idx], y0 = yrow[idx], x1 = ends[idx], y1 = yrow[idx],
           col = seg_col[idx], lwd = 3, lend = 1)
  if (any(censored & sel)) {
    ci <- which(censored & sel)
    points(ends[ci], yrow[ci], pch = 62, col = seg_col[ci], cex = 0.8)  # ">" open tip
  }
  if (has_type) legend("topright", legend = names(tcol), col = tcol, lwd = 3, bty = "n", cex = 0.8, horiz = TRUE)
  if (length(freq) > top_k) warning(sprintf("more than %d dyads; showing the %d most active.", top_k, top_k))
}


# =============================================================================
# dispatcher
# =============================================================================

#' Generic plot method
#'
#' @title plot.remify
#' @rdname plot.remify
#' @description several plots that describe the network of relational events, both for directed and undirected relational events.
#' @param x is a \code{remify} object.
#' @param which one or more numbers between 1 and 7. Plots described in order: (1) distribution of the inter-event times
#' (histogram), (2) tile plot titled 'activity plot', with in-degree and out-degree activity line plots on the sides (or
#' total-degree on the top side if the network is undirected). Tiles' color is scaled based on the count of the directed
#' (or undirected) dyad, (3) for directed networks two plots of normalized out-degree and in-degree (values ranging in [0,1])
#' over a set of \code{n_intervals} (evenly spaced). For undirected networks one plot of normalized total-degree over the
#' \code{n_intervals} (also here values ranging in [0,1]). The normalization is calculated in each interval as the
#' \code{(degree-min(degree))/(max(degree)-min(degree)))} for each actor considering minimum and maximum degree (in-,
#' out- or total-) observed in the interval (opacity and size of the points is proportional to the normalized measure),
#' (4) four plots: (i) number of events (# events) per time interval, (ii) proportion of observed dyads (# dyads / x$D)
#' per time interval, (iii) and (iv) (for directed network only) proportion of active senders and receivers per time interval
#' (calculated as # senders/ x$N and # receiver/x$N per interval), (5) two networks: (i) network of events where edges are
#' considered undirected (edges' opacity is proportional to the counts of the undirected events, vertices' opacity is
#' proportional to the total-degree of the actors), (ii) visualization of directed network (edges' opacity is proportional
#' to the counts of the directed events, vertices' opacity is proportional to the in-degree of the actors), (6) counting
#' process N(t): cumulative number of events against real (non-ordinal) time, with a dashed constant-rate reference line.
#' A roughly straight curve indicates a stationary baseline rate; systematic departures or kinks indicate non-stationarity.
#' Skipped for ordinal event histories, (7) dyad event raster: one row per dyad with a tick per event, capped at the top 30
#' most active dyads. For directed histories the two directions of a pair are kept as adjacent rows, so reciprocity appears
#' as neighbouring rows firing in sequence and inertia as horizontal clustering of ticks, (8) distribution of dyad recurrence
#' times: pooled gaps between successive events on the same dyad, with a dashed same-mean exponential reference. Excess mass
#' at short gaps (above the reference) indicates temporal clustering, suggesting window/decay memory effects rather than plain
#' inertia. For directed histories recurrence is measured on directed dyads; for ordinal histories the gap is the event-index
#' distance and the exponential reference is omitted, (9) dyad concentration (Lorenz curve): cumulative share of events against
#' cumulative share of dyads, taken over all \code{x$D} possible dyads so that never-active dyads enter as zeros. The dashed
#' diagonal is perfect equality; a curve bowing toward the bottom-right indicates events concentrated on few dyads, which motivates
#' frailty and makes per-dyad effects harder to identify. A Gini coefficient is reported in the corner, (10) reciprocity scatter
#' (directed histories only): for each unordered pair, the event count in the busier direction against the count in the quieter
#' direction, with point area proportional to the pair's total volume. Points near the dashed diagonal indicate balanced exchange
#' (reciprocity has little asymmetry to explain); points near the horizontal axis indicate one-directional flow (reciprocity is
#' informative), (11) distribution of event durations (\code{remify_durem} objects only): histogram of spell lengths (\code{end - time})
#' with a same-mean exponential reference for interval histories; for ordinal histories the duration is an event-index gap and the
#' reference is omitted, (12) concurrency over time (\code{remify_durem} only): number of simultaneously-active spells as a step
#' function, with the peak marked; spells are counted as active over the closed interval \code{[time, end]}, (13) event spells / Gantt
#' (\code{remify_durem} only): one lane per dyad with a segment per spell from start to end, capped at the 30 most active dyads and
#' coloured by event type when the history is typed. Note, plots 2–4 use multi-panel layouts and need a sufficiently large graphics
#' device. If you see "figure margins too large", enlarge the plot pane or open a larger device with e.g. \code{dev.new()}.
#' @param breaks default is \code{15L} and it describes the number of cells of the histogram plot for the inter-event times. It
#' can be specified in the same way as the argument used by the function \code{graphics::hist()} (see ?graphics::hist for more details).
#' @param palette a palette from \code{grDevices::hcl.pals()} (default is the \code{"Purples"} palette).
#' @param n_intervals number of time intervals for time plots (default is \code{10}).
#' @param rev default is TRUE (reverse order of the color specified in \code{palette})
#' @param actors default is the set of actors in the network (see \code{.dict[["actors"]]}). The user can specify a subset of actors
#' on which to run the descriptive plots. If the set contains more than 50 actors, then the function will select the 50 most active
#' actors from the set provided.
#' @param pch.degree default is 20. Shape of the points for the degree plots (in-degree, out-degree, total-degree).
#' @param igraph.edge.color color of the edges in visualization of the network with vertices and nodes. The user can specify the hex
#' value of a color, the color name or use the function\code{grDevices::rgb()} which returns the hex value.
#' @param igraph.vertex.color color of the vertices in visualization of the network with vertices and nodes. The user can specify the
#' hex value of a color, the color name or use the function \code{grDevices::rgb()} which returns the hex value.
#' @param ... other graphical parameters
#'
#' @return no return value, called for plotting descriptives on the relational event history data.
#'
#' @method plot remify
#' @export
plot.remify <- function(x,
                        which = c(1:10),
                        breaks = 15L,
                        palette = "Purples",
                        n_intervals = 10L,
                        rev = TRUE,
                        actors = NULL,
                        pch.degree = 20,
                        igraph.edge.color = "#4daa89",
                        igraph.vertex.color = "#5AAFC8",
                        ...) {

  # ---- resolve object structure (old attr vs new $meta) ----
  if (!is.null(x$meta)) {
    .dict     <- x$meta$dictionary
    .ordinal  <- isTRUE(x$meta$ordinal)
    .directed <- isTRUE(x$meta$directed)
    .ncores   <- x$meta$ncores
  } else {
    .dict     <- attr(x, "dictionary")
    .ordinal  <- isTRUE(attr(x, "ordinal"))
    .directed <- isTRUE(attr(x, "directed"))
    .ncores   <- attr(x, "ncores")
  }
  if (is.null(actors)) actors <- .dict$actors$actorName

  # ---- which plots ----
  if (missing(which)) which <- if (inherits(x, "remify_durem")) 1:13 else 1:10
  selected <- which
  which <- rep(FALSE, 13)
  which[selected] <- TRUE

  # ---- validate scalar args ----
  if (is.null(breaks)) breaks <- 15L
  if (is.null(palette)) palette <- "Purples" else palette <- as.character(palette)
  if (is.null(n_intervals)) n_intervals <- 3L else n_intervals <- as.integer(n_intervals)
  if (is.null(rev)) rev <- TRUE else rev <- as.logical(rev)

  if (is.null(pch.degree)) {
    pch.degree <- 20
  } else {
    if ((pch.degree > 25) | (pch.degree < 1)) pch.degree <- 20
    pch.degree <- as.integer(pch.degree)
  }

  if (is.null(igraph.edge.color))   igraph.edge.color   <- "#4daa89"
  if (is.null(igraph.vertex.color)) igraph.vertex.color <- "#5AAFC8"
  if (substr(igraph.edge.color, 1, 1) != "#") {
    edge_rgb <- grDevices::col2rgb(col = igraph.edge.color)
    igraph.edge.color <- rgb(red = edge_rgb[1] / 255, green = edge_rgb[2] / 255, blue = edge_rgb[3] / 255)
  }
  if (substr(igraph.vertex.color, 1, 1) != "#") {
    vertex_rgb <- grDevices::col2rgb(col = igraph.vertex.color)
    igraph.vertex.color <- rgb(red = vertex_rgb[1] / 255, green = vertex_rgb[2] / 255, blue = vertex_rgb[3] / 255)
  }
  if (nchar(igraph.edge.color) != 7)   igraph.edge.color   <- "#4daa89"
  if (nchar(igraph.vertex.color) != 7) igraph.vertex.color <- "#5AAFC8"

  dict <- .dict
  ordinal <- .ordinal

  # ---- actors input ----
  if (is.null(actors)) {
    actors <- dict$actors$actorName
  } else {
    actors <- sort(unique(as.character(actors)))
    check_actors <- sapply(actors, function(y) y %in% dict$actors$actorName)
    if (any(!check_actors)) {
      stop("one or more actors' names ('actors') are not found in the remify object 'x'.")
    }
    rm(check_actors)
  }

  # ---- actor subsetting (50-actor cap), run ONCE ----
  if ((x$N > 50) & (length(actors) > 50)) {
    if (length(actors) < x$N) {
      events_to_select <- which((x$edgelist$actor1 %in% actors) & (x$edgelist$actor2 %in% actors))
      if (length(events_to_select) == 0) {
        stop("no events found when selecting the set of actors (supplied via the argument 'actors').")
      }
      x$edgelist <- x$edgelist[events_to_select, ]
      rm(events_to_select)
    }
    # most-active actors by dyad frequency — work on columns directly,
    # never delimiter-join names (actor names may contain "_")
    dyad_key <- do.call(paste, c(x$edgelist[, c("actor1", "actor2")], sep = "\r"))
    dyads_freq_sorted <- sort(table(dyad_key), decreasing = TRUE)
    actors_loc <- 0
    d_loc <- 25
    actors_until_d_loc <- NULL
    while (actors_loc < 50 && d_loc <= length(dyads_freq_sorted)) {
      top_keys <- names(dyads_freq_sorted[seq_len(d_loc)])
      actors_until_d_loc <- unique(unlist(strsplit(top_keys, "\r", fixed = TRUE)))
      actors_loc <- length(actors_until_d_loc)
      d_loc <- d_loc + 1
    }
    actors <- sort(actors_until_d_loc)
    events_to_select <- which((x$edgelist$actor1 %in% actors) & (x$edgelist$actor2 %in% actors))
    x$edgelist <- x$edgelist[events_to_select, ]
    x$N <- actors_loc
    x$D <- ifelse(.directed, x$N * (x$N - 1), x$N * (x$N - 1) / 2)
    rm(dyads_freq_sorted, actors_loc, d_loc, actors_until_d_loc, events_to_select)
    warning(paste("Too many actors for rendering plots with a good quality: the 50 most active actors are selected (descriptives on dyads and actors may differ from the descriptives conducted on the whole set of actors)"))
  } else {
    if (length(actors) < x$N) {
      x$N <- length(actors)
      x$D <- ifelse(.directed, x$N * (x$N - 1), x$N * (x$N - 1) / 2)
      events_to_select <- which((x$edgelist$actor1 %in% actors) & (x$edgelist$actor2 %in% actors))
      if (length(events_to_select) == 0) {
        stop("no events found when selecting the set of actors (supplied via the argument 'actors').")
      }
      x$edgelist <- x$edgelist[events_to_select, ]
      rm(events_to_select)
    }
  }

  # ---- dispatch ----
  if (which[1L] & !ordinal) .plot_interevent(x, breaks)
  if (which[2L])            .plot_activity(x, actors, .directed, palette, rev, .ncores)
  if (which[3L]) .plot_normdegree(x, actors, .directed, n_intervals, palette, rev)
  if (which[4L])            .plot_intervals(x, .directed, n_intervals, .ncores)
  if (which[5L])            .plot_network(x, actors, .directed, .ncores, igraph.edge.color, igraph.vertex.color)
  if (which[6L] & !ordinal) .plot_counting(x)
  if (which[7L]) .plot_raster(x, .directed)
  if (which[8L]) .plot_recurrence(x, .directed, ordinal, breaks)
  if (which[9L]) .plot_concentration(x, .directed)
  if (which[10L] & .directed) .plot_reciprocity(x)

  is_durem <- inherits(x, "remify_durem")
  if (any(which[11:13]) && !is_durem)
    warning("plots 11-13 require a duration (remify_durem) object; skipped.")
  if (which[11L] && is_durem) .plot_duration(x, ordinal, breaks)
  if (which[12L] && is_durem) .plot_concurrency(x, ordinal)
  if (which[13L] && is_durem) .plot_gantt(x, .directed)

  invisible(NULL)
}
