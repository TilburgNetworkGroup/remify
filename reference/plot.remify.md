# plot.remify

several plots that describe the network of relational events, both for
directed and undirected relational events.

## Usage

``` r
# S3 method for class 'remify'
plot(
  x,
  which = c(1:5),
  breaks = 15L,
  palette = "Purples",
  n_intervals = 4L,
  rev = TRUE,
  actors = attr(x, "dictionary")$actors$actorName,
  pch.degree = 20,
  igraph.edge.color = "#4daa89",
  igraph.vertex.color = "#5AAFC8",
  ...
)
```

## Arguments

- x:

  is a `remify` object.

- which:

  one or more numbers between 1 and 5. Plots described in order: (1)
  distribution of the inter-event times (histogram), (2) tile plot
  titled 'activity plot', with in-degree and out-degree activity line
  plots on the sides (or total-degree on the top side if the network is
  undirected). Tiles' color is scaled based on the count of the directed
  (or undirected) dyad, (3) for directed networks two plots of
  normalized out-degree and in-degree (values ranging in \[0,1\]) over a
  set of `n_intervals` (evenly spaced). For undirected networks one plot
  of normalized total-degree over the `n_intervals` (also here values
  ranging in \[0,1\]). The normalization is calculated in each interval
  as the `(degree-min(degree))/(max(degree)-min(degree)))` for each
  actor considering minimum and maximum degree (in-, out- or total-)
  observed in the interval (opacity and size of the points is
  proportional to the normalized measure), (4) four plots: (i) number of
  events (# events) per time interval, (ii) proportion of observed dyads
  (# dyads / x\$D) per time interval, (iii) and (iv) (for directed
  network only) proportion of active senders and receivers per time
  interval (calculated as \# senders/ x\$N and \# receiver/x\$N per
  interval), (5) two networks: (i) network of events where edges are
  considered undirected (edges' opacity is proportional to the counts of
  the undirected events, vertices' opacity is proportional to the
  total-degree of the actors), (ii) visualization of directed network
  (edges' opacity is proportional to the counts of the directed events,
  vertices' opacity is proportional to the in-degree of the actors).

- breaks:

  default is `15L` and it describes the number of cells of the histogram
  plot for the inter-event times. It can be specified in the same way as
  the argument used by the function
  [`graphics::hist()`](https://rdrr.io/r/graphics/hist.html) (see
  ?graphics::hist for more details).

- palette:

  a palette from
  [`grDevices::hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html)
  (default is the `"Purples"` palette).

- n_intervals:

  number of time intervals for time plots (default is `10`).

- rev:

  default is TRUE (reverse order of the color specified in `palette`)

- actors:

  default is the set of actors in the network (see
  `attr(x,"dictionary")[["actors"]]`). The user can specify a subset of
  actors on which to run the descriptive plots. If the set contains more
  than 50 actors, then the function will select the 50 most active
  actors from the set provided.

- pch.degree:

  default is 20. Shape of the points for the degree plots (in-degree,
  out-degree, total-degree).

- igraph.edge.color:

  color of the edges in visualization of the network with vertices and
  nodes. The user can specify the hex value of a color, the color name
  or use the
  function[`grDevices::rgb()`](https://rdrr.io/r/grDevices/rgb.html)
  which returns the hex value.

- igraph.vertex.color:

  color of the vertices in visualization of the network with vertices
  and nodes. The user can specify the hex value of a color, the color
  name or use the function
  [`grDevices::rgb()`](https://rdrr.io/r/grDevices/rgb.html) which
  returns the hex value.

- ...:

  other graphical parameters

## Value

no return value, called for plotting descriptives on the relational
event history data.

## Details

Generic plot method
