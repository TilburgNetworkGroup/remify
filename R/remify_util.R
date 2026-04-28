
#' Generic plot method
#'
#' @title plot.remify
#' @rdname plot.remify
#' @description several plots that describe the network of relational events, both for directed and undirected relational events.
#' @param x is a \code{remify} object.
#' @param which one or more numbers between 1 and 5. Plots described in order: (1) distribution of the inter-event times (histogram), (2) tile plot titled 'activity plot', with in-degree and out-degree activity line plots on the sides (or total-degree on the top side if the network is undirected). Tiles' color is scaled based on the count of the directed (or undirected) dyad, (3) for directed networks two plots of normalized out-degree and in-degree (values ranging in [0,1]) over a set of \code{n_intervals} (evenly spaced). For undirected networks one plot of normalized total-degree over the \code{n_intervals} (also here values ranging in [0,1]). The normalization is calculated in each interval as the \code{(degree-min(degree))/(max(degree)-min(degree)))} for each actor considering minimum and maximum degree (in-, out- or total-) observed in the interval (opacity and size of the points is proportional to the normalized measure), (4) four plots: (i) number of events (# events) per time interval, (ii) proportion of observed dyads (# dyads / x$D) per time interval, (iii) and (iv) (for directed network only) proportion of active senders and receivers per time interval (calculated as # senders/ x$N and # receiver/x$N per interval), (5) two networks: (i) network of events where edges are considered undirected (edges' opacity is proportional to the counts of the undirected events, vertices' opacity is proportional to the total-degree of the actors), (ii) visualization of directed network (edges' opacity is proportional to the counts of the directed events, vertices' opacity is proportional to the in-degree of the actors).
#' @param breaks default is \code{15L} and it describes the number of cells of the histogram plot for the inter-event times. It  can be specified in the same way as the argument used by the function \code{graphics::hist()} (see ?graphics::hist for more details).
#' @param palette a palette from \code{grDevices::hcl.pals()} (default is the \code{"Purples"} palette).
#' @param n_intervals number of time intervals for time plots (default is \code{10}).
#' @param rev default is TRUE (reverse order of the color specified in \code{palette})
#' @param actors default is the set of actors in the network (see \code{.dict[["actors"]]}). The user can specify a subset of actors on which to run the descriptive plots. If the set contains more than 50 actors, then the function will select the 50 most active actors from the set provided.
#' @param pch.degree default is 20. Shape of the points for the degree plots (in-degree, out-degree, total-degree).
#' @param igraph.edge.color color of the edges in visualization of the network with vertices and nodes. The user can specify the hex value of a color, the color name or use the function\code{grDevices::rgb()} which returns the hex value.
#' @param igraph.vertex.color color of the vertices in visualization of the network with vertices and nodes. The user can specify the hex value of a color, the color name or use the function \code{grDevices::rgb()} which returns the hex value.
#' @param ... other graphical parameters
#'
#' @return no return value, called for plotting descriptives on the relational event history data.
#'
#' @method plot remify
#' @export
plot.remify <- function(x,
                    which = c(1:5),
                    breaks= 15L,
                    palette = "Purples",
                    n_intervals = 4L,
                    rev = TRUE,
                    actors = NULL,
                    pch.degree = 20,
                    igraph.edge.color = "#4daa89",
                    igraph.vertex.color = "#5AAFC8",
                    ...){

  # support both old remify (attributes) and new remify ($meta) structure
  if (!is.null(x$meta)) {
    .dict    <- x$meta$dictionary
    .ordinal <- isTRUE(x$meta$ordinal)
    .directed <- isTRUE(x$meta$directed)
    .ncores  <- x$meta$ncores
  } else {
    .dict    <- attr(x, "dictionary")
    .ordinal <- isTRUE(attr(x, "ordinal"))
    .directed <- isTRUE(attr(x, "directed"))
    .ncores  <- attr(x, "ncores")
  }
  if (is.null(actors)) actors <- .dict$actors$actorName

  # checks on input arguments

  # which plot
  selected <- which
  which <- rep(FALSE,5)
  which[selected] <- TRUE

  # breaks, palette, n_intervals and rev
  if(is.null(breaks)) breaks <- 15L
  if(is.null(palette)) palette <- "Purples" else palette <- as.character(palette)
  if(is.null(n_intervals)) n_intervals <- 3L else n_intervals <- as.integer(n_intervals)
  if(is.null(rev)) rev <- TRUE else rev <- as.logical(rev)

  # pch.degree
  if(is.null(pch.degree)){
    pch.degree <- 20
  }
  else{
    if((pch.degree > 25) | (pch.degree < 1)){
      pch.degree <- 20
    }
    pch.degree <- as.integer(pch.degree)
  }

  # igraph.edge.color and igraph.vertex.color
  if(is.null(igraph.edge.color)){
    igraph.edge.color <- "#4daa89"
  }
  if(is.null(igraph.vertex.color)){
    igraph.vertex.color <- "#5AAFC8"
  }
  if(substr(igraph.edge.color,1,1) != "#"){
    # if it is not an hex color then convert it to hex
    edge_rgb <- grDevices::col2rgb(col=igraph.edge.color)
    igraph.edge.color <- rgb(red = edge_rgb[1]/255,green = edge_rgb[2]/255, blue = edge_rgb[3]/255)
  }
  if(substr(igraph.vertex.color,1,1) != "#"){
    vertex_rgb <- grDevices::col2rgb(col=igraph.vertex.color)
    igraph.vertex.color <- rgb(red = vertex_rgb[1]/255,green = vertex_rgb[2]/255, blue = vertex_rgb[3]/255)
  }
  if(nchar(igraph.edge.color) != 7) igraph.edge.color <- "#4daa89"
  if(nchar(igraph.vertex.color) != 7) igraph.vertex.color <- "#5AAFC8"

  # storing some importnat information in advance
  dict <- .dict
  ordinal <- .ordinal

  # actors input
  if(is.null(actors)){
    actors <- dict$actors$actorName
  }
  else{
    # check if 'actors' contains names that are not in the dictionary
    actors <- sort(unique(as.character(actors)))
    check_actors <- sapply(actors , function(y) y %in% dict$actors$actorName)
    if(any(!check_actors)){
      stop("one or more actors' names ('actors') are not found in the remify object 'x'.")
    }
    rm(check_actors)
  }

  # limiting the number of actors to N = 50 (when x$N > 50)
  if((x$N > 50) & (length(actors)>50)){
    if(length(actors) < x$N){
      # only when length(actors)>50 but less than x$N, the function does its best by selecting the 50 most active actors out of the subset defined by the input "actors"
      events_to_select <- which((x$edgelist$actor1 %in% actors) & (x$edgelist$actor2 %in% actors))
      if(length(events_to_select) == 0){
          stop("no events found when selecting the set of actors (supplied via the argument 'actors').")
      }
      x$edgelist <- x$edgelist[events_to_select,]
      rm(events_to_select)
    }

    # selecting the first 50 most active actors (starting from the most frequent dyads)
    dyads_freq <- table(apply(x$edgelist,1,function(x) paste(x[2],x[3],sep="_")))
    dyads_freq_sorted <- sort(dyads_freq,decreasing = TRUE)
    actors_loc <- 0
    d_loc <- 25
    actors_until_d_loc <- NULL
    while(actors_loc < 50){
        actors_until_d_loc <-sapply(names(dyads_freq_sorted[1:d_loc]), function(y) (unlist(strsplit(x = y,  split = "_",fixed=TRUE))))
        actors_loc <- length(unique(as.vector(actors_until_d_loc)))
        d_loc <- d_loc + 1

    }
    # updating vector of actors' names after selecting the 50 most active actors
    actors <- sort(unique(as.vector(actors_until_d_loc)))

    # finding vector of events including only the 50 most active actors and sub-setting the x$edgelist
    events_to_select <- which((x$edgelist$actor1 %in% actors) & (x$edgelist$actor2 %in% actors))
    x$edgelist <- x$edgelist[events_to_select,]
    x$N <- actors_loc # can be 49-50-51, not always exaclty 50
    x$D <- ifelse(.directed,x$N*(x$N-1),x$N*(x$N-1)/2)

    # free-ing space
    rm(dyads_freq,dyads_freq_sorted,actors_loc,d_loc,actors_until_d_loc,events_to_select)

    # display warning to the UI about the subset of actors
    warning(paste("Too many actors for rendering plots with a good quality: the 50 most active actors are selected (descriptives on dyads and actors may differ from the descriptives conducted on the whole set of actors)"))
  }
  else{ # when x$N <= 50
    if(length(actors) < x$N){
      # when length(actors) is less than x$N (analysis on a subset of actors that is smaller than 50 actors)
      x$N <- length(actors)
      x$D <- ifelse(.directed,x$N*(x$N-1),x$N*(x$N-1)/2)
      events_to_select <- which((x$edgelist$actor1 %in% actors) & (x$edgelist$actor2 %in% actors))
      if(length(events_to_select) == 0){
        stop("no events found when selecting the set of actors (supplied via the argument 'actors').")
      }
      x$edgelist <- x$edgelist[events_to_select,]
      rm(events_to_select)
    }
  }

  # dyads, in-degree, out-degree and total-degree
  dyads <- paste(x$edgelist$actor1,x$edgelist$actor2,sep="_")
  if(.directed){
      in_degree <- table(factor(x$edgelist$actor2,levels=actors))
      out_degree <- table(factor(x$edgelist$actor1,levels=actors))
  }
  else{
      total_degree <- table(factor(c(x$edgelist$actor1,x$edgelist$actor2),levels=actors))
  }

  if((which[4L]  & !.directed) | any(which[c(2L,5L)])){
    # X_dyad is used by plot 5 and total_degree is used by plot 2 when the network is undirected
    # [incomplete] matrix of dyad frequencies by [actor1-actor2] with no direction (undirected) taken into account (some dyads may not be included at this stage, only observed ones are included)
    cl <- parallel::makeCluster(.ncores)
    dyad_no_direction_freq <-parallel::parApply(cl = cl, X = x$edgelist, MARGIN = 1,FUN = function(l) {
      dyad_l <- sort(as.character(c(l[2],l[3])))
      paste(dyad_l[1],dyad_l[2],sep="_")
      })
    parallel::stopCluster(cl)
    if(which[4L]  & !.directed){
      dyads <- dyad_no_direction_freq
    }
  }

  if(any(which[c(2L,5L)])){
    # ... frequencies dyads (sorted from large to small frequency values)
    dyad_freq <- table(dyads)

    # [incomplete] matrix of dyad frequencies by [actor1-actor2] (some dyads may not be included at this stage, only observed one are included)
    if(.directed){
      X <- matrix(NA, nrow=length(dyad_freq),ncol=3)
      X[,3] <- as.integer(dyad_freq)
      for(d in 1:length(dyad_freq)){
        X[d,1:2] <- unlist(strsplit(x = names(dyad_freq[d]),  split = "_"))
      }
      X <- X[order(X[,1],X[,2]),]
    }

    dyad_no_direction_freq <- table(dyad_no_direction_freq)
    X_dyad <- matrix(NA, nrow=length(dyad_no_direction_freq),ncol=3)
    X_dyad[,3] <- as.integer(dyad_no_direction_freq)
    for(d in 1:length(dyad_no_direction_freq)){
      X_dyad[d,1:2] <- unlist(strsplit(x = names(dyad_no_direction_freq[d]),split = "_"))
    }
    X_dyad <- X_dyad[order(X_dyad[,1],X_dyad[,2]),]
    rm(dyad_no_direction_freq)
    if(!.directed){
      # only for undirected networks (if undirected, X and X_dyad are the same)
      X <- X_dyad
    }
  }

  if(any(which[c(3L,4L)])){
    time <- x$edgelist$time
    time <- cut(time,breaks = n_intervals)
    y <- tapply(X = x$edgelist$actor1, INDEX = time, FUN = length)
    y[is.na(y)] <- 0
  }

  # important settings for graphics parameters after running the plot function
  # disabling dynamic pages for remify 3.2.0
  #ask_new_page <- devAskNewPage(TRUE)
  #on.exit(expr = devAskNewPage(ask_new_page))
  op <- par(no.readonly = TRUE)
  on.exit(expr = par(op))

  ## [[plot 1]] plotting histogram of the waiting times:
    # y-axis = Frequency (freq=TRUE)
    # x-axis = measurement unit of the waiting time
  if(which[1L] & !ordinal){
    time_unit <- NULL
    time_unit <- attr(x$intereventTime,"unit") # [[CHECK!!]] add attribute to x$intereventTime object based on the time scale (default is seconds if time is timestamp, or days if it is a Date )
    #dev.hold()
    hist(x = x$intereventTime,
      breaks = breaks,
      angle = 45,
      col = "lavender",
      border = "darkgray",
      main = paste("Distribution of the inter-event times",collapse=""),
      xlab = ifelse(!is.null(time_unit),paste("waiting time (",time_unit,")",sep="", collapse=""),paste("waiting time")),
      freq = TRUE)
    #dev.flush()
  }

  # [[plot 2]] two-way table of event counts (with marginal distributions on actor1 and actor2, or total degree if undirected network): actor- and tie- oriented networks
  if(which[2L]){
    # actors that interacted either as 'sender' or 'receiver' (or 'actor1' and 'actor2' in case of undirected networks)
    egrid <- expand.grid(actors,actors)
    egrid <- egrid[,2:1]
    if(.directed){
      tile_plot_x_axis_name <- "receiver"
      tile_plot_y_axis_name <- "sender"
      upper_plot_y_axis_name <- "in-degree"
    }
    else{
      tile_plot_x_axis_name <- "actor2"
      tile_plot_y_axis_name <- "actor1"
      upper_plot_y_axis_name <- "total-degree"
    }

    # [complete] matrix of dyad frequencies
    X_out <- data.frame(row = egrid[,1],col = egrid[,2], fill = NA)
    for(d in 1:dim(X)[1]){
      row_index <- which((X_out$row == X[d,1]) & (X_out$col == X[d,2]))
      X_out$fill[row_index] <- as.integer(X[d,3])
    }

    # assigning actors positions on the grid
    X_out[,1:2] <- expand.grid(1:length(actors),1:length(actors)) # for positioning actors on the grid
    X_out[,1:2] <- X_out[,2:1]

    # setting up axes measures
    if(.directed){
      max_upper_plot <- max(unname(in_degree))+2
      min_upper_plot <- min(unname(in_degree))-1
      max_out_degree <- max(unname(out_degree))+2
      min_out_degree <- min(unname(out_degree))-1
      total_or_in_degree <- in_degree
    }
    else{
      # when the network is undirected use 'total_degree' but keep same name to avoid redundant code
      max_upper_plot <- max(unname(total_degree))+2
      min_upper_plot <- min(unname(total_degree))-1
      total_or_in_degree <- total_degree
    }

    # creating layout and setting up graphical parameters
    if(.directed){
      layout_matrix <- matrix(c(3,2,1,4), ncol=2, byrow=TRUE)
    }
    else{
      layout_matrix <- matrix(c(3,2,1,2), ncol=2, byrow=TRUE)
    }
    colors_legend <- unique(sort(X_out$fill))
    bottom_and_left_mai <- max(strwidth(actors, "inch")+0.4, na.rm = TRUE)
    #dev.hold()
    layout(layout_matrix, widths=c(4/5,1/5), heights=c(1/5,4/5))
    par(oma=c(2,2,2,2))
    par(mar =c(6,6,1,1))
    par(mgp=c(6,1,0))

    # [1] tile plot
    plot.new()
    plot.window(xlim=c(1,x$N),ylim=c(1,x$N))
    with(X_out,{
      rect(col-0.5,row-0.5,col+0.5,row+0.5,col=hcl.colors(n=max(unique(sort(fill))),palette=palette,rev=rev)[fill],border="#ffffff")
      segments(x0=c(1:x$N)+0.5,y0=c(1:x$N)-0.5,x1=c(1:x$N)-0.5,y1=c(1:x$N)+0.5,col="gray")
      segments(x0=0.5,y0=0.5,x1=(x$N+0.5),y1=(x$N+0.5),col="gray")
      # actor names
      text(x = c(1:x$N), y = 0, labels = actors, srt = 90, pos = 1, xpd = TRUE,  adj = c(0.5,1), offset = 2.5)
      text(x = 0, y = c(1:x$N), labels = actors, srt = 0, pos = 2, xpd = TRUE,  adj = c(1,0.5), offset = 0)
      # axes names
      mtext(text  = tile_plot_x_axis_name, side=1, line=5, outer=FALSE, adj=0, at=floor(x$N/2))
      mtext(text = tile_plot_y_axis_name, side=2, line=5, outer=FALSE, adj=1, at=floor(x$N/2))
    })

    # [2] legend of tile plot
    par(mar=c(0,0,1,1))
    plot(0, 0, type="n", xlim = c(0, 5), ylim = c(0, 7), axes = FALSE, xlab = "", ylab = "")
    # colors' legend
    rect(xleft = 2, ybottom = seq(0,5,length=max(colors_legend)), xright = 3, ytop = seq(1.25,6.25,length=max(colors_legend)),col = hcl.colors(n=max(colors_legend),palette=palette,rev=rev)[1:max(colors_legend)], border = NA)
    # borders and ticks
    rect(xleft=2,ybottom=0,xright=3,ytop=6.25)
    segments(x0=c(2,2.8),y0=rep(seq(0,6.25,length=3)[2],2),x1=c(2.2,3))
    text(x = rep(3.2,3) , y = seq(0.1,6.25,length=3), labels = c(1,floor(median(colors_legend)),max(colors_legend)), adj = c(0,0.5))
    text(x = 2.5, y = 6.6, labels = "events",adj =c(0.5,0),cex=1.25)

    # [3] line plots in-degree
    par(mar=c(0,6,1,1))
    plot(x=1:x$N, type = 'n', xlim = c(1,x$N), ylim = c(min_upper_plot,max_upper_plot),axes=FALSE,frame.plot=FALSE,xlab="",ylab="")
    title(ylab=upper_plot_y_axis_name, line = 5)
    abline(v=seq(1,x$N,by=1),col = "gray", lty = "dotted", lwd = par("lwd"))
    segments(x0=seq(1,x$N,by=1),y0=0,y1=total_or_in_degree,lwd=2,col="cadetblue3")
    points(x=seq(1,x$N,by=1),y=total_or_in_degree,type="p",pch=19,cex=1,col="cadetblue3")
    axis(side=2)

    # [4] line plots out-degree
    if(.directed){
      par(mar=c(6,0,1,1))
      plot(x = seq(min_out_degree,max_out_degree,length=x$N), y = 1:x$N, type = 'n', xlim = c(min_out_degree,max_out_degree), ylim = c(1,x$N),axes=FALSE,frame.plot=FALSE,xlab="",ylab="")
      title(xlab="out-degree", line = 5)
      abline(h=seq(1,x$N,by=1),col = "gray", lty = "dotted", lwd = par("lwd"))
      segments(x0=0,y0=seq(1,x$N,by=1),x1=as.vector(unname(out_degree)),lwd=2,col="cadetblue3")
      points(x=as.vector(unname(out_degree)),y=seq(1,x$N,by=1),type="p",pch=19,cex=1,col="cadetblue3")
      axis(side=1)
      title(main="Activity plot",outer=TRUE)
      par(op)
    }
    #dev.flush()
  }

  ## [[plot 3]] normalized degrees (per actor and per intervals):
    # for directed networks: normalized in-degree and out-degree
    # for undirected networks: normalized total-degree
    # normalization means that per each intervals the in-/out-/total- degree of each actor is normalized for the minimum and maximum observed value of in-/out-/total- degree across all the actors. The normalization is carried out as follows: (degree-min(degree))/(max(degree)-min(degree))
  if(which[3L]){
    if(.directed){
      # out-degree plot
      tab_s <- tapply(X =x$edgelist$actor1, INDEX = time, FUN = function(w) table(w))
      #dev.hold()
      par(mfrow=c(1,1))
      left_mai <- max(strwidth(actors, "inch")+0.4, na.rm = TRUE)
      bottom_mai <- max(strwidth(labels(time), "inch")+2, na.rm = TRUE)
      par(mai =c(bottom_mai,left_mai,1,1))
      plot(1:length(y),rep(2*x$N,length(y)), type = "n", ylab = "", xlab = "time interval", ylim = c(0,2*x$N), xaxt = "n", yaxt = "n",)
      axis(side = 1, at = c(1:length(tab_s)))
      axis(side = 2, at = seq(1,2*x$N,by=2), labels = as.character(actors),las=2)
      abline(h = seq(0,2*x$N,by=2), lty=2, col="gray")
      for(l in 1:length(tab_s)){
        if(!is.null(tab_s[[l]])){
          y_loc <- names(tab_s[[l]])
          y_loc <- sapply(1:length(y_loc),function(x) x + (x-1) )
          scaled_y <-  (as.numeric(tab_s[[l]])-min(as.numeric(tab_s[[l]])))/(max(as.numeric(tab_s[[l]]))-min(as.numeric(tab_s[[l]])))
          if(any(is.nan(scaled_y))) scaled_y[is.nan(scaled_y)] <- 1.0
          points(rep(l,length(tab_s[[l]])),y_loc,type="p",pch=pch.degree,cex=3*scaled_y,col = rgb(red = 80/255, green = 199/255, blue = 199/255, alpha=scaled_y*1))
        }
      }
      title("(normalized) Out-degree per time interval")
      #dev.flush()

      # in-degree plot
      tab_s <- tapply(X =x$edgelist$actor2, INDEX = time, FUN = function(w) table(w))
      #dev.hold()
      plot(1:length(y),rep(2*x$N,length(y)), type = "n", ylab = "", xlab = "time interval", ylim = c(0,2*x$N), xaxt = "n", yaxt = "n",)
      axis(side = 1, at = c(1:length(tab_s)))
      axis(side = 2, at = seq(1,2*x$N,by=2), labels = as.character(actors),las=2)
      abline(h = seq(0,2*x$N,by=2), lty=2, col="gray")
      for(l in 1:length(tab_s)){
        if(!is.null(tab_s[[l]])){
          y_loc <- names(tab_s[[l]])
          y_loc <- sapply(1:length(y_loc),function(x) x + (x-1) )
          scaled_y <- (as.numeric(tab_s[[l]])-min(as.numeric(tab_s[[l]])))/(max(as.numeric(tab_s[[l]]))-min(as.numeric(tab_s[[l]])))
          if(any(is.nan(scaled_y))) scaled_y[is.nan(scaled_y)] <- 1.0
          points(rep(l,length(tab_s[[l]])),y_loc,type="p",pch=pch.degree,cex=3*scaled_y,col = rgb(red = 199/255, green = 121/255, blue = 80/255, alpha=scaled_y*1))
        }
      }
      title("(normalized) In-degree per time interval")
      #dev.flush()
      par(op)
    }
    else{ # for undirected networks
      # total-degree plot
      time_s <- rep(x$edgelist$time,each=2)
      time_s <- cut(time_s,breaks = n_intervals)
      tab_s <- tapply(X =as.vector((t(as.matrix(x$edgelist[,2:3])))), INDEX = time_s, FUN = function(w) table(w))
      #dev.hold()
      par(mfrow=c(1,1))
      left_mai <- max(strwidth(actors, "inch")+0.4, na.rm = TRUE)
      bottom_mai <- max(strwidth(labels(time), "inch")+2, na.rm = TRUE)
      par(mai =c(bottom_mai,left_mai,1,1))
      plot(1:length(y),rep(2*x$N,length(y)), type = "n", ylab = "", xlab = "time interval", ylim = c(0,2*x$N), xaxt = "n", yaxt = "n",)
      axis(side = 1, at = c(1:length(tab_s)))
      axis(side = 2, at = seq(1,2*x$N,by=2), labels = as.character(actors),las=2)
      abline(h = seq(0,2*x$N,by=2), lty=2, col="gray")
      for(l in 1:length(tab_s)){
        if(!is.null(tab_s[[l]])){
          y_loc <- names(tab_s[[l]])
          y_loc <- sapply(1:length(y_loc),function(x) x + (x-1) )
          scaled_y <-  (as.numeric(tab_s[[l]])-min(as.numeric(tab_s[[l]])))/(max(as.numeric(tab_s[[l]]))-min(as.numeric(tab_s[[l]])))
          if(any(is.nan(scaled_y))) scaled_y[is.nan(scaled_y)] <- 1.0
          points(rep(l,length(tab_s[[l]])),y_loc,type="p",pch=pch.degree,cex=3*scaled_y,col = rgb(red = 80/255, green = 199/255, blue = 199/255, alpha=scaled_y*1))
        }
      }
      title("(normalized) Total-degree per time interval")
      #dev.flush()
    }
  }

  # [[plot 4]] plots: quantity per time interval
    ## [1] (# events) per time interval
    ## [2] (proportion of active dyads, as observed dyads / D) per time interval
  # if directed network:
    ## [3] (proportion of active senders, as observed senders / N) per time interval
    ## [4] (proportion of active receivers, as observed receivers / N) per time interval
  if(which[4L]){
    # arranging layout
    layout.matrix <- NULL
    if(.directed){
    layout.matrix <- matrix(c(1,3,2,4), nrow = 2, ncol = 2)
    }
    else{
    layout.matrix <- matrix(c(1, 2), nrow = 1, ncol = 2)
    }
    #dev.hold()
    layout(mat = layout.matrix)
    df_spline <- floor(sqrt(n_intervals))
    # [1] plotting (# events) per time interval
    type_plot <- ifelse(n_intervals>3,"p","b")
    plot(1:length(y),y,type=type_plot,cex=0.8,col="#939393",ylab = "# events",xlab = "time interval", xaxt="n", lwd = 1.5, main="Number of events (# events) per time interval")
    if(n_intervals>3) lines(smooth.spline(x=c(1:length(y)),y=as.numeric(y),df=df_spline),col="#cd0303",lwd=2)

    # [2] plotting (proportion of active dyads) per time interval
    prop_dyads <- tapply(X = dyads, INDEX = time, FUN = function(l) length(unique(l)))
    prop_dyads[is.na(prop_dyads)] <- 0
    prop_dyads <- prop_dyads/x$D
    plot(prop_dyads,type=type_plot,cex=0.8,col="#939393",ylab = "active dyads (observed/total)",xlab = "time interval", xaxt="n", lwd = 1.5, main="Active dyads (observed/total) per time interval")
    if(n_intervals>3) lines(smooth.spline(x=c(1:length(y)),y=as.numeric(prop_dyads),df=df_spline),col="#cd0303",lwd=2)

    if(.directed){
      # [3] plotting (proportion of active senders) per time interval
      s <- tapply(X = x$edgelist$actor1, INDEX = time, FUN = function(l) length(unique(l)))
      s[is.na(s)] <- 0
      s <- s/x$N
      plot(s,type=type_plot,cex=0.8,col="#939393",ylab = "active senders (observed/total)",xlab = "time interval", xaxt="n", lwd = 1.5,main="Active senders (observed/total) per time interval")
      if(n_intervals>3) lines(smooth.spline(x=c(1:length(y)),y=as.numeric(s),df=df_spline),col="#cd0303",lwd=2)
      # [4] plotting (proportion of active receivers) per time interval
      s <- tapply(X = x$edgelist$actor2, INDEX = time, FUN = function(l) length(unique(l)))
      s[is.na(s)] <- 0
      s <- s/x$N
      plot(s,type=type_plot,cex=0.8,col="#939393",ylab = "active receivers (observed/total)",xlab = "time interval", xaxt="n", lwd = 1.5, main="Active receivers (observed/total) per time interval")
      if(n_intervals>3) lines(smooth.spline(x=c(1:length(y)),y=as.numeric(s),df=df_spline),col="#cd0303",lwd=2)
    }
    #dev.flush()
  }


  # [[plot 5]] network visualization as nodes and edges
  if(which[5L]){
    requireNamespace(package="igraph",quietly=TRUE)
    X_dyad[is.na(X_dyad)] <- 0
    # [1] network of undirected dyad intensity
    # defining nodes and links
    popularity_table <- table(factor(c(x$edgelist$actor1,x$edgelist$actor2),levels=actors))
    nodes <- data.frame(name= names(popularity_table),popularity =as.vector(popularity_table)) # popularity = total in- and out-degree
    rm(popularity_table)
    links <- data.frame(from = as.character(X_dyad[,1]),to = as.character(X_dyad[,2]), weight = as.numeric(X_dyad[,3]))
    # creating a graph_from_data_frame
    net_undirected <- igraph::graph_from_data_frame(d=links, vertices=nodes, directed=TRUE)

    # setting up vertex and edge attributes
    popularity_scale <- ((nodes$popularity-min(nodes$popularity))/(max(nodes$popularity)-min(nodes$popularity)))
    popularity_scale[is.na(popularity_scale)] <- 0
    links_weight <- (links$weight-min(links$weight))/(max(links$weight)-min(links$weight))
    links_weight[is.na(links_weight)] <- 0
    igraph::V(net_undirected)$size <- 10
    # defining transparency of vertices color based on their popularity (in-degree)
    transparency_popularity_levels <- sapply(round(255*popularity_scale), function(l) {
    hc <-rgb(0,0,0, alpha = l,maxColorValue = 255)
    substr(x=hc, nchar(hc)-1, nchar(hc))
    })
    igraph::V(net_undirected)$color <- sapply(transparency_popularity_levels,function(l) paste(igraph.vertex.color,l,sep=""))
    igraph::V(net_undirected)$frame.color <- "black"
    igraph::E(net_undirected)$arrow.mode <- 0
    igraph::E(net_undirected)$width <- 2
    # defining transparency of edges color based on count
    transparency_links_weight_levels <- sapply(round(255*links_weight), function(l) {
    hc <-rgb(0,0,0, alpha = l,maxColorValue = 255)
    substr(x=hc, nchar(hc)-1, nchar(hc))
    })
    igraph::E(net_undirected)$color <- sapply(transparency_links_weight_levels,function(l) paste(igraph.edge.color,l,sep=""))
    igraph::E(net_undirected)$curved <- 0.1
    igraph::graph_attr(net_undirected, "layout") <- igraph::layout_with_lgl
    if(.directed){
      X[is.na(X)] <- 0
      # [2] network of directed dyad intensity
      # defining nodes and links
      popularity_table <- table(factor(x$edgelist$actor2,levels=actors))
      nodes <- data.frame(name= names(popularity_table),popularity =as.vector(popularity_table)) # popularity = total in-degree
      rm(popularity_table)
      links <- data.frame(from = as.character(X[,1]),to = as.character(X[,2]), weight = as.numeric(X[,3]))

      # creating a graph_from_data_frame
      net_directed <- igraph::graph_from_data_frame(d=links, vertices=nodes, directed=TRUE)

      # setting up vertex and edge attributes
      popularity_scale <- ((nodes$popularity-min(nodes$popularity))/(max(nodes$popularity)-min(nodes$popularity)))
      links_weight <- (links$weight-min(links$weight))/(max(links$weight)-min(links$weight))
      igraph::V(net_directed)$size <- 10
      # defining transparency of vertices color based on their popularity (in-degree)
      transparency_popularity_levels <- sapply(round(255*popularity_scale), function(l) {
      hc <-rgb(0,0,0, alpha = ifelse(is.na(l),0,l),maxColorValue = 255)
      substr(x=hc, nchar(hc)-1, nchar(hc))
      })
      igraph::V(net_directed)$color <- sapply(transparency_popularity_levels,function(l) paste(igraph.vertex.color,l,sep=""))
      igraph::V(net_directed)$frame.color <- "black"
      igraph::E(net_directed)$arrow.size <- 0.3
      igraph::E(net_directed)$width <- 2
      # defining transparency of edges color based on count
      transparency_links_weight_levels <- sapply(round(255*links_weight), function(l) {
      hc <-rgb(0,0,0, alpha = ifelse(is.na(l),0,l),maxColorValue = 255)
      substr(x=hc, nchar(hc)-1, nchar(hc))
      })
      igraph::E(net_directed)$color <- sapply(transparency_links_weight_levels,function(l) paste(igraph.edge.color,l,sep=""))
      igraph::E(net_directed)$curved <- 0.1
      igraph::graph_attr(net_directed, "layout") <- igraph::layout_with_lgl
      # plotting network
      #dev.hold()
      par(mfrow=c(1,2))
      plot(net_undirected, main = "Network of events (undirected edges)")
      plot(net_directed, main = "Network of events (directed edges)")
      #dev.flush()
    }
    else{
      # plotting network
      #dev.hold()
      par(mfrow=c(1,1))
      plot(net_undirected, main = "Network of events (undirected edges)")
      #dev.flush()
    }
  }
}


#' @title dim.remify
#' @rdname dim.remify
#' @description A function that returns the dimension of the temporal network.
#' @param x a \code{remify} object.
#' @method dim remify
#'
#' @return vector of dimensions of the processed event sequence.
#'
#' @export
#'
#' @examples
#'
#' # processing the random network 'randomREHsmall'
#' library(remify)
#' data(randomREHsmall)
#' reh <- remify(edgelist = randomREHsmall$edgelist,
#'               model = "tie")
#'
#' # dimensions of the processed 'remify' object
#' dim(reh)
#'
dim.remify <- function(x){
  dimensions <- NULL
  if(is.null(x$E)){
    if(!is.null(x$C)){
      dimensions <- c(x$M, x$N, x$C, x$D)
      names(dimensions) <- c("events","actors","types","dyads")
    }
    else{
      dimensions <- c(x$M, x$N, x$D)
      names(dimensions) <- c("events","actors","dyads")
    }
  }else{
    if(!is.null(x$C)){
      dimensions <- c(x$E, x$M, x$N, x$C, x$D)
      names(dimensions) <- c("events","time points","actors","types","dyads")
    }
    else{
      dimensions <- c(x$E, x$M, x$N, x$D)
      names(dimensions) <- c("events","time points","actors","dyads")
    }
  }
  if(x$meta$riskset=="active"){
    dimensions <- c(dimensions, "dyads(active)" = x$activeD)
  }
  return(dimensions)
}

#######################################################################################
#######################################################################################


#######################################################################################
#######################################################################################
###########(END)             Methods for `remify` object             (END)#############
#######################################################################################
#######################################################################################

