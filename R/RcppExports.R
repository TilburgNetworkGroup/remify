# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' @title rehCpp (the Rcpp alias of \code{reh()})
#'
#' @details more details can be found at the following documentation: \link[remify]{reh}.
#' 
#' @param edgelist an object of class \code{"\link[base]{data.frame}"} or 
#' \code{"\link[base]{matrix}"} characterizing the relational event history sorted by 
#' time with columns 'time', 'actor1', 'actor2' and optionally 'type' and 
#' 'weight'.  
#' @param actors vector of actors that may be observed interacting in the network. If \code{NULL}, actor names will be drawn from the input edgelist.
#' @param types vector of event types that may occur in the network. If \code{NULL}, type names will be drawn from the input edgelist.
#' @param directed logical value indicating whether dyadic events are directed (\code{TRUE}) or undirected (\code{FALSE}).
#' @param ordinal  logical value indicating whether only the order of events matters in the model (\code{TRUE}) or also the waiting time must be considered in the model (\code{FALSE}).
#' @param origin time point since which when events could occur (default is \code{NULL}). If it is defined, it must have the same class of the time column in the input edgelist.
#' @param omit_dyad list of lists of two elements: `time`, that is a vector of the time points which to omit dyads from, `dyad`, which is a \code{"\link[base]{data.frame}"} where dyads to be omitted are supplied.
#'
#' @return list of objects with processed raw data.
#'
#' @export
rehCpp <- function(edgelist, actors, types, directed, ordinal, origin, omit_dyad) {
    .Call('_remify_rehCpp', PACKAGE = 'remify', edgelist, actors, types, directed, ordinal, origin, omit_dyad)
}

