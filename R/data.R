#' Random Relational Event History
#'
#' A randomly generated sequence of relational events with 20 actors and 9,915 events. Each event type is associated to one of the three following sentiments: \emph{conflict}, \emph{competition} and \emph{cooperation}.
#'
#'
#' @name randomREH
#' @docType data
#'
#' @usage data(randomREH)
#'
#' @format \code{data(randomREH)} will load the following objects into the global environment:
#'\describe{
#'    \item{\code{edgelist}}{a \code{data.frame} that contains the random sequence of events. Columns of the edgelist are:
#'            \itemize{
#'                \item{}{\code{time}: the timestamp indicating the time at which each event occurred;}
#'                \item{}{\code{actor1}: the name of the actor that generated the relational event;}
#'                \item{}{\code{actor2}: the name of the actor that received the relational event;}
#'                \item{}{\code{type}: the type of the relational event.}
#'            }
#'     }
#'    \item{\code{actors}}{names of actors interacting in the dynamic network.}
#'    \item{\code{types}}{names of event types observed in the network and describing the sentiment of the interaction (\emph{conflict}, \emph{competition} and \emph{cooperation}).}
#'    \item{\code{origin}}{starting time point (\code{t_0}) prior to the first observed event (\code{t_1}), the class of this object must be the same as the one of the time column in the edgelist.}
#'    \item{\code{omit_dyad}}{a list where each element describes an alteration of the riskset which takes place at specific time points and for certain actors and/or types.
#'     }
#'}
#'
#'
#' @examples
#' data(randomREH)
#' 
#' # actors names
#' actors
#' 
#' # types names
#' types
#'
#' # look into the first modification of the riskset: omit_dyad[[1]]
#' ## the data.frame `dyad` specifies which dyads will be omitted from the riskset 
#' ## (all the dyads that expressed a `conflict` between actor won't be part of the riskset):
#' omit_dyad[[1]]$dyad 
#' 
#' ## the vector `time` specifies the time points when this exclusion takes place 
#' head(omit_dyad[[1]]$time) # (printing out only the first 10 time points)
#' 
#' # run the preprocessing function reh() by supplying the loaded objects.
#' edgelist_reh <- reh(edgelist = edgelist,
#'                     actors = actors,
#'                     types = types, 
#'                     directed = TRUE,
#'                     ordinal = FALSE,
#'                     origin = origin,
#'                     omit_dyad = omit_dyad)
#'
#' # `edgelist_reh` is an object of class `reh`
#' class(edgelist_reh)
#' 
#' # names of objects inside `edgelist_reh`
#' names(edgelist_reh)
#'
#' @seealso
#'
NULL