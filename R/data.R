#' Random Relational Event History
#'
#' A randomly generated sequence of relational events with 20 actors and 9915 events. Each event type is associated to one of the three following sentiments: \emph{conflict}, \emph{competition} and \emph{cooperation}.
#'
#'
#' @name randomREH
#' @docType data
#'
#'
#' @format \code{data(randomREH)} will load a list containing following objects:
#'\describe{
#'    \item{\code{edgelist}}{a \code{data.frame} that contains the random sequence of events. Columns of the edgelist are:
#'            \describe{
#'                \item{\code{time}}{the timestamp indicating the time at which each event occurred;}
#'                \item{\code{actor1}}{the name of the actor that generated the relational event;}
#'                \item{\code{actor2}}{the name of the actor that received the relational event;}
#'                \item{\code{type}}{the type of the relational event.}
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
#' randomREH$actors
#'
#' # types names
#' randomREH$types
#'
#' # run the preprocessing function reh() by supplying the loaded objects.
#' edgelist_reh <- remify(edgelist = randomREH$edgelist,
#'                     actors = randomREH$actors,
#'                     directed = TRUE,
#'                     ordinal = FALSE,
#'                     origin = randomREH$origin,
#'                     model = "tie")
#'
#' # `edgelist_reh` is an object of class `remify`
#' class(edgelist_reh)
#'
#' # names of objects inside `edgelist_reh`
#' names(edgelist_reh)
#'
"randomREH"


#' Random Relational Event History (small)
#'
#' A subset from the randomly generated sequence of relational events `randomREH`, with 5 actors and 586 events (without event types).
#'
#'
#' @name randomREHsmall
#' @docType data
#'
#'
#' @format \code{data(randomREHsmall)} will load a list containing following objects:
#'\describe{
#'    \item{\code{edgelist}}{a \code{data.frame} that contains the random sequence of events. Columns of the edgelist are:
#'            \describe{
#'                \item{\code{time}}{the timestamp indicating the time at which each event occurred;}
#'                \item{\code{actor1}}{the name of the actor that generated the relational event;}
#'                \item{\code{actor2}}{the name of the actor that received the relational event;}
#'            }
#'     }
#'    \item{\code{actors}}{names of actors interacting in the dynamic network.}
#'    \item{\code{origin}}{starting time point (\code{t_0}) prior to the first observed event (\code{t_1}), the class of this object must be the same as the one of the time column in the edgelist.}
#'    \item{\code{omit_dyad}}{a list where each element describes an alteration of the riskset which takes place at specific time points and for certain actors and/or types.
#'     }
#'}
#'
#'
#' @examples
#' data(randomREHsmall)
#'
#' # actors names
#' randomREHsmall$actors
#'
#' # types names
#' randomREHsmall$types
#'
#'
#' # run the preprocessing function reh() by supplying the loaded objects.
#' small_edgelist_reh <- remify(edgelist = randomREHsmall$edgelist,
#'                     actors = randomREHsmall$actors,
#'                     directed = TRUE,
#'                     ordinal = FALSE,
#'                     origin = randomREHsmall$origin,
#'                     model = "tie")
#'
#' # `small_edgelist_reh` is an object of class `reh`
#' class(small_edgelist_reh)
#'
#' # names of objects inside `small_edgelist_reh`
#' names(small_edgelist_reh)
#'
"randomREHsmall"



#' History dataset
#'
#' A relational event history dataset for testing and examples.
#'
#' @format A data frame that can be used for an analysis with remify, remstats, remstimate
#' @source remstats package
"history"
