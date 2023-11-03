
#' @title Transform processed remify objects to different formats
#'
#' @description A function that transforms a \code{remify} object into one of the possible formats that suit external packages. The function can convert, at the moment, the data structure from an object of class \code{remify} to a data structure required by the function \code{relevent::rem()} or by the function \code{relevent::rem.dyad()} from the '\href{https://CRAN.R-project.org/package=relevent}{relevent}' package (Butts, C.T. 2023).
#'
#' @param data an object of class 'remify' (see function \code{remify::remify()}).
#' @param output_format a character indicating the output format which the input data has to be converted to. It can assume two values: \code{"relevent-rem"} , \code{"relevent-rem.dyad"}. Default value is \code{"relevent-rem"}.
#' @param ncores number of cores used to parallelize internal algorithms
#' @param optional_arguments vector of arguments names from relevent::rem or relevent::rem.dyad() that the user might want to process and have in the output object of rehshape (e.g., the pre-computed structures required by relevent::rem.dyad) 
#' 
#' 
#' @return  an object of class specified in the \code{output_format} argument. The output class object 'relevent-rem' contains a list of objects named after the arguments of the function \code{relevent::rem()}: 'eventlist' (mandatory), 'supplist' (optional), 'timing'(mandatory). The output class object 'relevent-rem.dyad' contains a list of objects named after the arguments of the function \code{relevent::rem.dyad()}: 'edgelist' (mandatory), 'n' (mandatory), 'ordinal'(optional).
#' @export
#' 
#' @examples 
#' 
#' # processing the random network 'randomREH'
#' library(remify)
#' data(randomREH)
#' reh <- remify(edgelist = randomREH$edgelist,
#'               model = "tie",
#'               riskset = "manual",
#'               omit_dyad = randomREH$omit_dyad)
#' 
#' # convert 'remify' object to output_format = "relevent-rem"
#' relevent_rem_obj <- rehshape(data = reh, output_format = "relevent-rem")
#' 
#' str(relevent_rem_obj) 
#' 
#' # convert 'remify' object to output_format = "relevent-rem.dyad"
#' relevent_rem.dyad_obj <- rehshape(data = reh, output_format = "relevent-rem.dyad")
#' 
#' summary(relevent_rem.dyad_obj)
#' 
rehshape <- function(data, 
                      output_format = c("relevent-rem","relevent-rem.dyad"),
                      ncores = 1L,
                      optional_arguments = NULL){

  output_format  <- match.arg(arg = output_format, choices = c("relevent-rem","relevent-rem.dyad"), several.ok = FALSE)
  
  if(!inherits(data,"remify")){
    stop("'data' must be a 'remify' object.")
  }

  # ... ncores
  if(is.null(ncores)) ncores <- 1L
  else if(((parallel::detectCores() > 2L) & (ncores > floor(parallel::detectCores()-2L))) | ((parallel::detectCores() == 2L) & (ncores > 1L))){
          stop("'ncores' is recommended to be set at most to: floor(parallel::detectCores()-2L)")
  }

  # ... converting from REMIFY to other structures ('relevent', 'goldfish', etc.)
  # check remify input object here
  ## ##
  ## ## ##
  # stop('') + add tests
  ## ##

  # ... to "relevent-rem"
  if(output_format == "relevent-rem"){
    # (3) processing information about likelihood
    timing <- ifelse(attr(data,"ordinal"),"ordinal","interval")
    dyad <- if(attr(data,"model") == "tie") attr(data,"dyadID") else c(0) 
    type <- if(attr(data,"with_type"))  attr(data,"typeID")-1 else c(0)
    out <- remify2relventrem(actor1 = attr(data,"actor1ID")-1,
                          actor2 = attr(data,"actor2ID")-1,
                          type = type,
                          dyad = dyad,
                          M = data$M,
                          N = data$N,
                          D = data$D,
                          with_type = attr(data,"with_type"),
                          directed = attr(data,"directed"),
                          model = attr(data,"model"),
                          omit_dyad = data$omit_dyad,
                          ncores = ncores) # The parallelization is run for remify objects processed for actor-oriented modeling (model = "actor")
    rm(type,dyad)                     
    out$eventlist$time <- data$edgelist$time
    out <- structure(list(eventlist = out$eventlist,
                            supplist = apply(out$supplist,2,as.logical),
                            timing = timing),
                      class = "relevent-rem")
    return(out)
  }

  # ... to "relevent-rem"
  if(output_format == "relevent-rem.dyad"){
    # process optional_arguments here (for the pre-computation of optional objects required by relevent::rem.dyad())
    out <- structure(list(edgelist = data$edgelist[,1:3],
                          n = data$N, 
                          ordinal = attr(data,"ordinal")),
                          class = "relevent-rem.dyad")
    return(out)
  }
}                   








