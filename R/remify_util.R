


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

