#' reh  
#'
#' A function that returns a 'reh' S3 object 
#'
#' @param edgelist a a dataframe of relational events sorted by time: [time,sender,receiver,type,weight]
#' @param riskset is a list of length equal to the number of events, each object a matrix with unobserved dyads (using actors string names)
#' @param covariates list of covariates to be provided according to the input structure working with 'remstats'
#'
#' @return  object list (the function saves also the output of optim)
#' @export

reh <- function(edgelist,
                riskset = replicate(NROW(edgelist),NaN,simplify=FALSE), 
                covariates = list(a = NULL)){

    out <- rehCpp(edgelist = edgelist, riskset = riskset, covariates = covariates)

    # possibly these won't be returned anymore
    out$old_edgelist <- edgelist
    out$old_riskset <- riskset
    out$old_covariates <- covariates
    
    class(out) <- "reh"
    return(out)
}

#' remify 
#'
#' A function that transform the REH input in one of the possible REH formats.
#'
#' @param input
#'
#' @return  object list (the function saves also the output of optim)
#' @export

remify <- function(input){
# [...] do stuff here
                }                   