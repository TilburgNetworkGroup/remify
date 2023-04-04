
#' @title Transform processed relational event sequences to different formats
#'
#' @description A function that transforms a \code{remify} object into one of the possible formats that suit external packages, and vice versa. The function can convert, at the moment, the data structure from an object of class \code{remify} to a data structure required by the function \code{relevent::rem()} from the \href{https://CRAN.R-project.org/package=relevent}{relevent} package (Butts, C.T. 2023) (and vice versa).
#'
#' @param data an object of either class 'remify' (see function \code{remify::remify()}) or class 'relevent'. The class 'relevent' is an artificial class object that contains a list of objects named after the argument names of the function \code{relevent::rem()}. For instance, if one wants to convert an object of structure 'relevent' we need it to contain: 'eventlist' (mandatory), 'supplist' (optional), 'timing'(mandatory). If the object 'timing' is \code{NULL}, the output object will assume an \code{"interval"} timing. The 'supplist' object can be left uspecified (\code{NULL}). If a 'remify' object is converted to a 'relevent' object, then the output will contain the same argument useful for running the \code{relevent::rem()} function.
#' @param output_format a character indicating the output format which the input data has to be converted to. It can assume two values: "remify" , "relevent"
#'
#' @return  an object of class specified in the \code{output_format} argument 
#' @export
rehshape <- function(data, output_format = c("remify","relevent")){

    output_format <- match.arg(output_format)
    if(!inherits(data,"remify") & !inherits(data,"relevent")){
      stop("'data' must be either a 'remify' object or a (artificial) object of class 'relevent'.")
    }
    # check data and output format
    if(class(data) == output_format){
      stop("'output_format' and class of 'data' must be different.")
    }
    
    # if data structure is 'remify'
    if(inherits(data,"remify")){
      # check remify object here
      ## ##
      ## ## ##
      # stop('') + add tests
      ## ##
      out <- NULL
      if(output_format == "relevent"){
        # (1) processing the edgelist
        eventlist <- matrix(NA,nrow=data$M,ncol=2)
        if(attr(data,"model")=="tie"){  # if remify object is processed for tie-oriented modeling, then we consider the attribute "dyad"
          eventlist[,1] <- attr(data,"dyad")
        }
        else{
          if(attr(data,"with_type")){ # if the remify object is processed for actor-oriented modeling, then we have to find the dyad ID ([[IMPROVEMENT!!]] this step can be run on a Rcpp function with parallelization)
            for(m in 1:data$M) eventlist[m,1] <- getDyadIndex(actor1 = data$edgelist$actor1[m]-1,actor2 = data$edgelist$actor2[m]-1,type = data$edgelist$type[m]-1,N = data$N, directed = attr(data,"directed"))
          }
          else{
            for(m in 1:data$M) eventlist[m,1] <- getDyadIndex(actor1 = data$edgelist$actor1[m]-1,actor2 = data$edgelist$actor2[m]-1,type = 0,N = data$N, directed = attr(data,"directed"))
          }
        }
        eventlist[,2] <- as.numeric(data$edgelist$time)
        colnames(eventlist) <- c("dyad","time")
        supplist <- NULL
        if(!is.null(data$omit_dyad)){
          # (2) converting the omit_dyad output object to the 'supplist' argument in relevent::rem()
          supplist <- matrix(TRUE,nrow=data$M,ncol=data$D)
          for(m in 1:data$M){
            if(data$omit_dyad$time[m]!=(-1)){
              change_m <- data$omit_dyad$riskset[data$omit_dyad$time[m]+1,]
              supplist[m,] <- as.logical(change_m)
              rm(change_m)
            }
          }
        }
        # (3) processing information about likelihood
        timing <- ifelse(attr(data,"ordinal"),"ordinal","interval")
        out <- structure(list(eventlist = eventlist,
                              supplist = supplist,
                              timing = timing),
                        class = "relevent")
      }
      return(out)
    }


    if(inherits(data,"relevent")){

      # check relevent object here
      ## ##
      ## ## ##
      # stop('') + add tests
      ## ##

      out <- NULL
      #convert from 'relevent' structure to 'remify'

      # full riskset (this will have different actors' names than the original data)
      dyads_l <- expand.grid(1:data$N,1:data$N) # number of actors 
      dyads_l <- dyads_l[-which(dyads_l[,1]==dyads_l[,2]),]
      dyads_l <- dyads_l[,c(2,1)] 
      dyads_l <- data.frame(actor1=rep(dyads_l[,1],data$C),actor2=rep(dyads_l[,2],data$C),type=rep(1:data$C,each=data$N*(data$N-1))) # number of event types

      # edgelist converted to [actor1,actor2,type]
      data$M <- dim(data$eventlist)[1]
      edgelist_orig <- data.frame(time = data$eventlist[,2], actor1 = rep(NA,data$M), actor2 = rep(NA,data$M), type = rep(NA,data$M))
      for(m in 1:data$M){ # [[IMPROVEMENT!!]] parallelization
        edgelist_orig[m,2:4] <- dyads_l[data$eventlist[m,1],]
      }

      # convert supplist to omit_dyad (it remains NULL if supplist is NULL or has full riskset over all the time points)
      converted_omit_dyad <- NULL
      if(!is.null(data$supplist)){
        check_changing_riskset <- sum(data$supplist)
        if(check_changing_riskset < (data$M*data$N*(data$N-1)*data$C)){
          converted_omit_dyad <- list()
          converted_omit_dyad$riskset <- (rbind(unique(data$supplist)[-1,]))*1 # this operation can be faster at rcpp level
          converted_omit_dyad$time <- rep(-1,data$M)
          for(m in 1:data$M){ # [[IMPROVEMENT!!]] parallelization
            if(sum(data$supplist[m,]) < (data$N*(data$N-1)*data$C)){ #if there is a change in the riskset, we need to assign which row (in c++ notation it is in the riskset object matrix)
            temp_mat <-  matrix(rep(data$supplist[m,],dim(converted_omit_dyad$riskset)[1]),nrow=dim(converted_omit_dyad$riskset)[1],byrow=TRUE)
            find_loc <- apply(converted_omit_dyad$riskset - temp_mat,1,sum)
            converted_omit_dyad$time[m] <- (which(find_loc==0)-1)
            }
          }
        }
      }

      # create 'remify' class object
      out <- remify::remify(edgelist = edgelist_orig,
                          actors = as.character(1:data$N),
                          types = as.character(1:data$C), 
                          directed = TRUE, 
                          ordinal = FALSE, 
                          origin = 0,
                          omit_dyad = NULL, # set to NULL but added later
                          model = "tie")
      # we have to reorder the columns of converted_omit_dyad
      dict_loc <- attr(out,"dictionary")
      position_rearranged <- NULL
      for(d in 1:dim(dyads_l)[1]){
        sender_old <- dyads_l$actor1[d]
        receiver_old <- dyads_l$actor2[d]
        type_old <- dyads_l$type[d]
        
        sender_new <- as.numeric(dict_loc$actors$actorName[which(dict_loc$actors$actorID == sender_old)])-1
        receiver_new <- as.numeric(dict_loc$actors$actorName[which(dict_loc$actors$actorID == receiver_old)])-1
        type_new <- as.numeric(dict_loc$types$typeName[which(dict_loc$types$typeID == type_old)])-1

        position_new <- getDyadIndex(actor1=sender_new,actor2=receiver_new,type=type_new,N=out$N,directed=attr(out,"directed"))+1 
        position_rearranged <- c(position_rearranged,position_new)
      }
      converted_omit_dyad$riskset <- converted_omit_dyad$riskset[,position_rearranged]
      out$omit_dyad <- converted_omit_dyad
      return(out)
    }
         
}                   








