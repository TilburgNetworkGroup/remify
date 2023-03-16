
#' @title Transform processed relational event sequences to different formats
#'
#' @description A function that transforms a \code{reh} object into one of the possible formats that suit external packages, and vice versa. The function can convert, at the moment, the data structure from (to) an object of class \code{reh} to (from) a data structure required by the function \code{relevent::rem()} from the \href{https://CRAN.R-project.org/package=relevent}{relevent} package (Butts, C.T. 2023).
#'
#' @param data an object of either class 'reh' (see function \code{remify::reh()}) or class 'relevent'. The class 'relevent' is an dummy class object that contains a list of objects named after the argument names of the function \code{relvent::rem()} that need to be converted. For instance, if one wants to convert an object of structure 'relevent' we need it to contain: 'eventlist' (mandatory), 'supplist' (optional), 'timing'(mandatory). If the object 'timing' is \code{NULL}, the output object will assume an \code{"interval"} timing. The 'supplist' object can be left uspecified (\code{NULL}).
#' @param output_format a character indicating the output format which the input data has to be converted to. It can assume two values: "reh" , "relevent"
#'
#' @return  an object of class specified by the \code{format} argument and containing the converted objects according to the required format
#' @export
rehshape <- function(data, output_format = c("reh","relevent")){

    output_format <- match.arg(output_format)
    data_format <- class(data)
    if(length(data_format)>1){
      stop("class of input data must be of length 1.")
    }
    # data has to be either of class 'reh' or 'relevent'
    if(!any(data_format == c("reh","relevent"))){
      stop("class of input data must be either `reh` or `relevent`.") 
    }
    # check data and output format
    if(data_format == output_format){
      warning("the format of the input data is the same as the required output format. The input data is returned.")
      return(data)
    }
    
    # if data structure is 'reh'
    if(data_format ==  "reh"){

      # check reh object here
      ## ##
      ## ## ##
      # stop('') + add tests
      ## ##

      out <- NULL
      if(output_format == "relevent"){
        # (1) processing the edgelist
        eventlist <- data$edgelist[,c(2,1)] # [dyad,time]
        eventlist[,1] <- eventlist[,1]+1
        if(is.null(attr(data,"time")$origin)){
          eventlist[,2] <- attr(data,"time")$value[,1] # if 'origin' is NULL the we use the time column,
        }
        else{
          eventlist[,2] <-cumsum(data$intereventTime) # if 'origin' is provided inside object 'reh', then the time variable is reconstructed via cumulative sum of intervent time variable 
        }
    
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


    if(data_format == "relevent"){

      # check relevent object here
      ## ##
      ## ## ##
      # stop('') + add tests
      ## ##

      out <- NULL
      #convert from 'relevent' structure to 'reh'

      # full riskset (this will have different actors' names than the original data)
      dyads_l <- expand.grid(1:data$N,1:data$N) # number of actors 
      dyads_l <- dyads_l[-which(dyads_l[,1]==dyads_l[,2]),]
      dyads_l <- dyads_l[,c(2,1)] 
      dyads_l <- data.frame(actor1=rep(dyads_l[,1],data$C),actor2=rep(dyads_l[,2],data$C),type=rep(1:data$C,each=data$N*(data$N-1))) # number of event types

      # edgelist converted to [actor1,actor2,type]
      data$M <- dim(data$eventlist)[1]
      edgelist_orig <- data.frame(time = data$eventlist[,2], actor1 = rep(NA,data$M), actor2 = rep(NA,data$M), type = rep(NA,data$M))
      for(m in 1:data$M){
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
          for(m in 1:data$M){
            if(sum(data$supplist[m,]) < (data$N*(data$N-1)*data$C)){ #if there is a change in the riskset, we need to assign which row (in c++ notation it is in the riskset object matrix)
            temp_mat <-  matrix(rep(data$supplist[m,],dim(converted_omit_dyad$riskset)[1]),nrow=dim(converted_omit_dyad$riskset)[1],byrow=TRUE)
            find_loc <- apply(converted_omit_dyad$riskset - temp_mat,1,sum)
            converted_omit_dyad$time[m] <- (which(find_loc==0)-1)
            }
          }
        }
      }

      # create 'reh' class object
      out <- remify::reh(edgelist = edgelist_orig,
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
        sender_old <- dyads_l$actor1[d]-1
        receiver_old <- dyads_l$actor2[d]-1 
        type_old <- dyads_l$type[d]-1
        
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








