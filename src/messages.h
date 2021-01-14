#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <iostream>
#include <typeinfo>
#include <map>
#include <iterator>
#include <string>


#ifndef MESSAGES_H
#define MESSAGES_H


//' askYesNoQuestion
int askYesNoQuestion(std::string message) {
        Rcpp::Environment base = Rcpp::Environment("package:base");
        Rcpp::Function readline = base["readline"];
        Rcpp::Function as_numeric = base["as.numeric"];
        int cond = 10;
        while((cond != 0) & (cond != 1)){
        Rcpp::Rcout << message;
        cond = Rcpp::as<int>(as_numeric(readline("> ")));
        }
      return cond;
}

//' warningMessage
std::string warningMessage(int cond){
      std::string message = "undefined";
      switch(cond){
            case 0:
                  message = "\033[1;33m Warning: the `time` variable is not sorted. Sorting will be forced, continue? \n\n 1 --> Yes \n 0 --> No \033[0m\n";
                  break;
            case 1:
                  message = "\033[1;33m Warning: at least two events (or more) occurred at the same time point. The interevent time of such events will be evenly spaced, continue? \n\n 1 --> Yes \n 0 --> No \033[0m\n";
                  break;
            case 2:
                  message = "\033[1;33m Warning: both `origin` and first time point have the same value. `origin` is then automatically set either to one day/second before the first time point or to 0. \033[0m\n";
                  break;
            case 3:
                  message = "\033[1;33m Warning: one or more time points supplied in `omit_dyad` were not found in the edgelist. Therefore they were removed. \033[0m\n";
                  break;
            case 4:
                  message = "\033[1;33m Warning: one or more actors/types supplied in `omit_dyad` were not found in the edgelist. Therefore the corresponding rows defined in the data.frame `dyad` were removed. \033[0m\n";
                  break;            
      }
      return message;
}


//' errorMessage
std::string errorMessage(int cond){
      std::string message = "undefiend";
      switch(cond){
            case 0:
                  message = "NA found in at least one of the element of the `riskset` object. Dyads to remove from the riskset cannot";
                  break;
            case 1:
                  message = "Error : the class of `time` object is not one of the following: integer, numeric, Date, POSIXct";
                  break;
      }
      return message;
}

#endif