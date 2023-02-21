#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <iostream>
#include <typeinfo>
#include <map>
#include <iterator>
#include <string>


#ifndef MESSAGES_H
#define MESSAGES_H

//' warningMessage
std::string warningMessage(int cond){
      std::string message = "undefined";
      switch(cond){
            case 0:
                  message = "Warning: the `time` variable is not sorted. Sorting will be forced.";
                  break;
            case 1:
                  message = "Warning: at least two events (or more) occurred at the same time point. The interevent time of such events will be evenly spaced.";
                  break;
            case 2:
                  message = "Warning: value supplied as `origin` is greater or equal than the first time point. `origin` is then automatically set either to one day/second before the first time point or to 0.";
                  break;
            case 3:
                  message = "Warning: one or more actors/types supplied in `omit_dyad` were not found in the edgelist. Therefore the corresponding rows defined in the data.frame `dyad` were removed.";
                  break;            
      }
      return message;
}


//' errorMessage
std::string errorMessage(int cond){
      std::string message = "undefiend";
      switch(cond){
            case 0:
                  message = "time vector in each element of the list 'omit_dyad' must be sorted so that elements indicate respectively start and stop time when the riskset changed";
                  break;
            case 1:
                  message = "self-events are not yet supported";
                  break; 
            case 2:
                  message = "Error: time vector in each element of the list 'omit_dyad' must be of length 2: start and stop time when the riskset changed";
                  break;
            case 3:
                  message = "either start or stop in one of the elements in the list 'omit_dyad' are not found in the edgelist. Please, provide observed time points as start and stop values";
                  break;
            case 4:
                  message = "Error: actor-oriented model can only work with directed networks";
                  break;            
            case 5:
                  message = "Error: time variable can't be negative";
                  break;
      }
      return message;
}

#endif