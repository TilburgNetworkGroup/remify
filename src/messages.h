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
                  message = "Warning: both `origin` and first time point have the same value. `origin` is then automatically set either to one day/second before the first time point or to 0.";
                  break;
            case 3:
                  message = "Warning: one or more time points supplied in `omit_dyad` were not found in the edgelist. Therefore they were removed.";
                  break;
            case 4:
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
                  message = "Error: NA found in at least one of the element of the `riskset` object. Dyads to remove from the riskset cannot";
                  break;
            case 1:
                  message = "Error : the class of `time` object is not one of the following: integer, numeric, Date, POSIXct";
                  break;
            case 2:
                  message = "Error: time vector in each element of the list 'omit_dyad' must be of length 2: start and stop time when the riskset changed";
                  break;
            case 3:
                  message = "Error: either start or stop in one of the elements in the list 'omit_dyad' are not found in the edgelist. Please, provide observed time points as start and stop values";
                  break;
            case 4:
                  message = "Error: actor-oriented model can only work with directed networks";
                  break;            
      }
      return message;
}

#endif