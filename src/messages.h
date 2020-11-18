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
                  message = "Hello Toronto";
                  break;
            
            case 1:
                  message = "Hello Canada";
                  break;
            
      }
      return message;
}


//' errorMessage
std::string errorMessage(int cond){
      std::string message = "undefiend";
      switch(cond){
            case 0:
                  message = "Time variable is not sorted or there are at least two events with the same time value.";
                  break;
            
            case 1:
                  message = "Hello Kyoto";
                  break;
      }
      return message;
}

#endif