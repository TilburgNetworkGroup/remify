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
//'
//' @param cond  it is the warning number
//'
// [[Rcpp::export]]
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
//'
//' @param cond  it is the error number
//'
// [[Rcpp::export]]
std::string errorMessage(int cond){
      std::string message = "undefiend";
      switch(cond){
            case 0:
                  message = "Hello Holland";
                  break;
            
            case 1:
                  message = "Hello Kyoto";
                  break;
      }
      return message;
}

#endif