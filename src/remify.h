// [[Rcpp::interfaces(r,cpp)]]

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <iostream>
#include <typeinfo>
#include <iterator>
#include <string>

#ifndef REMIFY_H
#define REMIFY_H


// title getDyadIndex
//
// param actor1 id of actor1 from 0 to N-1
// param actor2 id of actor2 from 0 to N-1
// param type id of event type from 0 to C-1
// param N number of actors
// param directed bool FALSE/TRUE if the networks is directed (TRUE) or not (FALSE)
//
// return dyad index according to the combination of id's of actor1/actor2/type
// [[Rcpp::export]]
int getDyadIndex(double actor1, double actor2, double type, int N, bool directed) {

    int dyad = -999; // returning impossible index if the dyad is a self-edge (i.e., sender and receiver are the same actor)
    if(actor1 != actor2){
        if(!directed){ // when directed == FALSE we sort actor1 and actor2
            if(actor1 < actor2){
                double dyad_loc = (N*(N-1)/2)*type+(N-1)*actor1+actor2-actor1-1-(actor1*actor1)/2;
                if(actor1>0){
                    dyad_loc += actor1/2;
                }
                dyad = dyad_loc;
            }
            else{
                double dyad_loc = (N*(N-1)/2)*type+(N-1)*actor2+actor1-actor2-1-(actor2*actor2)/2;
                if(actor2>0){
                    dyad_loc += actor2/2;
                }
                dyad = dyad_loc;
            }
        }
        else{ 
            // when directed == TRUE (we do not sort) (actor1 = sender, actor2 = receiver)
            double dyad_loc = N*(N-1)*type+(N-1)*actor1+actor2;
            if(actor2>actor1){
                dyad_loc -= 1;
            }
            dyad = dyad_loc;
        }
    }

    return dyad;
}

// title getDyadComposition (only for directed networks)
//
// param d id of the dyad
// param C number of event types
// param N number of actors
// param D number of dyads
//
// return dyad index according to the combination of id's of actor1/actor2/type
// [[Rcpp::export]]
Rcpp::IntegerVector getDyadComposition(int d, int C, int N, int D) {
  Rcpp::IntegerVector composition(3);
  // Note :
  // (1) this function assumes that all the possible dyads are in the stats object
  // (2) this function is not coded to account for reduced (that omits dyads) arrays of stats
  // (3) this function works only for directed netwroks
  double r = d; // this will be finally the receiver
  r += 1;
  int sender,receiver,type = -999;
  double c = 1, s = 1;
  while(c<=C){
    if((r/D)<=(c/C)){
      type = (c-1);
      break;
    }
    c += 1;
  }

  //if(type == (-999)){
  //  Rcpp::Rcout << "error \n"; //errorMessage(0); //adjust error message
  //}

  r -= N*(N-1)*type;

  while(s<=N){
    if((r/(N*(N-1)))<=(s/N)){
      sender = (s-1);
      break;
    }
    s += 1;
  }

  //if(sender == (-999)){
  //  Rcpp::Rcout << "error \n"; //errorMessage(0); //adjust error message
  //}

  arma::mat receiver_vec(N,1);
  receiver_vec.col(0) = arma::linspace(0,N-1,N);
  receiver_vec.shed_row(sender);
  r -= (N-1)*sender;
  receiver = receiver_vec[r-1]; // if either type or sender are not found, the function will stop earlier
  composition = {sender,receiver,type};
  return composition;
}

#endif 
