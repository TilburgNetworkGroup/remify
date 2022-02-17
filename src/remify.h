#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <iostream>
#include <typeinfo>
#include <iterator>
#include <string>


#ifndef REMIFY_H
#define REMIFY_H


//' @title getDyadIndex
//'
//' @param actor1 id of actor1 from 0 to N-1
//' @param actor2 id of actor2 from 0 to N-1
//' @param type id of event type from 0 to C-1
//' @param N number of actors
//' @param directed bool FALSE/TRUE if the networks is directed (TRUE) or not (FALSE)
//'
//' @return dyad index according to the combination of id's of actor1/actor2/type
// [[Rcpp::interfaces(r,cpp)]]
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

#endif