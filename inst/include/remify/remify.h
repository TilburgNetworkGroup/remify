
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <typeinfo>
#include <iterator>

#ifndef REMIFY_H
#define REMIFY_H

namespace remify{
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



        arma::ivec getDyadComposition(int d, int N, bool directed) {

        // Note on the internal routine getDyadComposition:
        // (1) this function assumes that all the possible dyads are in the stats object
        // (2) this function is not coded to account for reduced (that omits dyads) arrays of stats
        // (3) this function works (at the moment) only with directed netwroks
        arma::ivec composition(3);
        arma::mat actors_id(N,1);
        actors_id.col(0) = arma::linspace(0,N-1,N);
        int actor1,actor2,type;
        if(directed){ // if the dyadic events are directed
            // 1. get event type
            type = d/(N*(N-1)); //floor
            // 2. get actor1_actor2 with type = 0
            int dyad_notype = d - (type*N*(N-1)); // dyad ID that ranges between 0 and N*(N-1)
            // 3. get actor1
            actor1 = dyad_notype/(N-1.0); //floor // actor ID that ranges between 0 and N-1
            // 4. get actor2
            int which_actor2 = dyad_notype-(actor1)*(N-1.0);
            actors_id.shed_row(actor1);
            actor2 = actors_id[which_actor2];
            // save dyad composition
            composition = {actor1,actor2,type};
        }
        else{ // if the dyadic events are not directed (therefore the order of the actors involved in a dyadic event is actor1>actor2)
            //[... code here ...]
        }

        return composition;

        // Add errors and checks? No, because:
        // This function is not intended to be used by the final user.
        // Adding further checks (warnings or errors on number of event types, C, or maximum number of dyads, D) 
        // would cause an increase of the computational time required by the C++ algorithms in which
        // the function getDyadComposition() is called multiple times
        }

    }

#endif 
