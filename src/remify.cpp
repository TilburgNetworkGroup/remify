#include <RcppArmadillo.h>
#include <typeinfo>
#include <iterator>
#include <string>
#include <vector>
#include <numeric>
#include <algorithm>
#include <functional>
#include "messages.h"
#include "../inst/include/remify/remify.h"

#ifdef _OPENMP
#include <omp.h>
#endif

// /////////////////////////////////////////////////////////////////////////////////
// //////////(BEGIN)             remify C++ functions              (BEGIN)//////////
// /////////////////////////////////////////////////////////////////////////////////

// @title rearrangeDataFrame
//
// @details function that rearrange a data.frame according to the input index (vector of integers)
//
// @param x \code{data.frame} object to reorder
// @param index vector with the new order
//
// @return \code{data.frame} whose columns are rearranged according to the input index
Rcpp::DataFrame rearrangeDataFrame(Rcpp::DataFrame x, arma::uvec index) {
    int j,m; 
    for(j = 0; j < x.size(); j++){
        Rcpp::RObject x_j = x[j];
        switch (TYPEOF(x_j))
        {  
        //case INTSXP:{
        //    Rcpp::IntegerVector column_j = Rcpp::as<Rcpp::IntegerVector>(x_j);
        //    Rcpp::IntegerVector column_j_loc = Rcpp::clone(column_j);
        //    for(m = 0; m < x.nrows(); m++){
       //         arma::uword m_new = index(m); 
       //         column_j[m] = column_j_loc[m_new];
        //    }
       //     break;}
        case REALSXP:{            
            Rcpp::NumericVector column_j = Rcpp::as<Rcpp::NumericVector>(x_j);
            Rcpp::NumericVector column_j_loc = Rcpp::clone(column_j);
            for(m = 0; m < x.nrows(); m++){
                arma::uword m_new = index(m); 
                column_j[m] = column_j_loc[m_new];
            }
            break;}
        case STRSXP:{
            Rcpp::StringVector column_j = Rcpp::as<Rcpp::StringVector>(x_j);
            Rcpp::StringVector column_j_loc = Rcpp::clone(column_j);
            for(m = 0; m < x.nrows(); m++){
                arma::uword m_new = index(m); 
                column_j[m] = column_j_loc[m_new];
            }
            break;}
        //default: {
        //    break;} //neither an INTSXP nor a REALSXP nor a STRSXP. 
        }
    }
    return x;
}


// @title getOmitDyadActiveRiskSet
//
// @details function that returns a boolean vector of true/false describing events at risk/not at riks an an "active" risk set
//
// @param model string "tie" or "actor" (tie-oriented modeling or actor-oriented modeling)
// @param actor1 vector of actor1 names observed per event
// @param actor2 vector of actor2 names observed per event
// @param type vector of event types (optional)
// @param D number of dyads
// @param N number of actors
// @param directed , is the network directed (TRUE) or not (FALSE)?
// @param ncores number of cores used in the parallelization 
//
// @return vector of true false 
//
Rcpp::List getOmitDyadActiveRiskSet(std::string model,
                                        arma::uvec actor1, 
                                        arma::uvec actor2, 
                                        arma::uvec type,
                                        int D,
                                        int N,
                                        bool directed = true,
                                        int ncores = 1
                                        ) {
                         
    
    arma::uword M = actor1.size();
    arma::uword m;
    arma::umat riskset(1,D); // (output) row matrix of 0's (0 means that the dyad cannot occur)

    #ifdef _OPENMP
    omp_set_dynamic(0);         
    omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
    #pragma omp parallel for if(ncores>1) private(m) shared(M,riskset,actor1,actor2,type,N,directed)  
    #endif
    for(m = 0; m < M; m++){
        int dyad_m = remify::getDyadIndex(actor1(m)-1,actor2(m)-1,type(m)-1,N,directed);
        if(riskset(0,dyad_m) == 0){
            riskset(0,dyad_m) = 1; // it has to be included so we assign 1
        }

    }
    


    arma::vec which_time(M,arma::fill::zeros);

    Rcpp::List out = Rcpp::List::create(Rcpp::Named("time") = which_time,Rcpp::Named("riskset") = riskset); // to add later: Rcpp::Named("time") = which_time, 
    if(model == "actor"){
        arma::umat riskset_sender(1,N); 
        // parallelize here ? (ncores is set up already)
        arma::uvec which_sender = arma::unique(actor1);
        arma::urowvec vec_ones(which_sender.n_elem,arma::fill::ones);
        riskset_sender.cols(which_sender-1) = vec_ones;
        out["risksetSender"] = riskset_sender;
    }
   

    // finding active dyads
    arma::uvec active_dyads = arma::find(riskset.row(0));
    // saving number of active dyads
    out["D_active"] = active_dyads.n_elem;

    // finding vector fo dyadID for the active set of dyads 
    arma::uvec dyadIDactive(M,arma::fill::zeros);
    #ifdef _OPENMP
    omp_set_dynamic(0);         
    omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
    #pragma omp parallel for if(ncores>1) private(m) shared(M,dyadIDactive,active_dyads,actor1,actor2,type,N,directed)  
    #endif
    for(m = 0; m < M; m++){
        int dyad_m = remify::getDyadIndex(actor1(m)-1,actor2(m)-1,type(m)-1,N,directed);
        arma::uvec find_dyad_m =  arma::find(active_dyads == dyad_m);
        dyadIDactive(m) = find_dyad_m(0) + 1;
    }
    out["dyadIDactive"] = dyadIDactive;


    return out;
}


// @title getRisksetSender
//
// @param which_dyad is list of matrices where each matrix defines by row [actor1,actor2,type] to be removed from the riskset. Each matrix as a whole will finally produce a vector (length = D) of 1/0 with 0's for dyads that have to be excluded from the riskset
// @param C number of event types
// @param D number of dyads
// @param N number of actors
//
// @return utility matrix per row 0 if the event could not happen, 1 if the event could happen
Rcpp::IntegerMatrix getRisksetSender(Rcpp::List which_dyad,
                                        int C,
                                        int D, 
                                        int N) {
    arma::uword z,d,D_z;
    int j,c;
    arma::uword Z = which_dyad.size();
    Rcpp::IntegerMatrix riskset(Z,N);
    riskset.fill(1); 
    auto is_na = [](int &k) {k = (k == -1);}; 
    for(z = 0; z < Z; z++){
        Rcpp::IntegerMatrix which_dyad_z = which_dyad[z];
        D_z = which_dyad_z.nrow();
        Rcpp::IntegerVector actor1_z = which_dyad_z(Rcpp::_,0);
        Rcpp::IntegerVector actor2_z =  which_dyad_z(Rcpp::_,1);
        Rcpp::IntegerVector type_z =  which_dyad_z(Rcpp::_,2);
        Rcpp::IntegerVector actor1_na = Rcpp::clone(actor1_z);
        Rcpp::IntegerVector actor2_na = Rcpp::clone(actor2_z);
        Rcpp::IntegerVector type_na = Rcpp::clone(type_z); 
        std::for_each(actor1_na.begin(),actor1_na.end(),is_na);
        std::for_each(actor2_na.begin(),actor2_na.end(),is_na);
        std::for_each(type_na.begin(),type_na.end(),is_na);
        for(d = 0; d < D_z; d++){
            // (1) find case:
            if(type_na(d)){ // when [?,?,NA] (type is NA)
                if(!actor1_na(d) && !actor2_na(d)){ // when [X,Y,NA]
                    for(c = 0; c < C; c++){  // for all the event types
                        if(actor1_z(d) != actor2_z(d)){
                            riskset(z,actor1_z(d)) = 0; 
                        }
                    }   
                }
                else{
                    if(!actor1_na(d)){ // when [X,NA,NA] 
                        for(c = 0; c < C; c++){  // for all the event types
                            for(j = 0; j < N; j++){ // for all the receivers excluding the self-edge
                                if(j != actor1_z(d)){
                                    riskset(z,actor1_z(d)) = 0; 
                                }
                            }  
                        }
                    }
                }
            }
            else{ // when [?,?,C] (type is defined)       
                if(!actor1_na(d) && !actor2_na(d)){ // when [X,Y,C]
                    riskset(z,actor1_z(d)) = 0; 
                }
                else{
                    if(!actor1_na(d)){ // when [X,NA,C]          
                        for(j = 0; j < N; j++){ // for all the receivers excluding the self-edges
                            if(j != actor1_z(d)){
                                riskset(z,actor1_z(d)) = 0; 
                            }  
                        }
                    }
                }                   
            }
        }
    }
    return riskset;
}



// @title getRiskset (a function that returns an utility matrix used in optimization algorithms)
//
// @param which_dyad is list of matrices where each matrix defines by row [actor1,actor2,type] to be removed from the riskset. Each matrix as a whole will finally produce a vector (length = D) of 1/0 with 0's for dyads that have to be excluded from the riskset
// @param C number of event types
// @param D number of dyads
// @param N number of actors
// @param directed bool if the netwrok is directed, then directed ==  TRUE, FALSE otherwise
//
// @return risk set matrix by row 0 if it cannot happen, 1 if it has to be included in the risk set
Rcpp::IntegerMatrix getRiskset(Rcpp::List which_dyad, 
                                int C, 
                                int D, 
                                int N, 
                                bool directed) {
    arma::uword z,d,D_z;
    int i,j,c;
    arma::uword Z = which_dyad.size();
    Rcpp::IntegerMatrix riskset(Z,D);
    riskset.fill(1);
    auto is_na = [](int &k) {k = (k == -1);}; 
    for(z = 0; z < Z; z++){
        Rcpp::IntegerMatrix which_dyad_z = which_dyad[z];
        D_z = which_dyad_z.nrow();
        Rcpp::IntegerVector actor1_z = which_dyad_z(Rcpp::_,0);
        Rcpp::IntegerVector actor2_z =  which_dyad_z(Rcpp::_,1);
        Rcpp::IntegerVector type_z =  which_dyad_z(Rcpp::_,2);
        Rcpp::IntegerVector actor1_na = Rcpp::clone(actor1_z);
        Rcpp::IntegerVector actor2_na = Rcpp::clone(actor2_z);
        Rcpp::IntegerVector type_na = Rcpp::clone(type_z); 
        std::for_each(actor1_na.begin(),actor1_na.end(),is_na);
        std::for_each(actor2_na.begin(),actor2_na.end(),is_na);
        std::for_each(type_na.begin(),type_na.end(),is_na);

        for(d = 0; d < D_z; d++){
            // (1) find case:
            if(type_na(d)){ // when [?,?,NA] (type is NA)
                if(!actor1_na(d) && !actor2_na(d)){ // when [X,Y,NA]
                    int dyad_z_d;
                    for(c = 0; c < C; c++){  // for all the event types
                        if(actor1_z(d) != actor2_z(d)){
                            dyad_z_d = remify::getDyadIndex(actor1_z(d),actor2_z(d),c,N,directed);
                            riskset(z,dyad_z_d) = 0; 
                        }
                    }   
                }
                else{
                    if(!actor1_na(d)){ // when [X,NA,NA] 
                        int dyad_z_d;
                        for(c = 0; c < C; c++){  // for all the event types
                            for(j = 0; j < N; j++){ // for all the receivers excluding the self-edge
                                if(j != actor1_z(d)){
                                    dyad_z_d = remify::getDyadIndex(actor1_z(d),j,c,N,directed);
                                    riskset(z,dyad_z_d) = 0;
                                }
                            }  
                        }
                    }
                    else{ // when [NA,Y,NA]
                        int dyad_z_d;
                        for(c = 0; c < C; c++){  // for all the event types
                            for(i = 0; i < N; i++){ // for all the senders excluding the self-edge
                                if(i != actor2_z(d)){
                                    dyad_z_d = remify::getDyadIndex(i,actor2_z(d),c,N,directed);
                                    riskset(z,dyad_z_d) = 0;
                                }
                            }  
                        }
                    }
                }
            }
            else{ // when [?,?,C] (type is defined)
                if(actor1_na(d) && actor2_na(d)){ // when [NA,NA,C]
                    int dyad_z_d;
                    for(i = 0; i < N; i++){
                        for(j = 0; j < N; j++){ // for all the actors excluding the self-edges
                            if(i != j){
                                dyad_z_d = remify::getDyadIndex(i,j,type_z(d),N,directed);
                                riskset(z,dyad_z_d) = 0;
                            }
                        }  
                    }
                }
                else{    
                    if(!actor1_na(d) && !actor2_na(d)){ // when [X,Y,C]
                        int dyad_z_d = remify::getDyadIndex(actor1_z(d),actor2_z(d),type_z(d),N,directed);
                        riskset(z,dyad_z_d) = 0;
                    }
                    else{
                        if(!actor1_na(d)){ // when [X,NA,C]          
                            int dyad_z_d;
                            for(j = 0; j < N; j++){ // for all the receivers excluding the self-edges
                                if(j != actor1_z(d)){
                                    dyad_z_d = remify::getDyadIndex(actor1_z(d),j,type_z(d),N,directed);
                                    riskset(z,dyad_z_d) = 0;
                                }  
                            }
                        }
                        else{ // when [NA,Y,C] 
                            int dyad_z_d;
                            for(i = 0; i < N; i++){ // for all the receivers excluding the self-edges
                                if(i != actor2_z(d)){
                                    dyad_z_d = remify::getDyadIndex(i,actor2_z(d),type_z(d),N,directed);
                                    riskset(z,dyad_z_d) = 0; 
                                }  
                            }
                        }
                    }
                }                    
            }
        }
    }
    return riskset;
}


// @title processOmitDyad 
//
// a function that returns a list of two objects: a vector ("time") that indicates whether the riskset at the specific time point changed or not; a matrix ("riskset") with all the possible changes in the riskset (defined by row). If a change in the riskset is observed at a certain time index, the vector "time" will contain the row index of the matrix "riskset" to be chosen in order to apply the change into the riskset (the row index is given according to the C++ and Rcpp notation, starting from 0). If no changes in the riskset are observed, then the vector "time" will assume value -1.
//
// @param convertedOmitDyad 
// @param convertedOmitDyad_time
// @param M number of events
// @param C number of event types
// @param D number of dyads
// @param N number of actors
// @param directed bool if the netwrok is directed, then directed ==  TRUE, FALSE otherwise
// @param model "tie" or "actor" oriented
//
// @return a list of two objects: a vector ("time") that indicates whether the riskset at the specific time point changed or not; a matrix ("riskset") with all the possible changes in the riskset (defined by row).
Rcpp::List processOmitDyad(Rcpp::List convertedOmitDyad, Rcpp::List convertedOmitDyad_time, arma::uword M, int C, int D, int N, bool directed, std::string model) {

    arma::uword z,d;
    int m,r;
    int R = convertedOmitDyad.size();
    std::vector<int> timeID;
    Rcpp::IntegerMatrix timeID_mat(R,2);

    //(1) find vector of new bounds 
    for(r = 0; r < R; r++){
        Rcpp::IntegerVector timeID_r = Rcpp::as<Rcpp::IntegerVector>(convertedOmitDyad_time[r]);
        timeID_mat(r,Rcpp::_) = timeID_r;
        for(z = 0; z < 2; z++){
            timeID.push_back(timeID_r[z]);
        }
    }

    // (1.1) sorting the vector of timeID bounds
    std::sort(timeID.begin(),timeID.end()); 
    // (1.2) removing duplicates
    timeID.erase(std::unique(timeID.begin(),timeID.end()),timeID.end()); 

    //(2) arrange lower and upper bounds in two separate vectors ("lb" and "ub")
    std::vector<int> lb,ub;
    for(z = 0; z < (timeID.size()-1); z++){
        lb.push_back(timeID[z]);
        ub.push_back(timeID[z+1]);
    }
    
    //(2.1) further processing of bounds : making sure that we overlap the lists of changes (on the riskset) on the correct time points
    std::for_each(ub.begin(), ub.end()-1, [](int &x) {x -= 1;});
    //for(unsigned int p = 0; p<(ub.size()-1); p++){
    //        ub[p] -= 1;
    //}
    // old method - working b
    //for(z = 0; z < lb.size(); z++){
    //    if((lb[z]!=ub[z]) || ((lb[z]==timeID[0]) || (ub[z]==timeID[timeID.size()-1]))){
    //        // lower bound
    //        int lb_z = lb[z];
    //        if(std::any_of(ub.begin(),ub.end(), [&lb_z](int i){return i == lb_z;})){
    //            lb[z] += 1;
    //        }
    //        // upper bound
    //        int ub_z = ub[z];
    //        if(std::any_of(lb.begin(),lb.end(), [&ub_z](int i){return i == ub_z;})){
    //            ub[z] -= 1;
    //        }
    //    }
    //}
    
    //(2.2) understanding for each of the new intervals, which set of old intervals overlaps
    Rcpp::List which_r = Rcpp::List::create();
    for(z = 0; z < lb.size(); z++){
        Rcpp::IntegerVector which_r_loc;
        for(r = 0; r < timeID_mat.nrow(); r++){
            // old (input) bounds
            if((lb[z] >= timeID_mat(r,0)) && (ub[z] <= timeID_mat(r,1))){
                which_r_loc.push_back(r);
            }
        }
        if(which_r_loc.size()>0){
            which_r.push_back(which_r_loc);
        }
        
    }

    // (3) creating output object (last steps)
    // (3.1) vector "time" of length M : this vector will assume value -1 if no change in the riskset is observed, otherwise it will assume the row index (in c++ notation, i.e. starting from 0) of the matrix ("riskset" defined below within the same function) where to select the modified riskset;
    Rcpp::IntegerVector which_time(M);
    which_time.fill(-1);
    for(z = 0; z < which_r.size(); z++){
        // time vector
        for(m = lb[z]; m <= ub[z]; m++){
            which_time[m] = z;
        }
    }       
    // (3.2) creation of list "which_dyad" which binds the omit_dyad matrices (by row) according to the old overlapping intervals defined above
    //try to change it and recode it such that you can allocate the size of the final matrix and dynamically assigning the matrices to it
    Rcpp::List which_dyad = Rcpp::List::create();

    for(z = 0; z < which_r.size(); z++){
        Rcpp::IntegerVector which_r_loc = which_r[z];
        int idx_z = which_r_loc[0];
        Rcpp::IntegerMatrix dyad_z = convertedOmitDyad[idx_z];
        Rcpp::IntegerVector dyad_z_actor1 = dyad_z.column(0); // actor1
        Rcpp::IntegerVector dyad_z_actor2 = dyad_z.column(1); // actor2
        Rcpp::IntegerVector dyad_z_type = dyad_z.column(2); // type
        if(which_r_loc.size()>1){
            for(r = 1; r < which_r_loc.size(); r++){
                idx_z = which_r_loc[r];
                Rcpp::IntegerMatrix dyad_r = convertedOmitDyad[idx_z];
                Rcpp::IntegerVector dyad_r_actor1 = dyad_r.column(0);
                Rcpp::IntegerVector dyad_r_actor2 = dyad_r.column(1);
                Rcpp::IntegerVector dyad_r_type = dyad_r.column(2);
                for(d = 0; d < dyad_r_actor1.size(); d++){
                    dyad_z_actor1.push_back(dyad_r_actor1[d]);
                    dyad_z_actor2.push_back(dyad_r_actor2[d]);
                    dyad_z_type.push_back(dyad_r_type[d]);
                }
            }
        }
        Rcpp::IntegerMatrix dyad_z_out(dyad_z_actor1.size(),3);
        dyad_z_out.column(0) = dyad_z_actor1;
        dyad_z_out.column(1) = dyad_z_actor2;
        dyad_z_out.column(2) = dyad_z_type;        
        which_dyad.push_back(dyad_z_out);
    }

    // (3.3) based on which_dyad we create a matrix of changing riskset per each element of the list "which_dyad"
    Rcpp::IntegerMatrix riskset = getRiskset(which_dyad,C,D,N,directed);
    // arranging output in a list
    Rcpp::List out = Rcpp::List::create(Rcpp::Named("time") = which_time, Rcpp::Named("riskset") = riskset);
    if(model == "actor"){
        Rcpp::IntegerMatrix riskset_sender = getRisksetSender(which_dyad,C,D,N);
        out["risksetSender"] = riskset_sender;
    }

    return out;
}

// @title convertInputREH
//
// @param input_edgelist is the input data frame with information about [time,actor1,actor2,type,weight] by row.
// @param input_origin origin time point (t_0)
// @param actorsDictionary dictionary of actor names 
// @param typesDicitonary dictionary of event types 
// @param M number of observed relational events
// @param D number of possible dyads
// @param direcred boolean value: are events directed (1) or undirected (0)?
// @param omit_dyad list. The same input in rehCpp.
// @param model, "tie" or "actor" oriented
// @param weighted true/false if the network is weighted (true) or not (false)
// @param ordinal true/false whether to consider the order or the waiting time between events in the network 
// @param C number of event types, 1 is the minimum
// @param active true/false whther the risk set process is the active one (true) or not (false) - default is true
// @param ncores number of threads to use in the parallelization (default is 1)
//
// @return cube of possible combination [actor1,actor2,type]: the cell value is the column index in the rehBinary matrix
Rcpp::List convertInputREH( Rcpp::DataFrame input_edgelist, 
                            Rcpp::RObject input_origin,
                            Rcpp::DataFrame actorsDictionary, 
                            Rcpp::DataFrame typesDictionary, 
                            arma::uword M, 
                            arma::uword D, 
                            bool directed, 
                            Rcpp::List omit_dyad, 
                            std::string model, 
                            bool weighted, 
                            bool ordinal,
                            int C,
                            bool active = false,
                            int ncores = 1)
{
    // for loop iterators
    arma::uword m,r,z,d,R,Z_r,D_r,D_rr;
    // counter for warningMessages
    int undefined_dyad = 0;
    // Creating output list object
    Rcpp::List out = Rcpp::List::create();
    Rcpp::DataFrame edgelist = input_edgelist;
   // edgelist = Rcpp::clone(input_edgelist); // this makes a deep copy of the edgelist - we do not need this in this version of remify
    Rcpp::DataFrame convertedEdgelist;
    Rcpp::List warnings_list = Rcpp::List::create();

    //[**1**] Processing edgelist 
    std::vector<double> time_loc = Rcpp::as<std::vector<double>>(edgelist["time"]); // converting time input to a double 

    // edgelist input actor1 and actor2 with dictionary
    std::vector<std::string> stringActor1 = Rcpp::as<std::vector<std::string>>(edgelist["actor1"]);
    std::vector<std::string> stringActor2 = Rcpp::as<std::vector<std::string>>(edgelist["actor2"]);
    std::vector<std::string> actorName = Rcpp::as<std::vector<std::string>>(actorsDictionary["actorName"]); 
    int N = actorName.size(); // number of actors
    std::vector<int> actorID = Rcpp::as<std::vector<int>>(actorsDictionary["actorID"]);
    

    //(1) Converting `edgelist`
    // we run here a long (and redundant) code to first select (via ifelse) the characteristics of the network and then apply the conversion of the 'edgelist'
    // The sequence of ifelse will be about: [1] weighted/not weighted, [2] C>1 / C = 1, [3] tie / actor model, [4] class of time variable    
    // We run the ifelse to avoid running them at each iteration of m=0,1,...M-1.
    // This (sub)script (lines 618-1043) allows:
    // - not to return a weight column if there is no weight in the input edgelist
    // - not to return a type column if there is no type measured in the networks
    // - not to return dyad attribute for actor-oriented modeling
    // - carefully reduce the size of the event sequence when self-loops need to be removed (this influences all the points above)

    
    std::vector<std::string> typeName(C,"0"); // initialize typeName because it will also be used later when omit_dyad will be processed
    int INFTY_DYAD = 0; // creating a reference constant to remove self-loops from the 'dyad' vector
    double INFTY_TIME = *max_element(time_loc.begin(), time_loc.end()) + 1.0; // creating a reference constant to remove self-loops from the 'time'vector
    std::vector<int> convertedActor1_ID(M,0); // initialize vector of actor1 IDs 
    std::vector<int> convertedActor2_ID(M,0); // initialize vector of actor2 IDs
    std::vector<int> convertedType_ID(1,0); // initialize vector of type IDs (its size will be resized to M when necessary in the loops below)
    std::vector<int> dyad(M,1); // initialize vector of dyads (useful to throw the warning in case there are self-loops)
    if(weighted){
        std::vector<double> weight = Rcpp::as<std::vector<double>>(edgelist["weight"]);
        double INFTY_WEIGHT = *max_element(weight.begin(), weight.end()) + 1.0; // creating a reference constant to remove self-loops from the 'weight' vector
        if(C>1){
            std::vector<std::string> stringType = Rcpp::as<std::vector<std::string>>(edgelist["type"]); // get type column from edgelist
            typeName = Rcpp::as<std::vector<std::string>>(typesDictionary["typeName"]);
            std::vector<int> typeID = Rcpp::as<std::vector<int>>(typesDictionary["typeID"]);
            // allocating memory for actor1, actor2, type and dyad (dyad is used also for the actor-oriented code as helper for the exclusion of self-events)
            convertedType_ID.resize(M,0);
            if(model == "tie"){ // if model == "tie" we include the calculation od the dyad ID in the loop
                #ifdef _OPENMP
                omp_set_dynamic(0);         
                omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
                #pragma omp parallel for if(ncores>1) private(m) shared(M,stringActor1,stringActor2,stringType,actorName,typeName,actorID,typeID,convertedActor1_ID,convertedActor2_ID,convertedType_ID,N,directed,dyad,weight,time_loc)         
                #endif       
                for(m = 0; m < M; m++){ 
                    // m-th event in the edgelist input:
                    if(stringActor1[m].compare(stringActor2[m]) != 0){ // when actor1 is different than actor2
                        // find actor1
                        std::vector<std::string>::iterator i = std::find(actorName.begin(), actorName.end(), stringActor1[m]);
                        convertedActor1_ID[m] = actorID.at(std::distance(actorName.begin(), i));
                        

                        // find actor2
                        std::vector<std::string>::iterator j = std::find(actorName.begin(), actorName.end(), stringActor2[m]);
                        convertedActor2_ID[m] = actorID.at(std::distance(actorName.begin(), j));

                        // find type 
                        std::vector<std::string>::iterator c = std::find(typeName.begin(), typeName.end(), stringType[m]);
                        convertedType_ID[m] = typeID.at(std::distance(typeName.begin(), c));

                        // getting dyad index
                        dyad[m] = remify::getDyadIndex(convertedActor1_ID[m]-1,convertedActor2_ID[m]-1,convertedType_ID[m]-1,N,directed)+1; // dyads from 1 to D    
                    }
                    else{ // m-th event is a self-loop 
                        dyad[m] = INFTY_DYAD; // dyad = 0 means self-loop that will be removed
                        weight[m] = INFTY_WEIGHT;
                        time_loc[m] = INFTY_TIME;
                        stringActor1[m] = "";
                        stringActor2[m] = "";
                        stringType[m] = "";
                    }
                }
                // check for self-loop in [weighted/C>1/tie]
                int check_self_loop = std::count(dyad.cbegin(), dyad.cend(), 0); //##
                if(check_self_loop>0){ // there are self-loops to be removed from the sequence
                    dyad.erase(std::remove_if(dyad.begin(), dyad.end(), [&INFTY_DYAD](int x){return (x==INFTY_DYAD);}),dyad.end());
                }            
                out["dyad"] = dyad; 
            } 
            else{ // if the model == "actor" we omit the computation of the dyad ID from the loop
                #ifdef _OPENMP
                omp_set_dynamic(0);         
                omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
                #pragma omp parallel for if(ncores>1) private(m) shared(M,stringActor1,stringActor2,stringType,actorName,typeName,actorID,typeID,convertedActor1_ID,convertedActor2_ID,convertedType_ID,dyad,weight,time_loc)    
                #endif
                for(m = 0; m < M; m++){ //loop without calculating the dyad
                    // m-th event in the edgelist input:
                    if(stringActor1[m].compare(stringActor2[m]) != 0){ // when actor1 is different than actor2
                        // find actor1
                        std::vector<std::string>::iterator i = std::find(actorName.begin(), actorName.end(), stringActor1[m]);
                        convertedActor1_ID[m] = actorID.at(std::distance(actorName.begin(), i));

                        // find actor2
                        std::vector<std::string>::iterator j = std::find(actorName.begin(), actorName.end(), stringActor2[m]);
                        convertedActor2_ID[m] = actorID.at(std::distance(actorName.begin(), j));

                        // find type 
                        std::vector<std::string>::iterator c = std::find(typeName.begin(), typeName.end(), stringType[m]);
                        convertedType_ID[m] = typeID.at(std::distance(typeName.begin(), c)); 
                    }
                    else{
                        // m-th event is a self-loop
                        dyad[m] = INFTY_DYAD;
                        weight[m] = INFTY_WEIGHT;
                        time_loc[m] = INFTY_TIME;
                        stringActor1[m] = "";
                        stringActor2[m] = "";
                        stringType[m] = "";
                    }
                }
                // check for self-loop in [weighted/C>1/actor]
                int check_self_loop = std::count(dyad.cbegin(), dyad.cend(), 0); //##
                if(check_self_loop>0){ // there are self-loops to be removed from the sequence
                    dyad.erase(std::remove_if(dyad.begin(), dyad.end(), [&INFTY_DYAD](int x){return (x==INFTY_DYAD);}),dyad.end());
                }  
                out["dyad"] = R_NilValue;
            }
            // save size of the network
            out["M"] = dyad.size(); 
            if(dyad.size() < M){ // there are self-loops to be removed from the sequence
                // time
                time_loc.erase(std::remove_if(time_loc.begin(), time_loc.end(), [&INFTY_TIME](double x){return (x>=INFTY_TIME);}),time_loc.end());
                // actor1_ID
                convertedActor1_ID.erase(std::remove_if(convertedActor1_ID.begin(), convertedActor1_ID.end(), [](int x){return (x==0);}),convertedActor1_ID.end());
                // actor2_ID
                convertedActor2_ID.erase(std::remove_if(convertedActor2_ID.begin(), convertedActor2_ID.end(), [](int x){return (x==0);}),convertedActor2_ID.end());
                // type_ID
                convertedType_ID.erase(std::remove_if(convertedType_ID.begin(), convertedType_ID.end(), [](int x){return (x==0);}),convertedType_ID.end());
                // weight
                weight.erase(std::remove_if(weight.begin(), weight.end(), [&INFTY_WEIGHT](double x){return (x>=INFTY_WEIGHT);}),weight.end());

                // actor1
                stringActor1.erase(std::remove_if(stringActor1.begin(), stringActor1.end(), [](std::string x){return (x=="");}),stringActor1.end());
                // actor2
                stringActor2.erase(std::remove_if(stringActor2.begin(), stringActor2.end(), [](std::string x){return (x=="");}),stringActor2.end());
                // type
                stringType.erase(std::remove_if(stringType.begin(), stringType.end(), [](std::string x){return (x=="");}),stringType.end());

                if(Rcpp::is<Rcpp::DateVector>(edgelist["time"])){
                    Rcpp::DateVector time_converted = Rcpp::wrap(time_loc);
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2,
                            Rcpp::Named("type") = stringType,
                            Rcpp::Named("weight") = weight);
                            //Rcpp::Rcout << " time class is Date " << "\n";
                }
                else if(Rcpp::is<Rcpp::DatetimeVector>(edgelist["time"])){
                    Rcpp::DatetimeVector time_converted = Rcpp::wrap(time_loc);
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2,
                            Rcpp::Named("type") = stringType,
                            Rcpp::Named("weight") = weight);
                            //Rcpp::Rcout << " time class is Datetime " << "\n";
                }
                else{
                    Rcpp::NumericVector time_converted = Rcpp::wrap(time_loc); // we treat 'integer' time as 'numeric'
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2,
                            Rcpp::Named("type") = stringType,
                            Rcpp::Named("weight") = weight);
                            //Rcpp::Rcout << " time class is numeric" << "\n";
                }
            }
            else{ // no self-loops tp remove
                convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = edgelist["time"],
                                        Rcpp::Named("actor1") = stringActor1, 
                                        Rcpp::Named("actor2") = stringActor2,
                                        Rcpp::Named("type") = stringType,
                                        Rcpp::Named("weight") = edgelist["weight"]);
            } 
            out["actor1_ID"] = convertedActor1_ID;
            out["actor2_ID"] = convertedActor2_ID;
            out["type_ID"] = convertedType_ID;                     
        }
        else{
            if(model == "tie"){ // if model == "tie" we include the calculation od the dyad ID in the loop
                #ifdef _OPENMP
                omp_set_dynamic(0);         
                omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
                #pragma omp parallel for if(ncores>1) private(m) shared(M,stringActor1,stringActor2,actorName,actorID,convertedActor1_ID,convertedActor2_ID,N,directed,dyad,weight,time_loc) 
                #endif
                for(m = 0; m < M; m++){ 
                    // m-th event in the edgelist input:
                    if(stringActor1[m].compare(stringActor2[m]) != 0){ // when actor1 is different than actor2
                        // find actor1
                        std::vector<std::string>::iterator i = std::find(actorName.begin(), actorName.end(), stringActor1[m]);
                        convertedActor1_ID[m] = actorID.at(std::distance(actorName.begin(), i));

                        // find actor2
                        std::vector<std::string>::iterator j = std::find(actorName.begin(), actorName.end(), stringActor2[m]);
                        convertedActor2_ID[m] = actorID.at(std::distance(actorName.begin(), j));

                        // getting dyad index
                        dyad[m] = remify::getDyadIndex(convertedActor1_ID[m]-1,convertedActor2_ID[m]-1,0,N,directed)+1; // dyads from 1 to D   
                    }
                    else{ // m-th event is a self-loop 
                        dyad[m] = INFTY_DYAD; // dyad = 0 means self-loop that will be removed
                        weight[m] = INFTY_WEIGHT;
                        time_loc[m] = INFTY_TIME;
                        stringActor1[m] = "";
                        stringActor2[m] = "";
                    }
                }
                // check for self-loop in [weighted/C>1/tie]
                int check_self_loop = std::count(dyad.cbegin(), dyad.cend(), 0); //##
                if(check_self_loop>0){ // there are self-loops to be removed from the sequence
                    dyad.erase(std::remove_if(dyad.begin(), dyad.end(), [&INFTY_DYAD](int x){return (x==INFTY_DYAD);}),dyad.end());
                }            
                out["dyad"] = dyad; 
            } 
            else{ // if the model == "actor" we omit the computation of the dyad ID from the loop
                #ifdef _OPENMP
                omp_set_dynamic(0);         
                omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
                #pragma omp parallel for if(ncores>1) private(m) shared(M,stringActor1,stringActor2,actorName,actorID,convertedActor1_ID,convertedActor2_ID,dyad,weight,time_loc) 
                #endif
                for(m = 0; m < M; m++){ //loop without calculating the dyad
                    // m-th event in the edgelist input:
                    if(stringActor1[m].compare(stringActor2[m]) != 0){ // when actor1 is different than actor2
                        // find actor1
                        std::vector<std::string>::iterator i = std::find(actorName.begin(), actorName.end(), stringActor1[m]);
                        convertedActor1_ID[m] = actorID.at(std::distance(actorName.begin(), i));

                        // find actor2
                        std::vector<std::string>::iterator j = std::find(actorName.begin(), actorName.end(), stringActor2[m]);
                        convertedActor2_ID[m] = actorID.at(std::distance(actorName.begin(), j));
                    }
                    else{
                        // m-th event is a self-loop
                        dyad[m] = INFTY_DYAD;
                        weight[m] = INFTY_WEIGHT;
                        time_loc[m] = INFTY_TIME;
                        stringActor1[m] = "";
                        stringActor2[m] = "";
                    }
                }
                // check for self-loop in [weighted/C>1/actor]
                int check_self_loop = std::count(dyad.cbegin(), dyad.cend(), 0); //##
                if(check_self_loop>0){ // there are self-loops to be removed from the sequence
                    dyad.erase(std::remove_if(dyad.begin(), dyad.end(), [&INFTY_DYAD](int x){return (x==INFTY_DYAD);}),dyad.end());
                }  
                out["dyad"] = R_NilValue;
            }
            // save size of the network
            out["M"] = dyad.size(); 
            if(dyad.size() < M){ // there are self-loops to be removed from the sequence
                // time
                time_loc.erase(std::remove_if(time_loc.begin(), time_loc.end(), [&INFTY_TIME](double x){return (x>=INFTY_TIME);}),time_loc.end());
                // actor1_ID
                convertedActor1_ID.erase(std::remove_if(convertedActor1_ID.begin(), convertedActor1_ID.end(), [](int x){return (x==0);}),convertedActor1_ID.end());
                // actor2_ID
                convertedActor2_ID.erase(std::remove_if(convertedActor2_ID.begin(), convertedActor2_ID.end(), [](int x){return (x==0);}),convertedActor2_ID.end());
                // weight
                weight.erase(std::remove_if(weight.begin(), weight.end(), [&INFTY_WEIGHT](double x){return (x>=INFTY_WEIGHT);}),weight.end());

                // actor1
                stringActor1.erase(std::remove_if(stringActor1.begin(), stringActor1.end(), [](std::string x){return (x=="");}),stringActor1.end());
                // actor2
                stringActor2.erase(std::remove_if(stringActor2.begin(), stringActor2.end(), [](std::string x){return (x=="");}),stringActor2.end());

                if(Rcpp::is<Rcpp::DateVector>(edgelist["time"])){
                    Rcpp::DateVector time_converted = Rcpp::wrap(time_loc);
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2,
                            Rcpp::Named("weight") = weight);
                            //Rcpp::Rcout << " time class is Date " << "\n";
                }
                else if(Rcpp::is<Rcpp::DatetimeVector>(edgelist["time"])){
                    Rcpp::DatetimeVector time_converted = Rcpp::wrap(time_loc);
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2,
                            Rcpp::Named("weight") = weight);
                            //Rcpp::Rcout << " time class is Datetime " << "\n";
                }
                else{
                    Rcpp::NumericVector time_converted = Rcpp::wrap(time_loc); // we treat 'integer' time as 'numeric'
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2,
                            Rcpp::Named("weight") = weight);
                            //Rcpp::Rcout << " time class is numeric" << "\n";
                }
            }
            else{ // no self-loops tp remove
                convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = edgelist["time"],
                                        Rcpp::Named("actor1") = stringActor1, 
                                        Rcpp::Named("actor2") = stringActor2,
                                        Rcpp::Named("weight") = edgelist["weight"]);
            }  
            out["actor1_ID"] = convertedActor1_ID;
            out["actor2_ID"] = convertedActor2_ID;  
        }
    }
    else{ // no 'weight' column in 'edgelist'
        if(C>1){ // two or more event types
            std::vector<std::string> stringType = Rcpp::as<std::vector<std::string>>(edgelist["type"]); // get type column from edgelist
            typeName = Rcpp::as<std::vector<std::string>>(typesDictionary["typeName"]);
            std::vector<int> typeID = Rcpp::as<std::vector<int>>(typesDictionary["typeID"]);
            convertedType_ID.resize(M,0);
            if(model == "tie"){ // if model == "tie" we include the calculation od the dyad ID in the loop
                #ifdef _OPENMP
                omp_set_dynamic(0);         
                omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
                #pragma omp parallel for if(ncores>1) private(m) shared(M,stringActor1,stringActor2,stringType,actorName,typeName,actorID,typeID,convertedActor1_ID,convertedActor2_ID,convertedType_ID,N,directed,dyad,time_loc)  
                #endif
                for(m = 0; m < M; m++){ 
                    // m-th event in the edgelist input:
                    if(stringActor1[m].compare(stringActor2[m]) != 0){ // when actor1 is different than actor2
                        // find actor1
                        std::vector<std::string>::iterator i = std::find(actorName.begin(), actorName.end(), stringActor1[m]);
                        convertedActor1_ID[m] = actorID.at(std::distance(actorName.begin(), i));

                        // find actor2
                        std::vector<std::string>::iterator j = std::find(actorName.begin(), actorName.end(), stringActor2[m]);
                        convertedActor2_ID[m] = actorID.at(std::distance(actorName.begin(), j));

                        // find type 
                        std::vector<std::string>::iterator c = std::find(typeName.begin(), typeName.end(), stringType[m]);
                        convertedType_ID[m] = typeID.at(std::distance(typeName.begin(), c));

                        // getting dyad index
                        dyad[m] = remify::getDyadIndex(convertedActor1_ID[m]-1,convertedActor2_ID[m]-1,convertedType_ID[m]-1,N,directed)+1; // dyads from 1 to D    
                    }
                    else{ // m-th event is a self-loop 
                        dyad[m] = INFTY_DYAD; // dyad = 0 means self-loop that will be removed
                        time_loc[m] = INFTY_TIME;
                        stringActor1[m] = "";
                        stringActor2[m] = "";
                        stringType[m] = "";
                    }
                }
                // check for self-loop in [weighted/C>1/tie]
                int check_self_loop = std::count(dyad.cbegin(), dyad.cend(), 0); //##
                if(check_self_loop>0){ // there are self-loops to be removed from the sequence
                    dyad.erase(std::remove_if(dyad.begin(), dyad.end(), [&INFTY_DYAD](int x){return (x==INFTY_DYAD);}),dyad.end());
                }            
                out["dyad"] = dyad; 
            } 
            else{ // if the model == "actor" we omit the computation of the dyad ID from the loop
                #ifdef _OPENMP
                omp_set_dynamic(0);         
                omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
                #pragma omp parallel for if(ncores>1) private(m) shared(M,stringActor1,stringActor2,stringType,actorName,typeName,actorID,typeID,convertedActor1_ID,convertedActor2_ID,convertedType_ID,dyad,time_loc)  
                #endif
                for(m = 0; m < M; m++){ //loop without calculating the dyad
                    // m-th event in the edgelist input:
                    if(stringActor1[m].compare(stringActor2[m]) != 0){ // when actor1 is different than actor2
                        // find actor1
                        std::vector<std::string>::iterator i = std::find(actorName.begin(), actorName.end(), stringActor1[m]);
                        convertedActor1_ID[m] = actorID.at(std::distance(actorName.begin(), i));

                        // find actor2
                        std::vector<std::string>::iterator j = std::find(actorName.begin(), actorName.end(), stringActor2[m]);
                        convertedActor2_ID[m] = actorID.at(std::distance(actorName.begin(), j));

                        // find type 
                        std::vector<std::string>::iterator c = std::find(typeName.begin(), typeName.end(), stringType[m]);
                        convertedType_ID[m] = typeID.at(std::distance(typeName.begin(), c)); 
                    }
                    else{
                        // m-th event is a self-loop
                        dyad[m] = INFTY_DYAD;
                        time_loc[m] = INFTY_TIME;
                        stringActor1[m] = "";
                        stringActor2[m] = "";
                        stringType[m] = "";
                    }
                }
                // check for self-loop in [weighted/C>1/actor]
                int check_self_loop = std::count(dyad.cbegin(), dyad.cend(), 0); //##
                if(check_self_loop>0){ // there are self-loops to be removed from the sequence
                    dyad.erase(std::remove_if(dyad.begin(), dyad.end(), [&INFTY_DYAD](int x){return (x==INFTY_DYAD);}),dyad.end());
                }  
                out["dyad"] = R_NilValue;
            }
            // save size of the network
            out["M"] = dyad.size(); 
            if(dyad.size() < M){ // there are self-loops to be removed from the sequence
                // time
                time_loc.erase(std::remove_if(time_loc.begin(), time_loc.end(), [&INFTY_TIME](double x){return (x>=INFTY_TIME);}),time_loc.end());
                // actor1_ID
                convertedActor1_ID.erase(std::remove_if(convertedActor1_ID.begin(), convertedActor1_ID.end(), [](int x){return (x==0);}),convertedActor1_ID.end());
                // actor2_ID
                convertedActor2_ID.erase(std::remove_if(convertedActor2_ID.begin(), convertedActor2_ID.end(), [](int x){return (x==0);}),convertedActor2_ID.end());
                // type_ID
                convertedType_ID.erase(std::remove_if(convertedType_ID.begin(), convertedType_ID.end(), [](int x){return (x==0);}),convertedType_ID.end());

                // actor1
                stringActor1.erase(std::remove_if(stringActor1.begin(), stringActor1.end(), [](std::string x){return (x=="");}),stringActor1.end());
                // actor2
                stringActor2.erase(std::remove_if(stringActor2.begin(), stringActor2.end(), [](std::string x){return (x=="");}),stringActor2.end());
                // type
                stringType.erase(std::remove_if(stringType.begin(), stringType.end(), [](std::string x){return (x=="");}),stringType.end());

                if(Rcpp::is<Rcpp::DateVector>(edgelist["time"])){
                    Rcpp::DateVector time_converted = Rcpp::wrap(time_loc);
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2,
                            Rcpp::Named("type") = stringType
                            );
                            //Rcpp::Rcout << " time class is Date " << "\n";
                }
                else if(Rcpp::is<Rcpp::DatetimeVector>(edgelist["time"])){
                    Rcpp::DatetimeVector time_converted = Rcpp::wrap(time_loc);
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2,
                            Rcpp::Named("type") = stringType);
                            //Rcpp::Rcout << " time class is Datetime " << "\n";
                }
                else{
                    Rcpp::NumericVector time_converted = Rcpp::wrap(time_loc); // we treat 'integer' time as 'numeric'
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2,
                            Rcpp::Named("type") = stringType);
                            //Rcpp::Rcout << " time class is numeric" << "\n";
                }
            }
            else{ // no self-loops tp remove
                convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = edgelist["time"],
                                        Rcpp::Named("actor1") = stringActor1, 
                                        Rcpp::Named("actor2") = stringActor2,
                                        Rcpp::Named("type") = stringType);
            }
            out["actor1_ID"] = convertedActor1_ID;
            out["actor2_ID"] = convertedActor2_ID;
            out["type_ID"] = convertedType_ID;                     
        }
        else{
            if(model == "tie"){ // if model == "tie" we include the calculation od the dyad ID in the loop
                #ifdef _OPENMP
                omp_set_dynamic(0);         
                omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
                #pragma omp parallel for if(ncores>1) private(m) shared(M,stringActor1,stringActor2,actorName,actorID,convertedActor1_ID,convertedActor2_ID,N,directed,dyad,time_loc)  
                #endif
                for(m = 0; m < M; m++){ 
                    // m-th event in the edgelist input:
                    if(stringActor1[m].compare(stringActor2[m]) != 0){ // when actor1 is different than actor2
                        // find actor1
                        std::vector<std::string>::iterator i = std::find(actorName.begin(), actorName.end(), stringActor1[m]);
                        convertedActor1_ID[m] = actorID.at(std::distance(actorName.begin(), i));

                        // find actor2
                        std::vector<std::string>::iterator j = std::find(actorName.begin(), actorName.end(), stringActor2[m]);
                        convertedActor2_ID[m] = actorID.at(std::distance(actorName.begin(), j));

                        // getting dyad index
                        dyad[m] = remify::getDyadIndex(convertedActor1_ID[m]-1,convertedActor2_ID[m]-1,0,N,directed)+1; // dyads from 1 to D  
                    }
                    else{ // m-th event is a self-loop 
                        dyad[m] = INFTY_DYAD; // dyad = 0 means self-loop that will be removed
                        time_loc[m] = INFTY_TIME;
                        stringActor1[m] = "";
                        stringActor2[m] = "";
                    }
                }
                // check for self-loop in [weighted/C>1/tie]
                int check_self_loop = std::count(dyad.cbegin(), dyad.cend(), 0); //##
                if(check_self_loop>0){ // there are self-loops to be removed from the sequence
                    dyad.erase(std::remove_if(dyad.begin(), dyad.end(), [&INFTY_DYAD](int x){return (x==INFTY_DYAD);}),dyad.end());
                }      
                out["dyad"] = dyad; 
            } 
            else{ // if the model == "actor" we omit the computation of the dyad ID from the loop
                #ifdef _OPENMP
                omp_set_dynamic(0);         
                omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
                #pragma omp parallel for if(ncores>1) private(m) shared(M,stringActor1,stringActor2,actorName,actorID,convertedActor1_ID,convertedActor2_ID,dyad,time_loc)  
                #endif
                for(m = 0; m < M; m++){ //loop without calculating the dyad
                    // m-th event in the edgelist input:
                    if(stringActor1[m].compare(stringActor2[m]) != 0){ // when actor1 is different than actor2
                        // find actor1
                        std::vector<std::string>::iterator i = std::find(actorName.begin(), actorName.end(), stringActor1[m]);
                        convertedActor1_ID[m] = actorID.at(std::distance(actorName.begin(), i));

                        // find actor2
                        std::vector<std::string>::iterator j = std::find(actorName.begin(), actorName.end(), stringActor2[m]);
                        convertedActor2_ID[m] = actorID.at(std::distance(actorName.begin(), j));
                    }
                    else{
                        // m-th event is a self-loop
                        dyad[m] = INFTY_DYAD;
                        time_loc[m] = INFTY_TIME;
                        stringActor1[m] = "";
                        stringActor2[m] = "";
                    }
                }
                // check for self-loop in [weighted/C>1/actor]
                int check_self_loop = std::count(dyad.cbegin(), dyad.cend(), 0); //##
                if(check_self_loop>0){ // there are self-loops to be removed from the sequence
                    dyad.erase(std::remove_if(dyad.begin(), dyad.end(), [&INFTY_DYAD](int x){return (x==INFTY_DYAD);}),dyad.end());
                }  
                out["dyad"] = R_NilValue;
            }
            // save size of the network
            out["M"] = dyad.size(); 
            if(dyad.size() < M){ // there are self-loops to be removed from the sequence
                // time
                time_loc.erase(std::remove_if(time_loc.begin(), time_loc.end(), [&INFTY_TIME](double x){return (x>=INFTY_TIME);}),time_loc.end());
                // actor1_ID
                convertedActor1_ID.erase(std::remove_if(convertedActor1_ID.begin(), convertedActor1_ID.end(), [](int x){return (x==0);}),convertedActor1_ID.end());
                // actor2_ID
                convertedActor2_ID.erase(std::remove_if(convertedActor2_ID.begin(), convertedActor2_ID.end(), [](int x){return (x==0);}),convertedActor2_ID.end());

                // actor1
                stringActor1.erase(std::remove_if(stringActor1.begin(), stringActor1.end(), [](std::string x){return (x=="");}),stringActor1.end());
                // actor2
                stringActor2.erase(std::remove_if(stringActor2.begin(), stringActor2.end(), [](std::string x){return (x=="");}),stringActor2.end());

                if(Rcpp::is<Rcpp::DateVector>(edgelist["time"])){
                    Rcpp::DateVector time_converted = Rcpp::wrap(time_loc);
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2);
                            //Rcpp::Rcout << " time class is Date " << "\n";
                }
                else if(Rcpp::is<Rcpp::DatetimeVector>(edgelist["time"])){
                    Rcpp::DatetimeVector time_converted = Rcpp::wrap(time_loc);
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2);
                            //Rcpp::Rcout << " time class is Datetime " << "\n";
                }
                else{
                    Rcpp::NumericVector time_converted = Rcpp::wrap(time_loc); // we treat 'integer' time as 'numeric'
                    convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = time_converted,
                            Rcpp::Named("actor1") = stringActor1, 
                            Rcpp::Named("actor2") = stringActor2);
                            //Rcpp::Rcout << " time class is numeric" << "\n";
                }
            }
            else{ // no self-loops to remove
                convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = edgelist["time"],
                                        Rcpp::Named("actor1") = stringActor1, 
                                        Rcpp::Named("actor2") = stringActor2);
            }
            out["actor1_ID"] = convertedActor1_ID;
            out["actor2_ID"] = convertedActor2_ID;    
        }
    }

    // throw a warning about self-loops removed from the event sequence
    if(dyad.size() < M){
        //Rcpp::Rcout << warningMessage(1);
        // removing self-loops from the convertedEdgelist here
        warnings_list.push_back(warningMessage(1));
    }                                                                            

    //[**2**] Processing time variable
    out["order"] = R_NilValue; 
    if(!ordinal){
        std::vector<double> input_time = Rcpp::as<std::vector<double>>(convertedEdgelist["time"]); // converting any time input to a double 
        double min_time = *min_element(input_time.begin(), input_time.end());
        double origin;
        // (1.1) Check sorting time variable and force the sorting if necessary
        if(!std::is_sorted(std::begin(input_time), std::end(input_time))){
            //Rcpp::Rcout << warningMessage(0); // warning message about the sorting operation
            warnings_list.push_back(warningMessage(0));
            // reordering edgelist
            std::vector<int> sorted_time_order(input_time.size()); //it was std::vector<std::size_t> with size_t from stddef library
            std::iota(std::begin(sorted_time_order), std::end(sorted_time_order), 0);
            std::sort(std::begin(sorted_time_order), std::end(sorted_time_order),
                    [&input_time](const auto & lhs, const auto & rhs)
                    {
                        return input_time[lhs] < input_time[rhs];
                    }
            );
            arma::uvec order_index = arma::conv_to<arma::uvec>::from(sorted_time_order);
            convertedEdgelist = rearrangeDataFrame(convertedEdgelist,order_index); // overwriting 'convertedEdgelist' given the new order
            out["order"] = order_index; // returning order of time points if they are not sorted
            // saving the new sorted time
            input_time = Rcpp::as<std::vector<double>>(convertedEdgelist["time"]); // overwriting 'input time' given the new order
            std::adjacent_difference(input_time.begin(), input_time.end(), input_time.begin()); // with std::adjacent_difference input_time[0] remains the same so we will update it later when processing the origin
        }
        else{
            std::adjacent_difference(input_time.begin(), input_time.end(), input_time.begin()); // with std::adjacent_difference input_time[0] remains the same so we will update it later when processing the origin 
        } 

        // (2) Checking whether the origin is NULL or it is defined by the user
        if(Rf_isNull(input_origin)){ // if origin input is NULL
            origin = min_time - 1.0; // if in seconds event_0 will occur one second earlier than event_1, if in days it will be one day earlier
        }
        else{ // otherwise store check the input value and store it
            double origin_loc = Rcpp::as<double>(input_origin); 
            if(origin_loc >= min_time){ // check if the supplied origin has the same value of the first time point (throw a warning and change the value in the same way when origin is NULL)
                //Rcpp::Rcout << warningMessage(2); // the origin provided as input is a time value greater to at least one event, origin is now set to a different value 
                warnings_list.push_back(warningMessage(2));
                origin = min_time - 1.0; // setting the origin to a second/minute/hour/day earlier
            }
            else{
                origin = origin_loc;
            }
        }
        input_time[0] -= origin;

        out["intereventTime"] = input_time;

        // evenly spaced time (processing)
        auto which_interevent_is_zero = std::find(input_time.begin(), input_time.end(), 0.0);
        if(which_interevent_is_zero != input_time.end()){ // at least one interevent time is equal to 0.0
        arma::vec evenly_spaced_time = arma::conv_to<arma::vec>::from(input_time);
        arma::uvec rows_to_remove = arma::find(evenly_spaced_time <= 0.0);
        out["rows_to_remove"] = rows_to_remove+1;
        int size_time_input = static_cast<int>(input_time.size());
        int a,b;
        double wt;
        a = 1;
        wt = 0.0;
        while(a < size_time_input){
            b = a;
            std::vector<int> k_vec;
            while(b < size_time_input){
                if(input_time[b] == 0.0){
                    if(input_time[b-1] != 0.0){
                        k_vec.push_back(b-1);
                        k_vec.push_back(b);
                        wt = input_time[b-1];
                    }
                    else{
                        k_vec.push_back(b);
                    }
                }
                else if((input_time[b] != 0.0) && (static_cast<int>(k_vec.size())>0)){
                    wt /= static_cast<double>(k_vec.size());
                    for(int e = 0 ; e < static_cast<int>(k_vec.size()); e++){
                        int which_pos = k_vec[e];
                        evenly_spaced_time(which_pos) = wt;
                    }
                    k_vec.clear();
                    wt = 0.0;
                    a = b;
                    b = size_time_input;
                }
                else if((input_time[b] != 0.0) && (static_cast<int>(k_vec.size())==0)){
                    k_vec.clear();
                    wt = 0.0;
                    b = size_time_input;
                }
                if(b == (size_time_input-1)){
                    wt /= static_cast<double>(k_vec.size());
                    for(int e = 0 ; e < static_cast<int>(k_vec.size()); e++){
                        int which_pos = k_vec[e];
                        evenly_spaced_time(which_pos) = wt;
                    }
                    k_vec.clear();
                    wt = 0.0;
                    a = b;
                    b = size_time_input;
                }
                b += 1;
            }
            a += 1;
        }
            out["evenly_spaced_interevent_time"] = evenly_spaced_time;
        }
        else{
            out["rows_to_remove"] = R_NilValue;
            out["evenly_spaced_interevent_time"] = R_NilValue;
        }

    }
    else{
        std::vector<int> input_time = Rcpp::as<std::vector<int>>(convertedEdgelist["time"]); // converting any time input to a double 
        // (1.1) Check sorting time variable and force the sorting if necessary (ranks may be not correctly sorted)
        if(!std::is_sorted(std::begin(input_time), std::end(input_time))){
            //Rcpp::Rcout << warningMessage(0); // warning message about the sorting operation
            warnings_list.push_back(warningMessage(0));
            // reordering edgelist
            std::vector<int> sorted_time_order(input_time.size()); //it was std::vector<std::size_t> with size_t from stddef library
            std::iota(std::begin(sorted_time_order), std::end(sorted_time_order), 0);
            std::sort(std::begin(sorted_time_order), std::end(sorted_time_order),
                    [&input_time](const auto & lhs, const auto & rhs)
                    {
                        return input_time[lhs] < input_time[rhs];
                    }
            );
            arma::uvec order_index = arma::conv_to<arma::uvec>::from(sorted_time_order);
            convertedEdgelist = rearrangeDataFrame(convertedEdgelist,order_index); // overwriting 'convertedEdgelist' given the new order
            out["order"] = order_index; // returning order of time points if they are not sorted
            // saving the new sorted time
            input_time = Rcpp::as<std::vector<int>>(convertedEdgelist["time"]); // overwriting 'input time' given the new order
            std::adjacent_difference(input_time.begin(), input_time.end(), input_time.begin()); // with std::adjacent_difference input_time[0] remains the same so we will update it later when processing the origin
        }
        else{
            std::adjacent_difference(input_time.begin(), input_time.end(), input_time.begin()); // with std::adjacent_difference input_time[0] remains the same so we will update it later when processing the origin 
        }

        arma::uvec distance_time = arma::conv_to<arma::uvec>::from(input_time);
        distance_time(0) = 1;
        arma::uvec which_interevent_is_zero = arma::find(distance_time <= 0); // can't be negative at this stage of processing
        if(which_interevent_is_zero.n_elem != 0){ // at least one interevent time is equal to 0
            out["rows_to_remove"] = which_interevent_is_zero;
        }

        out["intereventTime"] = R_NilValue;
        out["evenly_spaced_interevent_time"] = R_NilValue;
    }

    // Storing converted `edgelist` (without self-loops, if present, and, reoredered if events were not sorted by their time of occurrence)
    out["edgelist"] = convertedEdgelist;

    // [**3**] Converting `omit_dyad` list
    if(omit_dyad.length()>0){
        // input time
        std::vector<double> time = Rcpp::as<std::vector<double>>(convertedEdgelist["time"]); // consider the vector of time from the convertedEdgelist (because there might be some self-loop removed, in turn, changing the size of the time vector)
        Rcpp::List convertedOmitDyad = Rcpp::List::create(); // r-th list with matrix inputs converted into IDs
        Rcpp::List convertedOmitDyad_time = Rcpp::List::create(); // r-th list with time inputs converted into IDs
        int N = actorName.size();
        R = omit_dyad.length();
        int M_processed = time.size();

        for(r = 0; r < R; r++){
            // converting r-th element in omit_dyad
            Rcpp::List omit_r = omit_dyad[r]; // r-th input of `omit_dyad`

            // (1) converting vector of time points
            std::vector<double> time_r = Rcpp::as<std::vector<double>>(omit_r["time"]);
            std::vector<int> timeID_r;
            Z_r = time_r.size(); // Z_r must be 2 elements long but we check its length and throw an errorMessage if it is not equal to 2

            
            if(Z_r != 2){
                // [[Rcpp::stop]] if time_r has size different than 2, stop the function
                Rcpp::stop(errorMessage(1));
                //out["error"] = errorMessage(1);
                //return out;
            }

            for(z = 0 ; z < Z_r; z++){
                if(Rcpp::NumericVector::is_na(time_r[z])){
                    if(z == 0){
                        timeID_r.push_back(0); // if (NA,xx), NA is set to 0
                    }
                    if(z == 1){
                        timeID_r.push_back(M_processed-1); // if (xx,NA), NA is set to M-1
                    }
                }
                else{
                    std::vector<double>::iterator iterator_z = std::find(time.begin(), time.end(), time_r[z]); 
                    if(iterator_z !=  time.end()){
                        timeID_r.push_back(iterator_z - time.begin());
                    }
                }
            }
            if(timeID_r.size() == 2){
                if(timeID_r[1]<timeID_r[0]){
                    // [[Rcpp::stop]] if converted time indices are not sorted, stop the function
                    Rcpp::stop(errorMessage(0));
                    //out["error"] = errorMessage(0);
                    //return out;
                }
            }
            else{
                // [[Rcpp::stop]] if one of the times provided in the input is not found, stop the function
                Rcpp::stop(errorMessage(2));
                //out["error"] = errorMessage(2);
                //return out;
            }
            
            convertedOmitDyad_time.push_back(timeID_r);

            // (2) converting `dyad` DataFrame according to the dictionaries of actors and types
            Rcpp::DataFrame dyad_r = Rcpp::as<Rcpp::DataFrame>(omit_r["dyad"]);
            D_r = dyad_r.nrows();
            std::vector<std::string> actor1_r, actor2_r, type_r;
            // Converting to std::vector<std::string> : useful for the conversion to IDs
            if(TYPEOF(dyad_r["actor1"]) == LGLSXP){ // this check is needed because if one of the three columns is only filled with NA's then the type becomes logical (LGLSXP) and cannot be converted to a std::vector of std::string
                for(d = 0; d < D_r; d++) actor1_r.push_back("NA");
            }
            else{
                actor1_r = Rcpp::as<std::vector<std::string>>(dyad_r["actor1"]); // actor1
            }
            if(TYPEOF(dyad_r["actor2"]) == LGLSXP){
                for(d = 0; d < D_r; d++) actor2_r.push_back("NA");
            }
            else{
                actor2_r = Rcpp::as<std::vector<std::string>>(dyad_r["actor2"]); // actor2
            }
            if(TYPEOF(dyad_r["type"]) == LGLSXP){
                for(d = 0; d < D_r; d++) type_r.push_back("NA");
            }
            else{
                type_r = Rcpp::as<std::vector<std::string>>(dyad_r["type"]); // type
            }

            Rcpp::IntegerVector convertedActor1, convertedActor2, convertedType; 
            std::vector<std::string>::iterator iteratorActor1, iteratorActor2, iteratorType;

            for(d = 0; d < D_r; d++){
                //(1) get iterators and check whether they are found in the dictionaries of actor and types (if not, the d-th iteration will be skipped):

                // iterator actor1:
                if(actor1_r[d] != "NA"){
                    iteratorActor1 = std::find(actorName.begin(), actorName.end(), actor1_r[d]);
                }
                // iterator actor2:
                if(actor2_r[d] != "NA"){
                    iteratorActor2 = std::find(actorName.begin(), actorName.end(), actor2_r[d]);
                }
                // iterator type:
                if(type_r[d] != "NA"){
                    iteratorType = std::find(typeName.begin(), typeName.end(), type_r[d]);
                }

                // checking for iterators:
                if(((actor1_r[d] != "NA") && (iteratorActor1 == actorName.end())) || ((actor2_r[d] != "NA") && (iteratorActor2 == actorName.end())) ||((type_r[d] != "NA") && (iteratorType == typeName.end()))){
                    undefined_dyad++;
                    // old code
                    //continue; // `continue` forces the for loop to continue with the next iteration
                }  
                else{ // all the three inputs [actor1,actor2,type] passed the check, therefore storing the ID's
                    // storing id actor1
                    if(actor1_r[d] != "NA"){
                        convertedActor1.push_back(iteratorActor1 - actorName.begin());
                    }
                    else{
                        convertedActor1.push_back(-1); //old code: R_NaN
                    }
                    // storing id actor2
                    if(actor2_r[d] != "NA"){
                        convertedActor2.push_back(iteratorActor2 - actorName.begin());
                    }
                    else{
                        convertedActor2.push_back(-1); //old code: R_NaN
                    }
                    // storing id type
                    if(type_r[d] != "NA"){
                        convertedType.push_back(iteratorType - typeName.begin());
                    }
                    else{
                        convertedType.push_back(-1); //old code: R_NaN
                    }
                }              
            }
            
            // sorting actor1 and actor2 ID;s if directed FALSE 
            if(!directed){
                D_rr = convertedActor1.length();   
                for(d = 0; d < D_rr; d++){
                    if((convertedActor1[d]!=(-1)) && (convertedActor2[d]!=(-1))){ // both id actor1 and id actor2 are not NaN
                        if(convertedActor1[d] > convertedActor2[d]){
                            int actor1_loc = convertedActor1[d];
                            int actor2_loc = convertedActor2[d];
                            convertedActor1[d] = actor2_loc;
                            convertedActor2[d] = actor1_loc;
                        }
                    }
                    else{
                        if((convertedActor1[d]==(-1)) && (convertedActor2[d]==0)){
                                convertedActor1[d] = 0;
                                convertedActor2[d]= -1;
                        }
                        else{
                            if((convertedActor1[d]==(N-1)) && (convertedActor2[d]==(-1))){     // or make sure that ID = 0 is in actor1 or ID = N-1 is in actor2
                                    convertedActor1[d] = -1;
                                    convertedActor2[d] = N-1;
                            }
                        }
                    }
                }
            }
            // creating converted matrix output for the r-th element in omit_dyad
            arma::mat dyad_r_mat(convertedActor1.length(),3);
            dyad_r_mat.col(0) = Rcpp::as<arma::vec>(convertedActor1);
            dyad_r_mat.col(1) = Rcpp::as<arma::vec>(convertedActor2);       
            dyad_r_mat.col(2) = Rcpp::as<arma::vec>(convertedType);

            // (3) storing the r-th converted input into the output list convertedOmitDyad
            convertedOmitDyad.push_back(dyad_r_mat);
        }

        // Warning messages
        if(undefined_dyad > 0){
            //Rcpp::Rcout << warningMessage(3); // when at least one actor supplied in omit_dyad was not found in the edgelist
            warnings_list.push_back(warningMessage(3));
        }

        //(4) processing (converted to id's and to -1 when NA) omit_dyad
        Rcpp::List outOmitDyad = processOmitDyad(convertedOmitDyad,convertedOmitDyad_time,M,C,D,N,directed,model);

        // (??) Storing the converted `omit_dyad`
        out["omit_dyad"] = outOmitDyad;

    }
    else if(active){ // If the input list `omit_dyad` is NULL and active=true, then compute the "active" risk set
        arma::uvec type_loc(M,arma::fill::ones);
        if(C>1){
            type_loc = Rcpp::as<arma::uvec>(out["type_ID"]);
        }
        Rcpp::List outOmitDyad = getOmitDyadActiveRiskSet(model,out["actor1_ID"],out["actor2_ID"],type_loc,D,N,directed,ncores);
        out["omit_dyad"] = outOmitDyad;
    }
    else{  // if the algorithm reaches here, the the risk set is "static" and the function returns a NULL value
      out["omit_dyad"] = R_NilValue;
    }

    // returning warnings messages as list
    out["warnings"] = warnings_list;     

    return out;
}



// @title remifyCpp (the Rcpp alias of \code{remify()})
//
// @details more details can be found at the following documentation: \link[remify]{reh}.
// 
// @param input_edgelist an object of class \code{"\link[base]{data.frame}"} or 
// \code{"\link[base]{matrix}"} characterizing the relational event history sorted by 
// time with columns 'time', 'actor1', 'actor2' and optionally 'type' and 
// 'weight'. 
// @param actors vector of actors that may be observed interacting in the network. If \code{NULL}, actor names will be drawn from the input edgelist.
// @param types vector of event types that may occur in the network. If \code{NULL}, type names will be drawn from the input edgelist.
// @param directed logical value indicating whether dyadic events are directed (\code{TRUE}) or undirected (\code{FALSE}).
// @param ordinal  logical value indicating whether only the order of events matters in the model (\code{TRUE}) or also the waiting time must be considered in the model (\code{FALSE}).
// @param origin time point since which when events could occur (default is \code{NULL}). If it is defined, it must have the same class of the time column in the input edgelist.
// @param omit_dyad list of lists of two elements: `time`, that is a vector of the time points which to omit dyads from, `dyad`, which is a \code{"\link[base]{data.frame}"} where dyads to be omitted are supplied.
// @param model "tie" or "actor" oriented model
// @param active true/false whther the risk set process is the active one (true) or not (false) - default is true
// @param ncores number of threads to use in the parallelization (default is 1)
//
// @return list of objects with processed raw data.
//
// [[Rcpp::export]]
Rcpp::List remifyCpp(Rcpp::DataFrame input_edgelist,
                  Rcpp::RObject actors, 
                  Rcpp::RObject types,  
                  bool directed,
                  bool ordinal,
                  Rcpp::RObject origin,
                  Rcpp::List omit_dyad,
                  std::string model,
                  bool active = false,
                  int ncores = 1){

    // Allocating memory for some variables and the output list
    arma::uword N,C,D; // number of dyads which depends on the directed input value 
    Rcpp::List out = Rcpp::List::create(); // output list

    // cloning some input objects
    Rcpp::DataFrame edgelist = Rcpp::clone(input_edgelist);

    // START of the processing

    // storing the number of events
    arma::uword M = edgelist.nrows(); // number of events

    // Processing actor1, actor2, type, weight columns to StringVector or NumericVector 

    // actor1
    edgelist["actor1"] = Rcpp::as<Rcpp::StringVector>(edgelist["actor1"]);

    // actor2 
    edgelist["actor2"] = Rcpp::as<Rcpp::StringVector>(edgelist["actor2"]);

    // type
    out["with_type"] = false;
    if(edgelist.containsElementNamed("type")){ 
        edgelist["type"] = Rcpp::as<Rcpp::StringVector>(edgelist["type"]); 
        out["with_type"] = true;
    }



    // Is the network weighted?
    out["weighted"] = false;
    if(edgelist.containsElementNamed("weight")){ // if weight is not defined
        edgelist["weight"] = Rcpp::as<Rcpp::NumericVector>(edgelist["weight"]); 
        out["weighted"] = true;
    } 

    // StringVector of actor1
    Rcpp::StringVector actor1 = edgelist["actor1"]; // actor1/sender

    // StringVector of actor2
    Rcpp::StringVector actor2 = edgelist["actor2"]; // actor2/receiver

    //StringVector of actor1 and actor2 
    arma::uword actors_vector_length = 0;
    Rcpp::StringVector actors_vector;
    if(!Rf_isNull(actors)){
        actors_vector = Rcpp::as<Rcpp::StringVector>(actors);
        actors_vector_length = actors_vector.length();
    }

    Rcpp::StringVector actor1_and_actor2(actor1.length()+actor2.length()+actors_vector_length);
    actor1_and_actor2[Rcpp::Range(0,(actor1.length()-1))] = actor1;
    actor1_and_actor2[Rcpp::Range(actor1.length(),(actor1.length()+actor2.length()-1))] = actor2;

    if(!Rf_isNull(actors)){
        actor1_and_actor2[Rcpp::Range(actor1.length()+actor2.length(),(actor1_and_actor2.length()-1))] = actors_vector;
    }

    // Finding unique strings in actor1_and_actor2 
    Rcpp::StringVector actorName = Rcpp::unique(actor1_and_actor2);
    actorName.sort(); // sorting actors
    if (std::find(actorName.begin(), actorName.end(), "") != actorName.end())
    {
        Rcpp::stop(errorMessage(3));
    }
    out["N"] = actorName.length(); // number of actors
    N = actorName.length(); 

    // Finding unique strings in event types 
    Rcpp::StringVector typeName;
    if(out["with_type"]){
        Rcpp::StringVector type = edgelist["type"];
        arma::uword types_vector_length = 0;
        Rcpp::StringVector types_vector;

        if(!Rf_isNull(types)){
            types_vector = Rcpp::as<Rcpp::StringVector>(types);
            types_vector_length = types_vector.length();
        }

        Rcpp::StringVector vector_of_types(type.length()+types_vector_length);
        vector_of_types[Rcpp::Range(0,type.length()-1)] = type;

        if(!Rf_isNull(types)){
            vector_of_types[Rcpp::Range(type.length(),vector_of_types.length()-1)] = types_vector;
        }

        typeName = Rcpp::unique(vector_of_types);
        typeName.sort(); // sorting types
        if (std::find(typeName.begin(), typeName.end(), "") != typeName.end())
        {
            Rcpp::stop(errorMessage(3));
        }
        C = typeName.length();  
        out["C"] = C;
        if(C == 1){
            out["C"] = R_NilValue;;
        }
    } 
    else{
        C = 1;
        out["C"] = R_NilValue;
    }
    if(C == 1){
        out["with_type"] = false;
    }
    
    // How many (possible) dyads? if `directed` N*(N-1), N*(N-1)/2 otherwise
    if(directed){
        D = N*(N-1)*C;
    }
    else{
        D = ((N*(N-1))/2)*C;
    }


    // Creating a dictionary for actors and event types, that is like: 'string_name' = integer (IDentifier)
    Rcpp::DataFrame actorsDictionary = Rcpp::DataFrame::create(Rcpp::Named("actorName") = actorName, Rcpp::Named("actorID") = Rcpp::Range(1,N)); 
    out["actorsDictionary"] = actorsDictionary;
    
    Rcpp::DataFrame typesDictionary;
    if(out["with_type"]){
        typesDictionary = Rcpp::DataFrame::create(Rcpp::Named("typeName") = typeName, Rcpp::Named("typeID") = Rcpp::Range(1,C)); 
        out["typesDictionary"] = typesDictionary;
    }

    // Processing time variable, converting input edgelist and omit_dyad list according to the new id's for both actors and event types
    Rcpp::List convertedInput = convertInputREH(edgelist,origin,actorsDictionary,typesDictionary,M,D,directed,omit_dyad,model,out["weighted"],ordinal,C,active,ncores);

    out["warnings"] = convertedInput["warnings"]; // exporting warnings list
    out["D"] = D; // number of dyads (this dimension is the largest possible and doesn't account for the dynamic riskset)
    out["dyad"] = convertedInput["dyad"];
    out["actor1_ID"] = convertedInput["actor1_ID"];
    out["actor2_ID"] = convertedInput["actor2_ID"];
    if(out["with_type"]){
        out["type_ID"] = convertedInput["type_ID"];
    }
    out["edgelist"] = convertedInput["edgelist"];
    out["M"] = convertedInput["M"]; // if there are self-loops the number of events decreases
    out["omit_dyad"] = convertedInput["omit_dyad"];
    out["intereventTime"] = convertedInput["intereventTime"];
    out["evenly_spaced_interevent_time"] = convertedInput["evenly_spaced_interevent_time"];
    out["rows_to_remove"] = convertedInput["rows_to_remove"];
    out["order"] = convertedInput["order"];

    // END of the processing and returning output
    return out;
}



// @title getEventsComposition
//
// @details this function can be seen as a wrapper of the function remify::getDyadComposition (in the header remify.h) and it returns the composition in the form of [actor1_ID,actor2_ID,(type_ID)] given a vector of dyads' ID (only suited for remify objects) supplied as input
//
// @param dyads vector of dyads' ID ranging from 1 to D 
// @param N number of actors in the network (from the remify object)
// @param D maximum number of dyads (from the remify object))
// @param directed directed network (TRUE), or undirected network (FALSE) - from the remify object
// @param ncores number of cores used in the parallelization of the procedure
//
// @return \code{data.frame} whose columns are rearranged according to the input index
// [[Rcpp::export]]
Rcpp::IntegerMatrix getEventsComposition(arma::vec dyads, 
                                int N, 
                                int D,
                                bool directed,
                                int ncores) {
    arma::uword d;
    arma::uword length_dyads = dyads.n_elem;
    Rcpp::IntegerMatrix out(length_dyads,3);

    #ifdef _OPENMP
    omp_set_dynamic(0);         
    omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
    #pragma omp parallel for if(ncores>1) private(d) shared(length_dyads,D,out,N,directed)
    #endif
    for(d = 0; d < length_dyads; d++){
        if((dyads(d) < 1) || (dyads(d) > D)){
            out(d,Rcpp::_) = Rcpp::IntegerVector::create(NA_INTEGER,NA_INTEGER,NA_INTEGER);
        }
        else{
            out(d,Rcpp::_) = remify::getDyadComposition(dyads(d)-1, N,directed)+1;
        }
    }

    return out;
}


// @title getDyadIndex_cpp
//
// @details this function is a wrapper to the function getDyadID in the header remify.h
//
// @param actor1 id of actor1 (ranging between 1 and N)
// @param actor2 id of actor2 (ranging between 1 and N)
// @param type id of type (ranging between 1 and C)
// @param N number of actors in the network
// @param directed directed network (TRUE), undirected network (FALSE)
//
// @return dyad ID according to remify::remify() ranging between 1 and D
//
// [[Rcpp::export]]
int getDyadIndex_cpp(double actor1, double actor2, double type, int N, bool directed){
    return remify::getDyadIndex(actor1-1,actor2-1,type-1,N,directed)+1;
}



// /////////////////////////////////////////////////////////////////////////////////
// ////////////(END)             remify C++ functions              (END)////////////
// /////////////////////////////////////////////////////////////////////////////////


// /////////////////////////////////////////////////////////////////////////////////
// /////////(BEGIN)             rehshape C++ functions              (BEGIN)/////////
// /////////////////////////////////////////////////////////////////////////////////

// @title remify2relventrem
//
// @details more details can be found at the following documentation: \link[remify]{reh}.
// 
// @param actor1
// @param actor2
// @param type
// @param dyad
// @param M
// @param N
// @param D
// @param with_type
// @param directed
// @param model
// @param omit_dyad
// @param ncores
//
// @return list of objects with processed raw data.
//
// [[Rcpp::export]]
Rcpp::List remify2relventrem(arma::vec actor1,
                            arma::vec actor2,
                            arma::vec type,
                            arma::vec dyad,
                            arma::uword M,
                            arma::uword N,
                            arma::uword D,
                            bool with_type,
                            bool directed,
                            std::string model,
                            Rcpp::List omit_dyad,
                            int ncores = 1){

    arma::uword m;
    Rcpp::List out = Rcpp::List::create(); // output list
    // (1) processing the edgelist
    Rcpp::DataFrame eventlist;
    //eventlist <- matrix(NA,nrow=data$M,ncol=2)
    if(model == "tie"){
        eventlist = Rcpp::DataFrame::create(Rcpp::Named("dyad") = dyad);
        out["eventlist"] = eventlist;
    }   
    else{
        arma::vec dyad_loc(M);
        if(with_type){ // if the remify object is processed for actor-oriented modeling, then we have to find the dyad ID 
            #ifdef _OPENMP
            omp_set_dynamic(0);
            omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
            #pragma omp parallel for if(ncores>1) private(m) shared(M,actor1,actor2,type,dyad_loc)
            #endif
            for(m = 0; m < M; m++){
                dyad_loc(m) = remify::getDyadIndex(actor1(m),actor2(m),type(m),N,directed)+1;
            }
        }
        else{
            #ifdef _OPENMP
            omp_set_dynamic(0);         
            omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
            #pragma omp parallel for if(ncores>1) private(m) shared(M,actor1,actor2,dyad_loc)
            #endif
            for(m = 0; m < M; m++){
                dyad_loc(m) = remify::getDyadIndex(actor1(m),actor2(m),0,N,directed)+1;
            }
        }
        eventlist = Rcpp::DataFrame::create(Rcpp::Named("dyad") = dyad_loc );
        out["eventlist"] = eventlist;
    }
    

    // (2) converting omit_dyad output object to the 'supplist' argument in relevent::rem()
    arma::umat supplist(M,D,arma::fill::ones);
    if(omit_dyad.size()>1){
        
        // add parallelization here
        arma::vec omit_dyad_time = Rcpp::as<arma::vec>(omit_dyad["time"]);
        arma::umat omit_dyad_riskset = Rcpp::as<arma::umat>(omit_dyad["riskset"]);

        #ifdef _OPENMP
        omp_set_dynamic(0);         
        omp_set_num_threads(ncores); // number of threads for all consecutive parallel regions
        #pragma omp parallel for if(ncores>1) private(m) shared(M,omit_dyad_time,omit_dyad_riskset,supplist)
        #endif
        for(m = 0; m < M; m++){
        if(omit_dyad_time(m)!=(-1)){
            supplist.row(m) = omit_dyad_riskset.row(omit_dyad_time(m));
        }
        }
    }
    out["supplist"] = supplist;


    // return processed eventlist and supplist
    return out;
}

// /////////////////////////////////////////////////////////////////////////////////
// ///////////(END)             rehshape C++ functions              (END)///////////
// /////////////////////////////////////////////////////////////////////////////////
