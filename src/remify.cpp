#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <iostream>
#include <typeinfo>
#include <map>
#include <iterator>
#include <string>
#include <algorithm> 
#include "remify.h"
#include "messages.h"



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
        case INTSXP:{
            Rcpp::IntegerVector column_j = Rcpp::as<Rcpp::IntegerVector>(x_j);
            Rcpp::IntegerVector column_j_loc = Rcpp::clone(column_j);
            for(m = 0; m < x.nrows(); m++){
                arma::uword m_new = index(m); 
                column_j[m] = column_j_loc[m_new];
            }
            break;}
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





// @title getIntereventTime
//
// @param time first column of the edgelist (time variable) 
// @param origin origin time point (t_0)
// @param ordinal boolean value  
//
// @return list object with: interevent time (object name 'value') and the order of the time variable if it changed (object name 'order').
Rcpp::List getIntereventTime(Rcpp::RObject time,
                             Rcpp::RObject origin,
                             bool ordinal) {
    // intereventTime variable (calculating the waiting time between two subsequent events). If `ordinal = TRUE` do nothing.
    Rcpp::List out = Rcpp::List::create(); // output list
    arma::vec time_input = Rcpp::as<arma::vec>(time);
    arma::vec intereventTime(time_input.n_elem,arma::fill::ones);   

    if(!ordinal){
        int force_sorting = 0; // 0 = No, 1 = Yes
        int force_even_spacing = 0; // 0 = No, 1 = Yes
        arma::uword m = 0;

        // (1) Checking whether the origin is NULL or it is defined by the user (time variable is not yet sorted if needed, therefore we work with time_input.min() value)
        arma::vec origin_input(1, arma::fill::zeros);
        arma::vec time_vector(time_input.n_elem+1,arma::fill::zeros);
        if(time_input.min() >= 0){
            if(Rf_isNull(origin)){ // if origin input is NULL
                origin_input(0) = time_input.min() - 1; // if in seconds event_0 will occur one second earlier than event_1, if in days it will be one day earlier
                if(origin_input(0) < 0) origin_input(0) = 0; // if the supplied `time` is a vector of either integers or doubles, this case might be true and then t_0 = 0
            }
            else{ // otherwise store check the input value and store it
                arma::vec origin_loc = Rcpp::as<arma::vec>(origin);
                if(origin_loc(0) >= time_input.min()){ // check if the supplied origin has the same value of the first time point (throw a warning and change the value in the same way when origin is NULL)
                    Rcpp::Rcout << warningMessage(2);
                    origin_input(0) = time_input.min() - 1; // setting the origin to a second/day earlier
                    if(origin_input(0) < 0) origin_input(0) = 0; // setting the origin to zero (if the previous value generated a negative time)
                }
                else{
                    origin_input(0) = origin_loc(0);
                }
            }
            time_vector = arma::join_cols(origin_input,time_input);
        }
        else{
            Rcpp::stop(errorMessage(5)); // time variable can't be negative
        }
        

        // (2) Check if the `time` variable is sorted
        while(m < intereventTime.n_elem){
            if(time_vector(m) <= time_vector(m+1)){
                intereventTime(m) = time_vector(m+1) - time_vector(m); // compute the interevent time
                m++;
            }
            else{
                force_sorting = 1; 
                m = intereventTime.n_elem;     
            }
        }
        // (2.1) Force the sorting of `time` if force_sorting = 1
        if(force_sorting == 1){
            Rcpp::Rcout << warningMessage(0); // warning message about the sorting operation
            out["order"] = arma::sort_index(time_vector(arma::span(1,time_vector.n_elem-1))); // excluding the origin in the ordering because it is used only for the computation of the intereventTime
            time_vector = arma::sort(time_vector); 
            intereventTime = arma::diff(time_vector);
        }

        // (3) Check if there are events occurred at the same time point
        m = 1;     // we skip m=0 which is the origin because there can be cases in which origin value is 0 and time_input(0) is 0 as well so the intereventTime results zero
        while(m < intereventTime.n_elem){
            if(intereventTime(m) != 0.0) m++;
            else{
                force_even_spacing = 1;
                m = intereventTime.n_elem;
            }
        }

        // (3.1) Force even spacing of events if force_even_spacing = 1
        if(force_even_spacing == 1){
            Rcpp::Rcout << warningMessage(1); // warning message about co-occurrence of events
            arma::uword position_first_zero = intereventTime.n_elem + 1;
            arma::uword counter = 0; // number of events to which evenly span the time
            for(m = 0; m < intereventTime.n_elem; m++){ // seeking for intereven times values equal to zero
                if((intereventTime(m) == 0.0)){ 
                    if(position_first_zero == intereventTime.n_elem + 1){ // if it is the first time point found with waiting time zero: save position and increment counter
                        position_first_zero = m;
                        counter++;
                        if(m == (intereventTime.n_elem-1)){ // when [(x),0] the last two events occurred at the same time
                            // apply the forced spacing
                            counter++; // adding one more event (x) that has to take part in the spacing 
                            arma::vec time_spacing(m-position_first_zero+1);
                            time_spacing.fill(1.0/counter); // operation that makes events equally spaced
                            intereventTime(arma::span(position_first_zero,m)) = time_spacing;
                            break;
                        }
                    }
                    else{
                        counter++;
                        if(m == (intereventTime.n_elem-1)){ // when [(x),0,0,...,0], thus the last waiting time is zero as well
                            // apply the forced spacing 
                            counter++; // adding one more event (x) that has to take part in the spacing 
                            arma::vec time_spacing(m-position_first_zero+1);
                            time_spacing.fill(1.0/counter); // operation that makes events equally spaced
                            intereventTime(arma::span(position_first_zero,m)) = time_spacing;
                            break;
                        }
                    }
                }
                else{ 
                    if(position_first_zero != intereventTime.n_elem + 1){ // if the waiting time is not zero and the position_first_zero is not the default. Case when [(x),0,0,..,0,y]
                        // increment the counter 
                        counter++; // adding one more event (x) that has to take part in the spacing 

                        // apply the forced spacing when [(x),0,0,..,0,y], applied only to [(x),0,0,...,0]
                        arma::vec time_spacing(m-position_first_zero);
                        time_spacing.fill(1.0/counter); // operation that makes events equally spaced
                        intereventTime(arma::span(position_first_zero,m-1)) = time_spacing;
                        intereventTime(m) = intereventTime(m) - (1.0-1.0/counter);
                        
                        // reset values of both position_first_zero and counter
                        position_first_zero = intereventTime.n_elem + 1;
                        counter = 0;
                    }
                }
            }
        }
    }

    out["value"] = intereventTime;
    return out;
}


// @title getRisksetSender
//
// @param which_dyad is list of matrices where each matrix defines by row [actor1,actor2,type] to be removed from the riskset. Each matrix as a whole will finally produce a vector (length = D) of 1/0 with 0's for dyads that have to be excluded from the riskset
// @param C number of event types
// @param D number of dyads
// @param N number of actors
//
// @return utility matrix per row 0 if the event could happen but didn't, 1 if the event happend, -1 if the event couldn't occur
Rcpp::IntegerMatrix getRisksetSender(Rcpp::List which_dyad,
                                        int C,
                                        int D, 
                                        int N) {
    arma::uword z,d,D_z;
    int j,c;
    arma::uword Z = which_dyad.size();
    Rcpp::IntegerMatrix riskset(Z,N);
    riskset.fill((N-1)*C);
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
                            riskset(z,actor1_z(d)) -= 1; 
                        }
                    }   
                }
                else{
                    if(!actor1_na(d)){ // when [X,NA,NA] 
                        for(c = 0; c < C; c++){  // for all the event types
                            for(j = 0; j < N; j++){ // for all the receivers excluding the self-edge
                                if(j != actor1_z(d)){
                                    riskset(z,actor1_z(d)) -= 1; 
                                }
                            }  
                        }
                    }
                }
            }
            else{ // when [?,?,C] (type is defined)       
                if(!actor1_na(d) && !actor2_na(d)){ // when [X,Y,C]
                    riskset(z,actor1_z(d)) -= 1; 
                }
                else{
                    if(!actor1_na(d)){ // when [X,NA,C]          
                        for(j = 0; j < N; j++){ // for all the receivers excluding the self-edges
                            if(j != actor1_z(d)){
                                riskset(z,actor1_z(d)) -= 1; 
                            }  
                        }
                    }
                }                   
            }
        }
    }
    auto is_gtzero = [](int &k) {k = (k > 0);}; // we set k>0 (instead of == 0) because there can be negative integers due to the multiple definition of the same dyad (this should not happen when the user defines dyads to omit in a clean way)
    std::for_each(riskset.begin(),riskset.end(),is_gtzero); 
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
// @return utility matrix per row 0 if the event could happen but didn't, 1 if the event happend, -1 if the event couldn't occur
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
                            dyad_z_d = getDyadIndex(actor1_z(d),actor2_z(d),c,N,directed);
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
                                    dyad_z_d = getDyadIndex(actor1_z(d),j,c,N,directed);
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
                                    dyad_z_d = getDyadIndex(i,actor2_z(d),c,N,directed);
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
                                dyad_z_d = getDyadIndex(i,j,type_z(d),N,directed);
                                riskset(z,dyad_z_d) = 0;
                            }
                        }  
                    }
                }
                else{    
                    if(!actor1_na(d) && !actor2_na(d)){ // when [X,Y,C]
                        int dyad_z_d = getDyadIndex(actor1_z(d),actor2_z(d),type_z(d),N,directed);
                        riskset(z,dyad_z_d) = 0;
                    }
                    else{
                        if(!actor1_na(d)){ // when [X,NA,C]          
                            int dyad_z_d;
                            for(j = 0; j < N; j++){ // for all the receivers excluding the self-edges
                                if(j != actor1_z(d)){
                                    dyad_z_d = getDyadIndex(actor1_z(d),j,type_z(d),N,directed);
                                    riskset(z,dyad_z_d) = 0;
                                }  
                            }
                        }
                        else{ // when [NA,Y,C] 
                            int dyad_z_d;
                            for(i = 0; i < N; i++){ // for all the receivers excluding the self-edges
                                if(i != actor2_z(d)){
                                    dyad_z_d = getDyadIndex(i,actor2_z(d),type_z(d),N,directed);
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
        timeID_mat(r,Rcpp::_) = timeID_r; // check this
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
    for(z = 0; z < lb.size(); z++){
        if((lb[z]!=ub[z]) | ((lb[z]==timeID[0]) | (ub[z]==timeID[timeID.size()-1]))){
            // lower bound
            int lb_z = lb[z];
            if(std::any_of(ub.begin(),ub.end(), [&lb_z](int i){return i == lb_z;})){
                lb[z] += 1;
            }
            // upper bound
            int ub_z = ub[z];
            if(std::any_of(lb.begin(),lb.end(), [&ub_z](int i){return i == ub_z;})){
                ub[z] -= 1;
            }
        }
    }
    
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
    if((model == "actor") && (directed == true)){
        Rcpp::IntegerMatrix riskset_sender = getRisksetSender(which_dyad,C,D,N);
        out["risksetSender"] = riskset_sender;
    }
    else if((model == "actor") && (directed == false)){
        Rcpp::stop(errorMessage(4));
    }

    return out;
}

// @title convertInputREH
//
// @param edgelist is the input data frame with information about [time,actor1,actor2,type,weight] by row.
// @param actorsDictionary dictionary of actor names 
// @param typesDicitonary dictionary of event types 
// @param M number of observed relational events
// @param D number of possible dyads
// @param direcred boolean value: are events directed (1) or undirected (0)?
// @param omit_dyad list. The same input in rehCpp.
// @param model, "tie" or "actor" oriented
//
// @return cube of possible combination [actor1,actor2,type]: the cell value is the column index in the rehBinary matrix
Rcpp::List convertInputREH(Rcpp::DataFrame edgelist, Rcpp::DataFrame actorsDictionary, Rcpp::DataFrame typesDictionary, arma::uword M, arma::uword D, bool directed, Rcpp::List omit_dyad, std::string model) {
    // for loop iterators
    arma::uword m,r,z,d,R,Z_r,D_r,D_rr;
    // counter for warningMessages
    int undefined_dyad = 0;
    // Creating output list object
    std::vector<int> dyad(M);
    Rcpp::List out = Rcpp::List::create();
        
    // edgelist input 
    std::vector<double> time = Rcpp::as<std::vector<double>>(edgelist["time"]);
    std::vector<std::string> stringActor1 = Rcpp::as<std::vector<std::string>>(edgelist["actor1"]);
    std::vector<std::string> stringActor2 = Rcpp::as<std::vector<std::string>>(edgelist["actor2"]);
    std::vector<std::string> stringType = Rcpp::as<std::vector<std::string>>(edgelist["type"]);

    // strings in the dictionaries
    std::vector<std::string> actorName = Rcpp::as<std::vector<std::string>>(actorsDictionary["actorName"]);
    int N = actorName.size(); // number of actors
    std::vector<int> actorID = actorsDictionary["actorID"];
    std::vector<std::string> typeName = Rcpp::as<std::vector<std::string>>(typesDictionary["typeName"]);
    std::vector<int> typeID = typesDictionary["typeID"];

    // (1) Converting `edgelist`
    for(m = 0; m < M; m++){
        // m-th event in the edgelist input:
        if(stringActor1[m].compare(stringActor2[m]) != 0){ // when actor1 is different than actor2
        // find actor1
        std::vector<std::string>::iterator i = std::find(actorName.begin(), actorName.end(), stringActor1[m]);
        int convertedActor1_m = actorID.at(std::distance(actorName.begin(), i));

        // find actor2
        std::vector<std::string>::iterator j = std::find(actorName.begin(), actorName.end(), stringActor2[m]);
        int convertedActor2_m = actorID.at(std::distance(actorName.begin(), j));

        // find type 
        std::vector<std::string>::iterator c = std::find(typeName.begin(), typeName.end(), stringType[m]);
        int convertedType_m = typeID.at(std::distance(typeName.begin(), c));

        // getting dyad index
        dyad[m] = getDyadIndex(convertedActor1_m,convertedActor2_m,convertedType_m,N,directed);    
        }
        else{
            Rcpp::stop(errorMessage(1)); // self-events are not supported yet, throwing an error message
        } 
    }

    Rcpp::DataFrame convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = edgelist["time"],
    Rcpp::Named("dyad") = dyad, 
    Rcpp::Named("weight") = edgelist["weight"]);


    // (2) Storing converted `edgelist`
    out["edgelist"] = convertedEdgelist; 

    // (3) Converting `omit_dyad` list
    if(omit_dyad.length()>0){
        Rcpp::List convertedOmitDyad = Rcpp::List::create(); // r-th list with matrix inputs converted into IDs
        Rcpp::List convertedOmitDyad_time = Rcpp::List::create(); // r-th list with time inputs converted into IDs
        int N = actorName.size();
        int C = typeName.size();
        R = omit_dyad.length();

        for(r = 0; r < R; r++){
            // converting r-th element in omit_dyad
            Rcpp::List omit_r = omit_dyad[r]; // r-th input of `omit_dyad`

            // (1) converting vector of time points
            std::vector<double> time_r = Rcpp::as<std::vector<double>>(omit_r["time"]);
            std::vector<int> timeID_r;
            Z_r = time_r.size();

            
            if(Z_r != 2){
                // [[Rcpp::stop]] if time_r has size different than 2, stop the function
                Rcpp::stop(errorMessage(2));
            }

            for(z = 0 ; z < Z_r; z++){
                if(Rcpp::NumericVector::is_na(time_r[z])){
                    if(z == 0){
                        timeID_r.push_back(0); // if (NA,xx), NA is set to 0
                    }
                    if(z == 1){
                        timeID_r.push_back(M-1); // if (xx,NA), NA is set to M-1
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
                }
            }
            else{
                // [[Rcpp::stop]] if one of the times provided in the input is not found, stop the function
                Rcpp::stop(errorMessage(3));
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
            
            // sorting actor1 and actor2 if directed FALSE 
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
            Rcpp::Rcout << warningMessage(3); // when at least one actor supplied in omit_dyad was not found in the edgelist
        }

        //(4) processing (converted to id's and to -1 when NA) omit_dyad
        Rcpp::List outOmitDyad = processOmitDyad(convertedOmitDyad,convertedOmitDyad_time,M,C,D,N,directed,model);

        // (??) Storing the converted `omit_dyad`
        out["omit_dyad"] = outOmitDyad;

    }
    else{ // If the input list `omit_dyad` is NULL, then return a NULL value
        out["omit_dyad"] = R_NilValue;
        }
                                                 
    return out;
}



// @title rehCpp (the Rcpp alias of \code{reh()})
//
// @details more details can be found at the following documentation: \link[remify]{reh}.
// 
// @param edgelist an object of class \code{"\link[base]{data.frame}"} or 
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
//
// @return list of objects with processed raw data.
//
// [[Rcpp::export]]
Rcpp::List rehCpp(Rcpp::DataFrame edgelist, 
                  Rcpp::RObject actors, 
                  Rcpp::RObject types,  
                  bool directed,
                  bool ordinal,
                  Rcpp::RObject origin,
                  Rcpp::List omit_dyad,
                  std::string model) {

    // Allocating memory for some variables and the output list
    arma::uword D; // number of dyads which depends on the directed input value 
    Rcpp::List out = Rcpp::List::create(); // output list

    // START of the processing

    // Converting (overwriting) actor1, actor2, type, weight columns to StringVector or NumericVector (process [3-4] columns when they miss here)

    // actor1
    edgelist["actor1"] = Rcpp::as<Rcpp::StringVector>(edgelist["actor1"]);

    // actor2 
    edgelist["actor2"] = Rcpp::as<Rcpp::StringVector>(edgelist["actor2"]);

    // type
    out["with_type"] = false;
    if(!edgelist.containsElementNamed("type")){ // if type is not defined, one event type `0` is created for
        Rcpp::DataFrame edgelist_loc = Rcpp::clone(edgelist);
        Rcpp::StringVector one_type_vector(edgelist.nrows(),"0");
        edgelist_loc.push_back(one_type_vector,"type");
        edgelist = Rcpp::as<Rcpp::DataFrame>(edgelist_loc);
    }
    else{
        edgelist["type"] = Rcpp::as<Rcpp::StringVector>(edgelist["type"]); 
    }

    // weight
    out["weighted"] = false;
    if(!edgelist.containsElementNamed("weight")){ // if weight is not defined
        Rcpp::DataFrame edgelist_loc = Rcpp::clone(edgelist);
        Rcpp::NumericVector one_weight_vector(edgelist.nrows(),1.0);
        edgelist_loc.push_back(one_weight_vector,"weight");
        edgelist = Rcpp::as<Rcpp::DataFrame>(edgelist_loc);
    }
    else{
        edgelist["weight"] = Rcpp::as<Rcpp::NumericVector>(edgelist["weight"]); 
    }

    // processing `time` variable
    Rcpp::List intereventTime = getIntereventTime(edgelist["time"],origin,ordinal);
    out["intereventTime"] = intereventTime["value"];
    // Reordering `edgelist` if `intereventTime` was sorted
    if(intereventTime.containsElementNamed("order")){  
        arma::uvec new_order = intereventTime["order"];
        // reordering edgelist
        edgelist = rearrangeDataFrame(edgelist,new_order);
    }

    // StringVector of actor1
    Rcpp::StringVector actor1 = edgelist["actor1"]; // actor1/sender

    // StringVector of actor2
    Rcpp::StringVector actor2 = edgelist["actor2"]; // actor2/receiver

    //StringVector of actor1 and actor2 
    Rcpp::StringVector actor1_and_actor2(actor1.length()+actor2.length());
    actor1_and_actor2[Rcpp::Range(0,(actor1.length()-1))] = actor1;
    actor1_and_actor2[Rcpp::Range(actor1.length(),(actor1_and_actor2.length()-1))] = actor2;
    if(!Rf_isNull(actors)){
        Rcpp::StringVector actors_vector = Rcpp::as<Rcpp::StringVector>(actors);
        arma::uword N_loc = actors_vector.length();
        for(arma::uword n = 0; n < N_loc; n++){
            actor1_and_actor2.push_back(actors_vector[n]);
        } 
    } 
    // Finding unique strings in actor1_and_actor2 
    Rcpp::StringVector actorName = Rcpp::unique(actor1_and_actor2);
    actorName.sort(); // sorting actors

    // Finding unique strings in event types 
    Rcpp::StringVector vector_of_types = edgelist["type"];
    if(!Rf_isNull(types)){
        Rcpp::StringVector types_vector = Rcpp::as<Rcpp::StringVector>(types);
        arma::uword C_loc = types_vector.length();
        for(arma::uword c = 0; c < C_loc; c++){
            vector_of_types.push_back(types_vector[c]);
        } 
    } 
    Rcpp::StringVector typeName = Rcpp::unique(vector_of_types);
    typeName.sort(); // sorting types
    
    // Storing some useful dimensions
    out["M"] = edgelist.nrows(); // number of events
    out["N"] = actorName.length(); // number of actors
    out["C"] = typeName.length(); // number of events types
    
    // How many (possible) dyads? if `directed` N*(N-1), N*(N-1)/2 otherwise
    if(directed){
        D = actorName.length()*(actorName.length()-1)*typeName.length();
    }
    else{
        D = ((actorName.length()*(actorName.length()-1))/2)*typeName.length();
    }
    out["D"] = D; // number of dyads (this dimension is the largest possible and doesn't account for the dynamic riskset)

    // Are there more than one event type? (if event type is only one, then no event types are considered)
    if(typeName.length() > 1) out["with_type"] = true;

    // Is the network weighted?
    Rcpp::NumericVector event_weights = edgelist["weight"]; 
    Rcpp::NumericVector unique_weights = Rcpp::unique(event_weights);
    if(unique_weights.length() > 1) out["weighted"] = true;   

    // Creating a dictionary for actors and event types, that is like: 'string_name' = integer (IDentifier)
    Rcpp::DataFrame actorsDictionary = Rcpp::DataFrame::create(Rcpp::Named("actorName") = actorName, Rcpp::Named("actorID") = Rcpp::Range(0,actorName.length()-1)); 
    out["actorsDictionary"] = actorsDictionary;
    
    Rcpp::DataFrame typesDictionary = Rcpp::DataFrame::create(Rcpp::Named("typeName") = typeName, Rcpp::Named("typeID") = Rcpp::Range(0,typeName.length()-1)); 
    out["typesDictionary"] = typesDictionary;
  

    // Converting input edgelist and omit_dyad list according to the new id's for both actors and event types
    Rcpp::List convertedInput = convertInputREH(edgelist,actorsDictionary,typesDictionary,out["M"],out["D"],directed,omit_dyad,model);
    out["edgelist"] = convertedInput["edgelist"];
    out["omit_dyad"] = convertedInput["omit_dyad"]; 

    // END of the processing and returning output
    return out;
}
