#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <iostream>
#include <omp.h>
#include <typeinfo>
#include <map>
#include <iterator>
#include <string>
#include "messages.h"


#define LOG(x) std::cout << x << "\n"


//' rearrangeDataFrame
//'
//' @param x \code{data.frame} object to reorder
//' @param index vector with the new order
//'
//' @return \code{data.frame} whose columns are rearranged according to the input index
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
        default: {break;} //neither an INTSXP nor a REALSXP nor a STRSXP. The same input data.frame is returned           
        }
    }
    return x;
}





//' getIntereventTime
//'
//' @param time first column of the edgelist (time variable) 
//' @param origin origin time point (t_0)
//' @param ordinal boolean value  
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

        // (1) Checking whether the origin is NULL or it is defined by the user
        arma::vec origin_input(1, arma::fill::zeros);
        if(Rf_isNull(origin)){ // if origin input is NULL
            origin_input(0) = time_input(0) - 1; // if in seconds event_0 will occur one second earlier than event_1, if in days it will be one day earlier
            if(origin_input(0) < 0) origin_input(0) = 0; // if the supplied `time` is a vector of either integers or doubles, this case might be true and then t_0 = 0
        }
        else{ // otherwise store check the input value and store it
            arma::vec origin_loc = Rcpp::as<arma::vec>(origin);
            if(origin_loc(0) == time_input(0)){ // check if the supplied origin has the same value of the first time point (throw a warning and change the value in the same way when origin is NULL)
                Rcpp::Rcout << warningMessage(2);
                origin_input(0) = time_input(0) - 1; // setting the origin to a second/day earlier
                if(origin_input(0) < 0) origin_input(0) = 0; // setting the origin to zero (if the previous value generated a negative time)
            }
            else{
                origin_input(0) = origin_loc(0);
            }
        }
        arma::vec time_vector = join_cols(origin_input,time_input);

        // (2) Check if the `time` variable is sorted
        while(m < intereventTime.n_elem){
            if(time_vector(m) <= time_vector(m+1)){
                intereventTime(m) = time_vector(m+1) - time_vector(m); // compute the interevent time
                m++;
            }
            else{
                force_sorting = askYesNoQuestion(warningMessage(0));
                if(force_sorting == 1) {m = intereventTime.n_elem;}
                else{Rcpp::stop("");}
            }
        }
        // (2.1) Force the sorting of `time` if force_sorting = 1
        if(force_sorting == 1){
            out["order"] = arma::sort_index(time_vector);
            time_vector = sort(time_vector);
            intereventTime = arma::diff(time_vector);
        }

        // (3) Check if there are events occurred at the same time point
        m = 0;
        while(m < intereventTime.n_elem){
            if(intereventTime(m) != 0.0) m++;
            else{
                force_even_spacing = askYesNoQuestion(warningMessage(1));
                if(force_even_spacing == 1) {m = intereventTime.n_elem;}
                else{Rcpp::stop("");}
            }
        }

        // (3.1) Force even spacing of events if force_even_spacing = 1
        if(force_even_spacing == 1){
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





//' getRisksetMatrix (obtain permutations of actors' ids and event types).
//'
//' @param actorID vector of actors' id's.
//' @param typeID vector of types' id's.
//' @param N number of actors in the dataset.
//' @param C number of event types
//' @param direcred boolean value: are events directed (1) or undirected (0)?
//'
//' @return matrix of possible dyadic events.
arma::mat getRisksetMatrix(arma::uvec actorID, arma::uvec typeID, arma::uword N, arma::uword C, bool directed){
    switch(directed){
    case 0: { // for undirected network
        arma::uword i,j,c;
        arma::uword col_index = 0;
        arma::mat riskset(((N*(N-1))/2)*C,4); 
        for(c = 0; c < C; c++){
            for(i = 0; i < N; i++){
                for(j = (i+1); j < N ; j++){
                        // unit increase col_index 
                        riskset(col_index,0) = actorID(i);
                        riskset(col_index,1) = actorID(j);
                        riskset(col_index,2) = typeID(c);
                        riskset(col_index,3) = col_index;
                        col_index += 1;       
                }
            }
        }
        return riskset; 
    }

    case 1: { // for directed network
        arma::uword i,j,c;
        arma::mat riskset(N*N*C,4);
        arma::uvec indices_to_shed(N*C); // this is the vector where to store the indices of selfedges to remove at the end of the function
        indices_to_shed.fill(N*N*C);
        for(c = 0; c < C; c++){
            for(i = 0; i < N; i++){
                for(j = 0; j < N ; j++){
                        if(j != i){
                        riskset(j+i*N+c*(N*N),0) = actorID(i);
                        riskset(j+i*N+c*(N*N),1) = actorID(j);
                        riskset(j+i*N+c*(N*N),2) = typeID(c);
                    }
                    else {
                        indices_to_shed(j+c*N) = (j+i*N+c*(N*N));
                    }
                }
            }
        }
        riskset.shed_rows(indices_to_shed); 
        riskset.col(3) = arma::linspace(0,riskset.n_rows-1,riskset.n_rows);
        return riskset;
    }
    }
}





//' getRisksetCube
//'
//' @param risksetMatrix output of getRiskset() function
//' @param N number of actors in the dataset.
//' @param C number of event types
//'
//' @return cube of possible combination [actor1,actor2,type]: the cell value is the column index in the rehBinary matrix
arma::ucube getRisksetCube(arma::umat risksetMatrix, arma::uword N, arma::uword C) {
    arma::uword d;
    arma::ucube risksetCube(N,N,C);
    risksetCube.fill(N*N*C); // this is just a number to fill the cube (selfedges at each event type will remain with this value)
    for(d = 0; d < risksetMatrix.n_rows; d++){
            risksetCube(risksetMatrix(d,0),risksetMatrix(d,1),risksetMatrix(d,2)) = risksetMatrix(d,3);
        }
    return risksetCube; 
}



//' convertInputREH
//'
//' @param edgelist is the input data frame with information about [time,actor1,actor2,type,weight] by row.
//' @param actorsDictionary dictionary of actor names 
//' @param typesDicitonary dictionary of event types 
//' @param M number of observed relational events
//' @param direcred boolean value: are events directed (1) or undirected (0)?
//' @param omit_dyad list. The same input in rehCpp.
//'
//' @return cube of possible combination [actor1,actor2,type]: the cell value is the column index in the rehBinary matrix
Rcpp::List convertInputREH(Rcpp::DataFrame edgelist, Rcpp::DataFrame actorsDictionary, Rcpp::DataFrame typesDictionary, arma::uword M, bool directed, Rcpp::List omit_dyad) {

    // for loop iterators
    arma::uword m,r,z,d,R,D,Z_r,D_r;
    // counters for warningMessages
    int time_not_observed = 0;
    int undefined_dyad = 0;
    // Creating output list object
    Rcpp::IntegerVector convertedActor1(M),convertedActor2(M),convertedType(M);
    Rcpp::List out = Rcpp::List::create();
        
    // edgelist input 
    std::vector<double> time = Rcpp::as<std::vector<double>>(edgelist["time"]);
    std::vector<std::string> stringActor1 = Rcpp::as<std::vector<std::string>>(edgelist["actor1"]);
    std::vector<std::string> stringActor2 = Rcpp::as<std::vector<std::string>>(edgelist["actor2"]);
    std::vector<std::string> stringType = Rcpp::as<std::vector<std::string>>(edgelist["type"]);

    // strings in the dictionaries
    std::vector<std::string> actorName = Rcpp::as<std::vector<std::string>>(actorsDictionary["actorName"]);
    std::vector<int> actorID = actorsDictionary["actorID"];
    std::vector<std::string> typeName = Rcpp::as<std::vector<std::string>>(typesDictionary["typeName"]);
    std::vector<int> typeID = typesDictionary["typeID"];

    // (1) Converting `edgelist`
    for(m = 0; m < M; m++){
        // m-th event in the edgelist input:

        // find actor1
        std::vector<std::string>::iterator i = std::find(actorName.begin(), actorName.end(), stringActor1[m]);
        int convertedActor1_m = actorID.at(std::distance(actorName.begin(), i));

        // find actor2
        std::vector<std::string>::iterator j = std::find(actorName.begin(), actorName.end(), stringActor2[m]);
        int convertedActor2_m = actorID.at(std::distance(actorName.begin(), j));

        // sorting actor1 and actor2 when directed = FALSE
        if(!directed){
            if(convertedActor1_m < convertedActor2_m){
                convertedActor1[m] = convertedActor1_m;
                convertedActor2[m] = convertedActor2_m;
            }
            else{
                convertedActor1[m] = convertedActor2_m;
                convertedActor2[m] = convertedActor1_m;
            }
        }
        else{ // when directed == TRUE (we do not sort) (actor1 = sender, actor2 = receiver)
            convertedActor1[m] = convertedActor1_m;
            convertedActor2[m] = convertedActor2_m;
        }

        // find type 
        std::vector<std::string>::iterator c = std::find(typeName.begin(), typeName.end(), stringType[m]);
        convertedType[m] = typeID.at(std::distance(typeName.begin(), c));
    }

    Rcpp::DataFrame convertedEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = edgelist["time"], 
                                                          Rcpp::Named("actor1") = convertedActor1,
                                                          Rcpp::Named("actor2") = convertedActor2,
                                                          Rcpp::Named("type") = convertedType,
                                                          Rcpp::Named("weight") = edgelist["weight"]);
    // (2) Storing converted `edgelist`
    out["edgelist"] = convertedEdgelist; 

    // (3) Converting `omit_dyad` list
    if(omit_dyad.length()>0){
        Rcpp::List convertedOmitDyad = Rcpp::List::create(); // this is the list object where to append each element of the converted list `omit_dyad`
        int N = actorName.size();
        R = omit_dyad.length();
        for(r = 0; r < R; r++){
            // converting r-th element in omit_dyad
            Rcpp::List omit_r = omit_dyad[r]; // r-th input of `omit_dyad`
            Rcpp::List convertedOmit_r = Rcpp::List::create(); // r-th list with inputs converted into IDs

            // (1) converting vector of time points
            std::vector<double> time_r = Rcpp::as<std::vector<double>>(omit_r["time"]);
            std::vector<int> timeID_r;
            Z_r = time_r.size();
    
            for(z = 0 ; z < Z_r; z++){
                std::vector<double>::iterator iterator_z = std::find(time.begin(), time.end(), time_r[z]); 
                if(iterator_z !=  time.end()){
                    timeID_r.push_back(iterator_z - time.begin());
                }
                else{
                    time_not_observed++;
                }
            }
            convertedOmit_r["time"] = timeID_r;
            // (2) converting `dyad` DataFrame according to the dictionaries of actors and types
            Rcpp::DataFrame dyad_r = Rcpp::as<Rcpp::DataFrame>(omit_r["dyad"]);
            D_r = dyad_r.nrow();
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
                if(((actor1_r[d] != "NA") & (iteratorActor1 == actorName.end())) || ((actor2_r[d] != "NA") & (iteratorActor2 == actorName.end())) ||((type_r[d] != "NA") & (iteratorType == typeName.end()))){
                    undefined_dyad++;
                    continue; // `continue` forces the for loop to continue with the next iteration
                }  
                else{ // all the three inputs [actor1,actor2,type] passed the check, therefore storing the ID's
                    // storing id actor1
                    if(actor1_r[d] != "NA"){
                        convertedActor1.push_back(iteratorActor1 - actorName.begin());
                    }
                    else{
                        convertedActor1.push_back(R_NaN);
                    }
                    // storing id actor2
                    if(actor2_r[d] != "NA"){
                        convertedActor2.push_back(iteratorActor2 - actorName.begin());
                    }
                    else{
                        convertedActor2.push_back(R_NaN);
                    }
                    // storing id type
                    if(type_r[d] != "NA"){
                        convertedType.push_back(iteratorType - typeName.begin());
                    }
                    else{
                        convertedType.push_back(R_NaN);
                    }
                }              
            }
            
            // sorting actor1 and actor2 if directed FALSE 
            // Rcpp::IntegerMatrix, then check actor1/actor2 and change in case the order, then create a dataframe
            Rcpp::IntegerMatrix actor_1_2(convertedActor1.length(),2);
            actor_1_2.column(0) = convertedActor1; 
            actor_1_2.column(1) = convertedActor2; 
            Rcpp::LogicalVector actor1_na = Rcpp::is_na(convertedActor1);
            Rcpp::LogicalVector actor2_na = Rcpp::is_na(convertedActor2);
            D = convertedActor1.length();    
            if(!directed){ 
                for(d = 0; d < D; d++){
                    if(!actor1_na(d) & !actor2_na(d)){ // both id actor1 and id actor2 are not NaN
                        if(actor_1_2(d,0) > actor_1_2(d,1)){
                            int actor1_loc = actor_1_2(d,0);
                            int actor2_loc = actor_1_2(d,1);
                            actor_1_2(d,0) = actor2_loc;
                            actor_1_2(d,1) = actor1_loc;
                        }
                    }
                    else{
                        if(actor1_na(d) & (actor_1_2(d,1) == 0)){
                                actor_1_2(d,0) = 0;
                                actor_1_2(d,1) = R_NaN;
                        }
                        else{
                            if((actor_1_2(d,0) == (N-1)) & actor2_na(d)){     // or make sure that ID = 0 is in actor1 or ID = N-1 is in actor2
                                    actor_1_2(d,0) = R_NaN;
                                    actor_1_2(d,1) = N-1;
                            }
                        }
                    }
                }
            }

            convertedOmit_r["dyad"] = Rcpp::DataFrame::create(Rcpp::Named("actor1") = actor_1_2.column(0), 
                                                                Rcpp::Named("actor2") = actor_1_2.column(1), 
                                                            Rcpp::Named("type") = convertedType);
            // (3) storing the r-th converted input into the output list convertedOmitDyad
            convertedOmitDyad.push_back(convertedOmit_r);
        }

        // Warning messages
        if(time_not_observed > 0){
            Rcpp::Rcout << warningMessage(3); // when at least one time point supplied in omit_dyad was not found in the edgelist
        }
        if(undefined_dyad > 0){
            Rcpp::Rcout << warningMessage(4); // when at least one actor supplied in omit_dyad was not found in the edgelist
        }
        // (4) Storing the converted `omit_dyad`
        out["omit_dyad"] = convertedOmitDyad;
    }
    else{ // If the input list `omit_dyad` is NULL, then return a NULL value
        out["omit_dyad"] = R_NilValue;
        }
                                                 
    return out;
}





//' getBinaryREH (a function that returns an utility matrix used in optimization algorithms)
//'
//' @param edgelist edgelist converted according to actorID and typeID
//' @param omit_dyad input list converted according to actorID and typeID, for handling the dynamic composition of the riskset
//' @param risksetCube arma::cube object [N*N*C] where the cell value returns the column index to use in the outBinaryREH
//' @param M number of observed relational events
//' @param D number of possible dyads (accounting for event types as well)
//'
//' @return utility matrix per row 0 if the event could happen but didn't, 1 if the event happend, -1 if the event couldn't occur
arma::mat getBinaryREH(Rcpp::DataFrame edgelist, Rcpp::List omit_dyad, arma::ucube risksetCube, arma::uword M, arma::uword D) {
    arma::uword m;
    arma::mat outBinaryREH(M,D,arma::fill::zeros); // by setting the initial values to zero we already handle those
                                                    // relational events that could have occurred but didn't
    Rcpp::IntegerVector actor1 = edgelist["actor1"];
    Rcpp::IntegerVector actor2 = edgelist["actor2"];
    Rcpp::IntegerVector type = edgelist["type"];

    // (1) occurred events ( = 1)
    for(m = 0; m < M; m++){
        // relational event that occurred
        arma::uword event_m = risksetCube(actor1[m],actor2[m],type[m]);
        outBinaryREH(m,event_m) = 1;
    }
   
    // (2) omitting relational events from the riskset ( = -1)
    if(omit_dyad.length()>0){
        
        arma::uword r,d,c,D_r; 
        arma::uword R = omit_dyad.length();
        arma::uword C = risksetCube.n_slices;
        for(r = 0; r < R; r++){
            Rcpp::List omit_dyad_r = Rcpp::as<Rcpp::List>(omit_dyad[r]);
            arma::uvec time_r = Rcpp::as<arma::uvec>(omit_dyad_r["time"]); // `time` : vector of index positions of time point where to apply the exclusion of the dyad
            Rcpp::DataFrame dyad_r = Rcpp::as<Rcpp::DataFrame>(omit_dyad_r["dyad"]); // data.frame of dyads to be ecluded at time points defined in `time`
            Rcpp::IntegerVector actor1_r = Rcpp::as<Rcpp::IntegerVector>(dyad_r["actor1"]);
            Rcpp::IntegerVector actor2_r = Rcpp::as<Rcpp::IntegerVector>(dyad_r["actor2"]);
            Rcpp::IntegerVector type_r = Rcpp::as<Rcpp::IntegerVector>(dyad_r["type"]);
            Rcpp::LogicalVector actor1_na = Rcpp::is_na(actor1_r);
            Rcpp::LogicalVector actor2_na = Rcpp::is_na(actor2_r);
            Rcpp::LogicalVector type_na = Rcpp::is_na(type_r);
            D_r = dyad_r.nrows();

            for(d = 0; d < D_r; d++){
                // (1) find case:
                if(type_na(d)){ // when [?,?,NA] (type is NA)
                    if(!actor1_na(d) & !actor2_na(d)){ // when [X,Y,NA]
                        arma::uvec dyad_r_d(1);
                        for(c = 0; c < C; c++){  // for all the event types
                            dyad_r_d(0) = risksetCube(actor1_r(d),actor2_r(d),c);
                            outBinaryREH(time_r,dyad_r_d).fill(-1); // for the specified time points in the `time` object
                        }   
                    }
                    else{
                        if(!actor1_na(d)){ // when [X,NA,NA] 
                            for(c = 0; c < C; c++){  // for all the event types
                                arma::umat slice_c = risksetCube.slice(c);
                                // transposing slice_c so as to be able to select senders by column
                                slice_c.t();
                                slice_c.shed_row(actor1_r(d)); // removing actor1 from the receivers (avoiding self-edfges)
                                arma::uvec dyad_r_d = slice_c.col(actor1_r(d));
                                outBinaryREH(time_r,dyad_r_d).fill(-1);
                            }
                        }
                        else{ // when [NA,Y,NA]
                            for(c = 0; c < C; c++){  // for all the event types
                                arma::umat slice_c = risksetCube.slice(c);
                                slice_c.shed_row(actor2_r(d)); // removing actor2 from the senders (avoiding self-edfges)
                                arma::uvec dyad_r_d = slice_c.col(actor2_r(d));
                                outBinaryREH(time_r,dyad_r_d).fill(-1);
                            }
                        }
                    }
                }
                else{ // when [?,?,C] (type is defined)
                    if(actor1_na(d) & actor2_na(d)){ // when [NA,NA,C]
                        arma::umat slice_r_d = risksetCube.slice(type_r(d));
                        arma::uvec upper_indices = arma::trimatu_ind(arma::size(slice_r_d),1); // upper triangular (excluding main diagonal)
                        arma::uvec lower_indices = arma::trimatl_ind(arma::size(slice_r_d),-1); // lower triangular (excluding main diagonal)
                        arma::uvec slice_indices = arma::join_cols(upper_indices,lower_indices); // concatenating vectors
                        arma::uvec dyad_r_d = slice_r_d(slice_indices);
                        outBinaryREH(time_r,dyad_r_d).fill(-1);
                    }
                    else{    
                        if(!actor1_na(d) & !actor2_na(d)){ // when [X,Y,C]
                            arma::uvec dyad_r_d(1);
                            dyad_r_d(0) = risksetCube(actor1_r(d),actor2_r(d),type_r(d));
                            outBinaryREH(time_r,dyad_r_d).fill(-1); // for the specified time points in the `time` object
                        }
                        else{
                            if(!actor1_na(d)){ // when [X,NA,C]          
                                arma::umat slice_r_d = risksetCube.slice(type_r(d));
                                // transposing slice_r_d so as to be able to select senders by column
                                slice_r_d.t();
                                slice_r_d.shed_row(actor1_r(d)); // removing actor1 from the receivers (avoiding self-edfges)
                                arma::uvec dyad_r_d = slice_r_d.col(actor1_r(d));
                                outBinaryREH(time_r,dyad_r_d).fill(-1);
                            }
                            else{ // when [NA,Y,C] 
                                arma::umat slice_r_d = risksetCube.slice(type_r(d));
                                slice_r_d.shed_row(actor2_r(d)); // removing actor2 from the senders (avoiding self-edfges)
                                arma::uvec dyad_r_d = slice_r_d.col(actor2_r(d));
                                outBinaryREH(time_r,dyad_r_d).fill(-1);
                            }
                        }
                    }                    
                }
            }
        }
    }

    return outBinaryREH;
}





//' rehCpp (the Rcpp alias of \code{reh()})
//'
//' @details more details can be found at the following documentation: \link[remify]{reh}.
//'
//' @export
// [[Rcpp::export]]
Rcpp::List rehCpp(Rcpp::DataFrame edgelist, 
                  Rcpp::RObject actors, 
                  Rcpp::RObject types,  
                  bool directed,
                  bool ordinal,
                  Rcpp::RObject origin,
                  Rcpp::List omit_dyad) {

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
    if(!edgelist.containsElementNamed("weight")){ // if type is not 
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

    // Creating riskset objects (it is not the rehBinary but it just includes all the possible combination of [actor1,actor2,type]) ...

    // ... arranged in a matrix [D*3]
    out["risksetMatrix"] = getRisksetMatrix(actorsDictionary["actorID"],typesDictionary["typeID"],out["N"],out["C"],directed);

    // ... arranged in a cube [N*N*C]
    out["risksetCube"] = getRisksetCube(out["risksetMatrix"],out["N"],out["C"]);

    // Converting input edgelist and omit_dyad list according to the new id's for both actors and event types
    Rcpp::List convertedInput = convertInputREH(edgelist,actorsDictionary,typesDictionary,out["M"],directed,omit_dyad);
    out["edgelist"] = convertedInput["edgelist"];
    Rcpp::List convertedOmit_dyad = convertedInput["omit_dyad"]; // perhaps remove from the (final) output (?)

    // Create event binary matrix from the riskset and the edgelist, that is `rehBinary`
    out["rehBinary"] = getBinaryREH(Rcpp::as<Rcpp::DataFrame>(out["edgelist"]),convertedOmit_dyad,out["risksetCube"],out["M"],out["D"]);
                                    
    // END of the processing and returning output
    return out;
}
