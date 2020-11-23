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
//' @param x `data.frame` object to reorder
//' @param index vector with the new order
//'
//' @return data.frame `x` whose columns are rearranged according to `index`
// [[Rcpp::export]]
Rcpp::DataFrame rearrangeDataFrame(Rcpp::DataFrame x, arma::uvec index) { // this function works only for numeric columns (but we are going to have StringVector as well)
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

//' rearrangeList
//'
//' @param x `'list` object to reorder
//' @param index vector with the new order
//'
//' @return list `x` rearranged according to `index` 
// [[Rcpp::export]]
Rcpp::List rearrangeList(Rcpp::List x, arma::uvec index) {
    Rcpp::List x_loc = Rcpp::clone(x);
    arma::uword j,j_new; 
    for(j = 0; j < x.size(); j++){
       j_new = index(j);
      x[j] = x_loc[j_new];
    }
    return x;
}

//' getIntereventTime
//'
//' @param time first column of the edgelist (time variable) 
//' @param ordinal 
//'
//' @return list of objects
// [[Rcpp::export]]
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
//' @param direcred are events directed or undirected?
//'
//' @return matrix of possible dyadic events.
//'
// [[Rcpp::export]]
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
//' @return cube of possible combination [sender,receiver,type]: the cell value is the column index in the rehBinary matrix
//'
// [[Rcpp::export]]
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
//' @param edgelist is the input data frame with information about [time,sender,receiver,type,weight] by row.
//' @param riskset riskset list with old actors sitring names.
//' @param actorsDictionary dictionary of actors names (input string name = integer id)
//' @param typesDicitonary dictionary of event types (input string name = integer id)
//' @param M number of observed relational events
//' @param directed boolean if the network is directed or not
//'
//' @return cube of possible combination [sender,receiver,type]: the cell value is the column index in the rehBinary matrix
//'
// [[Rcpp::export]]
Rcpp::List convertInputREH(Rcpp::DataFrame edgelist, Rcpp::List riskset, Rcpp::List actorsDictionary, Rcpp::List typesDictionary, arma::uword M, bool directed) {

    arma::uword m,d;
    Rcpp::IntegerVector outputSender(M),outputReceiver(M),outputType(M);
    Rcpp::List outRiskset = Rcpp::List::create();
    std::vector<std::string> out_list_names;
    if(directed){
        std::vector<std::string> names_directed = {"time","sender","receiver","type","weight"};
        out_list_names.insert(out_list_names.end(), std::begin(names_directed), std::end(names_directed)); 
    }
    else{
        std::vector<std::string> names_undirected = {"time","actor1","actor2","type","weight"};
        out_list_names.insert(out_list_names.end(), std::begin(names_undirected), std::end(names_undirected)); 
    }

    // Creating output list object
    Rcpp::List out = Rcpp::List::create();

    // edgelist input to be recoded
    std::vector<std::string> stringSender = Rcpp::as<std::vector<std::string>>(edgelist["actor1"]);
    std::vector<std::string> stringReceiver = Rcpp::as<std::vector<std::string>>(edgelist["actor2"]);
    std::vector<std::string> stringType = Rcpp::as<std::vector<std::string>>(edgelist["type"]);

    // strings in the dictionaries
    std::vector<std::string> actor = Rcpp::as<std::vector<std::string>>(actorsDictionary["actor"]);
    std::vector<int> actorID = actorsDictionary["actorID"];
    std::vector<std::string> type = Rcpp::as<std::vector<std::string>>(typesDictionary["type"]);
    std::vector<int> typeID = typesDictionary["typeID"];

    for(m = 0; m < M; m++){
        // (1) converting m-th event in the edgelist input
        // find sender
        std::vector<std::string>::iterator i = std::find(actor.begin(), actor.end(), stringSender[m]);
        int outputSender_m = actorID.at(std::distance(actor.begin(), i));
        // find receiver
        std::vector<std::string>::iterator j = std::find(actor.begin(), actor.end(), stringReceiver[m]);
        int outputReceiver_m = actorID.at(std::distance(actor.begin(), j));

        // sorting sender/actor1 and receiver/actor2 when directed = FALSE
        if(!directed){
            if(outputSender_m < outputReceiver_m){
                outputSender[m] = outputSender_m;
                outputReceiver[m] = outputReceiver_m;
            }
            else{
                outputSender[m] = outputReceiver_m;
                outputReceiver[m] = outputSender_m;
            }
        }
        else{ // when directed == TRUE (we do not sort)
            outputSender[m] = outputSender_m;
            outputReceiver[m] = outputReceiver_m;
        }
        // find type 
        std::vector<std::string>::iterator c = std::find(type.begin(), type.end(), stringType[m]);
        outputType[m] = typeID.at(std::distance(type.begin(), c));

        // (2) converting m-th object in the riskset input list
        if(!R_IsNaN(riskset[m])){ //
            // we expect a Rcpp::DataFrame (sender,receiver,type)
            Rcpp::StringMatrix omitDyads = riskset[m];

            // number of dyads to omit from the riskset at the m-th time point
            arma::uword D_m = omitDyads.nrow();

            // converting input senders, receivers and types to std::string vectors
            Rcpp::StringVector omitDyadsSender = omitDyads.column(0);
            Rcpp::StringVector omitDyadsReceiver = omitDyads.column(1);
            Rcpp::StringVector omitDyadsType = omitDyads.column(2);
            std::vector<std::string> stringOmitSender = Rcpp::as<std::vector<std::string>>(omitDyadsSender); // sender
            std::vector<std::string> stringOmitReceiver = Rcpp::as<std::vector<std::string>>(omitDyadsReceiver); // receiver
            std::vector<std::string> stringOmitType = Rcpp::as<std::vector<std::string>>(omitDyadsType); // type

            // allocating space for the converted senders, receivers and types
            Rcpp::IntegerVector omitSender(D_m),omitReceiver(D_m),omitType(D_m);

            
            for(d = 0; d < D_m; d++){
                // find sender
                std::vector<std::string>::iterator i = std::find(actor.begin(), actor.end(), stringOmitSender[d]);
                int omitSender_d = actorID.at(std::distance(actor.begin(), i));
                // find receiver
                std::vector<std::string>::iterator j = std::find(actor.begin(), actor.end(), stringOmitReceiver[d]);
                int omitReceiver_d = actorID.at(std::distance(actor.begin(), j));

                // sorting sender/actor1 and receiver/actor2 when directed = FALSE
                if(!directed){
                    if(omitSender_d < omitReceiver_d){
                        omitSender[d] = omitSender_d;
                        omitReceiver[d] = omitReceiver_d;
                    }
                    else{
                        omitSender[d] = omitReceiver_d;
                        omitReceiver[d] = omitSender_d;
                    }
                }
                else{ // when directed == TRUE (we do not sort)
                    omitSender[d] = omitSender_d;
                    omitReceiver[d] = omitReceiver_d;
                }

                // find type 
                std::vector<std::string>::iterator c = std::find(type.begin(), type.end(), stringOmitType[d]);
                omitType[d] = typeID.at(std::distance(type.begin(), c));
            }

            Rcpp::DataFrame converted_m = Rcpp::DataFrame::create(Rcpp::Named("actor1") = omitSender, // out_list_names[1]
                                                    Rcpp::Named("actor2") = omitReceiver,
                                                    Rcpp::Named("type") = omitType);
            outRiskset.push_back(converted_m);
              
        }
        else{
            outRiskset.push_back(R_NaN);
        }

    }

    Rcpp::DataFrame outEdgelist = Rcpp::DataFrame::create(Rcpp::Named("time") = edgelist["time"], 
                                                          Rcpp::Named("actor1") = outputSender,
                                                          Rcpp::Named("actor2") = outputReceiver,
                                                          Rcpp::Named("type") = outputType,
                                                          Rcpp::Named("weight") = edgelist["weight"]);
    out["edgelist"] = outEdgelist; 
    out["riskset"] = outRiskset;
    

                                                 
    return out;
}

//' getBinaryREH (a function that returns a utility matrix used in optimization algorithms)
//'
//' @param edgelist edgelist converted according to actorID and typeID
//' @param riskset riskset list converted according to actorID and typeID
//' @param risksetCube arma::cube object [N*N*C] where the cell value returns the column index to use in the outBinaryREH
//' @param M number of observed relational events
//' @param D number of possible dyads (accounting for event types as well)
//'
//' @return utility matrix per row 0 if the event could happen but didn't, 1 if the event happend, -1 if the event couldn't occur
//' 
// [[Rcpp::export]]
arma::mat getBinaryREH(Rcpp::DataFrame edgelist, Rcpp::List riskset, arma::ucube risksetCube, arma::uword M, arma::uword D) {
    arma::uword m,d;
    arma::mat outBinaryREH(M,D,arma::fill::zeros); // by setting the initial values to zero we already handle those
                                                    // relational events that could have occurred but didn't
    Rcpp::IntegerVector sender = edgelist["actor1"];
    Rcpp::IntegerVector receiver = edgelist["actor2"];
    Rcpp::IntegerVector type = edgelist["type"];

    for(m = 0; m < M; m++){
        // relational event that occurred
        arma::uword event_m = risksetCube(sender[m],receiver[m],type[m]);
        outBinaryREH(m,event_m) = 1;
        // relational events that couldn't occur
        if(!R_IsNaN(riskset[m])){
            // we expect a Rcpp::DataFrame (sender,receiver,type)
            arma::mat omitDyads = riskset[m];

            // number of dyads to omit from the riskset at the m-th time point
            arma::uword D_m = omitDyads.n_rows;
            // getting sender, receiver and type combinations to omit from the riskset at the m-th time point
            arma::vec omitSender = omitDyads.col(0); // sender
            arma::vec omitReceiver = omitDyads.col(1); // receiver
            arma::vec omitType = omitDyads.col(2); // type
            for(d = 0; d < D_m; d++){
                arma::uword event_d = risksetCube(omitSender(d),omitReceiver(d),omitType(d));
                outBinaryREH(m,event_d) = -1;
            }
        }
    }
    return outBinaryREH;
}


//' rehCpp (a function for preprocessing data)
//'
//' @param edgelist is a dataframe of relational events sorted by time: [time,actor1,actor2,type,weight]
//' @param covariates list of covariates to be provided according to the input structure working with 'remstats'
//' @param add_actors vector of actors not in the network but to be considered in the analysis
//' @param add_types vector of types not in the network but to considered in the analysis
//' @param directed dyadic events directed (TRUE) or undirected (FALSE)
//' @param ordinal TRUE if the only the time order of events is known, FALSE if also the time value is known
//' @param origin time origin value 
//' @param riskset is a list of length equal to the number of events, each object a matrix with unobserved dyads (using actors string names)
//'
//' @return list of objects
//' @export
// [[Rcpp::export]]
Rcpp::List rehCpp(Rcpp::DataFrame edgelist, 
                  Rcpp::List covariates, 
                  Rcpp::RObject add_actors, 
                  Rcpp::RObject add_types,  
                  bool directed,
                  bool ordinal,
                  Rcpp::RObject origin,
                  Rcpp::List riskset) {

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
    // Reordering `edgelist` and `riskset` if `intereventTime` was sorted
    if(intereventTime.containsElementNamed("order")){  
        arma::uvec new_order = intereventTime["order"];
        // reordering edgelist and riskset
        edgelist = rearrangeDataFrame(edgelist,new_order);
        riskset = rearrangeList(riskset,new_order);
    }
   
    // StringVector of senders
    Rcpp::StringVector senders = edgelist["actor1"]; // edgelist[1] is the actor1/sender

    // StringVector of receivers
    Rcpp::StringVector receivers = edgelist["actor2"]; // edgelist[2] is the actor2/receiver

    //StringVector of senders and receivers 
    Rcpp::StringVector senders_and_receivers(senders.length()+receivers.length());
    senders_and_receivers[Rcpp::Range(0,(senders.length()-1))] = senders;
    senders_and_receivers[Rcpp::Range(senders.length(),(senders_and_receivers.length()-1))] = receivers;
    if(!Rf_isNull(add_actors)){
        Rcpp::StringVector add_actors_vector = Rcpp::as<Rcpp::StringVector>(add_actors);
        for(arma::uword n = 0; n < add_actors_vector.length(); n++){
            senders_and_receivers.push_back(add_actors_vector[n]);
        } 
    } 
    // Finding unique strings in senders_and_receivers 
    Rcpp::StringVector actor = Rcpp::unique(senders_and_receivers);
    actor.sort(); // sorting actors

    // Finding unique strings in event types 
    Rcpp::StringVector vector_of_types = edgelist["type"];
    if(!Rf_isNull(add_types)){
        Rcpp::StringVector add_types_vector = Rcpp::as<Rcpp::StringVector>(add_types);
        for(arma::uword c = 0; c < add_types_vector.length(); c++){
            vector_of_types.push_back(add_types_vector[c]);
        } 
    } 
    Rcpp::StringVector type = Rcpp::unique(vector_of_types);
    type.sort(); // sorting types
    
    // Storing some useful dimensions
    out["M"] = edgelist.nrows(); // number of events
    out["N"] = actor.length(); // number of actors
    out["C"] = type.length(); // number of events types
    
    // How many (possible) dyads? if `directed` N*(N-1), N*(N-1)/2 otherwise
    if(directed){
        D = actor.length()*(actor.length()-1)*type.length();
    }
    else{
        D = ((actor.length()*(actor.length()-1))/2)*type.length();
    }
    out["D"] = D; // number of dyads

    // Are there more than one event type? (if event type is only one, then no event types are considered)
    if(type.length() > 1) out["with_type"] = true;

    // Is the network weighted?
    Rcpp::NumericVector event_weights = edgelist["weight"]; 
    Rcpp::NumericVector unique_weights = Rcpp::unique(event_weights);
    if(unique_weights.length() > 1) out["weighted"] = true;   

    // Creating a dictionary for actors and event types, that is like: 'string_name' = integer (IDentifier)
    Rcpp::List actorsDictionary = Rcpp::List::create(Rcpp::Named("actor") = actor, Rcpp::Named("actorID") = Rcpp::Range(0,actor.length()-1)); 
    out["actorsDictionary"] = actorsDictionary;
    Rcpp::List typesDictionary = Rcpp::List::create(Rcpp::Named("type") = type, Rcpp::Named("typeID") = Rcpp::Range(0,type.length()-1)); 
    out["typesDictionary"] = typesDictionary;
    // Creating riskset objects (it is not the rehBinary but it just includes all the possible combination of [sender,receiver,type]) ...

    // ... arranged in a matrix [D*3]
    out["risksetMatrix"] = getRisksetMatrix(actorsDictionary["actorID"],typesDictionary["typeID"],out["N"],out["C"],directed);

    // ... arranged in a cube [N*N*C]
    out["risksetCube"] = getRisksetCube(out["risksetMatrix"],out["N"],out["C"]);

    // Converting input edgelist and riskset list according to the new id's for both actors and event types
    Rcpp::List convertedInput = convertInputREH(edgelist,riskset,actorsDictionary,typesDictionary,out["M"],directed);
    out["edgelist"] = convertedInput["edgelist"];
    out["riskset"] = convertedInput["riskset"];

    // Create event binary matrix from the riskset and the edgelist, that is rehBinary
    out["rehBinary"] = getBinaryREH(out["edgelist"],out["riskset"],out["risksetCube"],out["M"],out["D"]);
                                    
    // Preprocess covariates here (we want to make 'remstats' understand our input)
    // ...

    // END of the processing and returning output

    return out;
}




//' tryClone
//'
//' @param N N parameter
//' @param C C parameter
//'
//' @return NumericVector
//'
//' @export
// [[Rcpp::export]]
arma::ucube tryClone(arma::uword N, arma::uword C) {

    arma::ucube risksetCube(N,N,C);
    risksetCube.fill(arma::datum::nan); 

   return risksetCube;
}