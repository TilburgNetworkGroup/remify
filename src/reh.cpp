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
    std::vector<std::string> stringSender = Rcpp::as<std::vector<std::string>>(edgelist[1]);
    std::vector<std::string> stringReceiver = Rcpp::as<std::vector<std::string>>(edgelist[2]);
    std::vector<std::string> stringType = Rcpp::as<std::vector<std::string>>(edgelist[3]);
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

            Rcpp::DataFrame converted_m = Rcpp::DataFrame::create(Rcpp::Named(out_list_names[1]) = omitSender,
                                                    Rcpp::Named(out_list_names[2]) = omitReceiver,
                                                    Rcpp::Named(out_list_names[3]) = omitType);
            outRiskset.push_back(converted_m);
              
        }
        else{
            outRiskset.push_back(R_NaN);
        }

    }


    Rcpp::DataFrame outEdgelist = Rcpp::DataFrame::create(Rcpp::Named(out_list_names[0]) = edgelist[0],
                                                    Rcpp::Named(out_list_names[1]) = outputSender,
                                                    Rcpp::Named(out_list_names[2]) = outputReceiver,
                                                    Rcpp::Named(out_list_names[3]) = outputType,
                                                    Rcpp::Named(out_list_names[4]) = edgelist[4]);
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
    Rcpp::IntegerVector sender = edgelist[1];
    Rcpp::IntegerVector receiver = edgelist[2];
    Rcpp::IntegerVector type = edgelist[3];
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
//' @param riskset is a list of length equal to the number of events, each object a matrix with unobserved dyads (using actors string names)
//' @param covariates list of covariates to be provided according to the input structure working with 'remstats'
//' @param add_actors vector of actors not in the network but to be considered in the analysis
//' @param add_types vector of types not in the network but to considered in the analysis
//' @param directed dyadic events directed (TRUE) or undirected (FALSE)
//' @param ordinal TRUE if the only the time order of events is known, FALSE if also the time value is known
//' @param time_unit n 
//' @param time_class n 
//'
//' @return list of objects
//' @export
// [[Rcpp::export]]
Rcpp::List rehCpp(Rcpp::DataFrame edgelist,
                  Rcpp::List riskset, 
                  Rcpp::List covariates, 
                  Rcpp::StringVector add_actors, 
                  Rcpp::StringVector add_types,  
                  bool directed,
                  bool ordinal,
                  std::string time_unit,
                  std::string time_class) {

    // allocating memory for some variables and the output list
    arma::uword D; // number of dyads which depends on the directed input value 
    Rcpp::List out = Rcpp::List::create(); // output list

    // START of the processing

    // StringVector of senders
    Rcpp::StringVector senders = edgelist[1]; // edgelist[1] is the actor1/sender

    // StringVector of receivers
    Rcpp::StringVector receivers = edgelist[2]; // edgelist[2] is the actor2/receiver

    //StringVector of senders and receivers 
    Rcpp::StringVector senders_and_receivers(senders.length()+receivers.length());
    senders_and_receivers[Rcpp::Range(0,(senders.length()-1))] = senders;
    senders_and_receivers[Rcpp::Range(senders.length(),(senders_and_receivers.length()-1))] = receivers;
    if(add_actors.length() > 0){
        for(arma::uword n = 0; n < add_actors.length(); n++){
            senders_and_receivers.push_back(add_actors[n]);
        } 
    } 
    // Finding unique strings in senders_and_receivers 
    Rcpp::StringVector actor = Rcpp::unique(senders_and_receivers);
    actor.sort(); // sorting actors

    // Finding unique strings in event types  
    Rcpp::StringVector vector_of_types = edgelist[3]; // edgelist[3] it the type of the relational event
    if(add_types.length() > 0){
        for(arma::uword c = 0; c < add_types.length(); c++){
            vector_of_types.push_back(add_types[c]);
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
    out["with_type"] = 0;
    if(type.length() > 1) out["with_type"] = 1;
    // Is the network weighted?
    out["weighted"] = 0;
    Rcpp::NumericVector event_weights = edgelist[4];
    Rcpp::NumericVector unique_weights = Rcpp::unique(event_weights);
    if(unique_weights.length() > 1) out["weighted"] = 1;

    // intereventTime variable (calculating the waiting time between two subsequent events). If `ordinal = TRUE` do nothing.
    arma::vec intereventTime(edgelist.nrows(),arma::fill::ones);
    
    if(!ordinal){
        // which_time_class
        std::vector<std::string> time_classes = { "numeric" , "Date" , "Datetime" }; // classes of `time` object
        std::vector<std::string>::iterator time_class_iterator = std::find(time_classes.begin(), time_classes.end(), time_class);
        int which_time_class = std::distance(time_classes.begin(), time_class_iterator);

        // which_time_unit
        std::vector<std::string> time_units = { "second", "minute", "hour", "day", "month", "year", "NULL" }; // time units for the interevent time
        std::vector<std::string>::iterator time_unit_iterator = std::find(time_units.begin(), time_units.end(), time_unit);
        int which_time_unit = std::distance(time_units.begin(), time_unit_iterator);

        switch(which_time_class)
        {
        case 0: { // time_class is `numeric`
            arma::vec time = edgelist[0];
            arma::uword m = 1;
            // check if the `time` variable is sorted
            while(m < time.n_elem){
                if(time(m-1) < time(m)){
                    intereventTime(m) = time(m) - time(m-1); // compute the interevent time
                    m++;
                }
                else{
                    Rcpp::stop(errorMessage(0));
                }
            }
            intereventTime(0) = time(0); // assign the first interevent time as the first observed time value, assuming that t_0 = 0
            break;
            }
        case 1: {// time_class is `Date`
            break;
            }
        case 2: {// time_class is `Datetime`
            break;
            }

               // the first waiting time will be the min(diff) because we don't know the time origin of the study and therefore the waiting time to until the first event (we might want the user to specify a start time for the study)

            // function getInerteventTime(time,start,time_class,time_unit) to work with the data type below, time_unit is the lowest measurable time unit : one might want the interevent time in seconds or hours or minutes or days as time unit (thus decimals will differentiate events one another)

            // start time could be an input NULL by default (if NULL it will consider the min(diff) as waiting time, otherwise it will calculate it[ (1) check if the class object is the same as the time variable (2) if the start time is earlier than the first time point])
            // if it is [[IntegerVector]] or [[NumericVector]] (time_unit is ignored here) :
            // (1) check whether the variable is sorted (allow for equal numbers meaning that two or more events occurred at the same time)
            // (2) compute the waiting time



            // if it is [[DateVector]] (sensitivtiy is by default 'days') :
            // (1) check whether the variable is sorted (allow for same dates meaning that two or more events occurred at the same time : assume equally spaced events within a day)
            // (2) compute the waiting time
            // if it is a [[DatetimeVector]] (time_unit is by default in 'seconds') :
            // (1) check whether the variable is sorted (allow for same dates meaning that two or more events occurred at the same timestamp [very unlikely] : assume equally spaced events within a second [??] or get the microseconds)
            // (2) compute the waiting time
            // If the check about the ordering fails, return an errorMessage, user must check this issue by himself.
            
                
                
                // if(!is.null(start_time)){
                //  if(is.Date(start_time) | is.numeric(start_time)){
                
                    // process the time variable here (getSeconds(), getMinutes(), getHours(), getDays(), getYears(), user should be able to specify the time scale)
                    // calculate the intereventTime variable here
                    // the starting point (t_0) is important to define: it can either be a day/hour/minute before by convention or a prespeficied by the user
                //  }
                //  else{stop(errorMessage(cond = 1))} // errorMessage() is a function coming from the header file messages.h (loaded on line 9 in this file)
                // }
                // else{ ...  process here when there is no start_time argument define, as it is by default ... }

                //double t0 = 0.0; //it can be an argument of the function, a check for t0 being the actual minimum value must be provided in the future (just for now it is internally set)
                //time_loc.push_front(t0);
        }
    }
    out["intereventTime"] = intereventTime;     

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
//' @param input data.frame object
//'
//' @return NumericVector
//'
//' @export
// [[Rcpp::export]]
arma::uword tryClone(arma::vec time) {
        arma::uword m = 0;
        while(m < (time.n_elem -1)){
            if(time(m) < time(m+1)){
                m++;
            }
            else{
                Rcpp::stop(errorMessage(0));
                //errorMessage(); "Time variable is not sorted or there are at least two events with the same time value."
            }
        }
    return m ;
   //switch(TYPEOF(x[0])){
    //case INTSXP: return 1; // integers
   // case REALSXP: return 2; // numeric
   // case STRSXP: return 3; // strings
   // default: Rcpp::stop("type not handled"); // no support for obj
   // }
   // return -1;
}