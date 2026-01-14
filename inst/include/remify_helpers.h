#pragma once
#include <RcppArmadillo.h>

Rcpp::DataFrame rearrangeDataFrame(Rcpp::DataFrame x, arma::uvec index);
Rcpp::IntegerMatrix getRisksetSender(Rcpp::List which_dyad,
                                     int C,
                                     int D,
                                     int N);
Rcpp::IntegerMatrix getRiskset(Rcpp::List which_dyad,
                                 int C,
                                 int D,
                                 int N,
                                 bool directed);
Rcpp::List processOmitDyad(Rcpp::List convertedOmitDyad, Rcpp::List convertedOmitDyad_time, arma::uword M, int C, int D, int N, bool directed, std::string model);
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
                            int ncores = 1);
arma::imat getEventsComposition(arma::vec dyads,
                                int N,
                                int D,
                                bool directed,
                                int ncores);
int getDyadIndex_cpp(double actor1, double actor2, double type, int N, bool directed);
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
                             int ncores = 1);
