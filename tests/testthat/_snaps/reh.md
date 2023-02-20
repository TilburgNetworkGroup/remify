# test (1) on method print()

    Code
      reh(edgelist = reh_loc$edgelist, actors = reh_loc$actors, types = reh_loc$types,
      directed = TRUE, ordinal = FALSE, origin = reh_loc$origin, omit_dyad = reh_loc$
        omit_dyad, model = "tie")
    Output
      Relational Event Network
      (processed for tie-oriented modeling):
      	> events = 9915
      	> actors = 20
      	> (event) types = 3
      	> riskset = dynamic
      	> directed = TRUE
      	> ordinal = FALSE
      	> weighted = FALSE
      	> time length ~ 1970-03-21 21:13:48 
      	> interevent time 
      		 >> minimum ~ 0.0011 seconds
      		 >> maximum ~ 5811.4011 seconds

# test (2) on method print()

    Code
      reh(edgelist = reh_loc$edgelist, actors = reh_loc$actors, types = reh_loc$types,
      directed = TRUE, ordinal = FALSE, origin = NULL, omit_dyad = reh_loc$omit_dyad,
      model = "tie")
    Output
      Relational Event Network
      (processed for tie-oriented modeling):
      	> events = 9915
      	> actors = 20
      	> (event) types = 3
      	> riskset = dynamic
      	> directed = TRUE
      	> ordinal = FALSE
      	> weighted = FALSE
      	> time length ~ 1970-03-21 20:59:34 
      	> interevent time 
      		 >> minimum ~ 0.0011 seconds
      		 >> maximum ~ 5811.4011 seconds

# test (3) on method print()

    Code
      reh(edgelist = reh_loc$edgelist, actors = reh_loc$actors, types = reh_loc$types,
      directed = TRUE, ordinal = FALSE, origin = reh_loc$origin, omit_dyad = reh_loc$
        omit_dyad, model = "tie")
    Output
      Warning: at least two events (or more) occurred at the same time point. The interevent time of such events will be evenly spaced.
      Relational Event Network
      (processed for tie-oriented modeling):
      	> events = 9915
      	> actors = 20
      	> (event) types = 3
      	> riskset = static
      	> directed = TRUE
      	> ordinal = FALSE
      	> weighted = FALSE
      	> time length ~ 1970-03-22 
      	> interevent time 
      		 >> minimum ~ 0.0069 days
      		 >> maximum ~ 1 days

