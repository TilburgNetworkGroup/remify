# remify 3.2.6
 
* Date: 2024-05-15
* Minor fixes of implicit conversions in remify.h

# remify 3.2.5
 
* Date: 2024-02-05
* This is the 3.2.5 version of the package. In this version, events are dropped from the edgelist only if at least one missing value is found in at least one of the columns: "time", "actor1", "actor2", "type", "weight". Bug resolved when "ncores > 1" with function "getEventComposition()". Bug correction in 'getDyadID' method.
 