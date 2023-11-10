#ifndef MESSAGES_H
#define MESSAGES_H

//' warningMessage
std::string warningMessage(int cond){
      std::string message = "undefined";
      switch(cond){
            case 0:
                  message = "\nWarning: the `time` variable is not sorted. Sorting will be forced.\n";
                  break;
            case 1:
                  message = "\nWarning: self-loops are present in the input edgelist (i.e. `actor1` and `actor2` are the same). They are removed with the processing.\n";
                  break;
            case 2:
                  message = "\nWarning: value supplied as `origin` is greater or equal than the first time point. `origin` is then automatically set either to one day/second before the first time point or to 0.\n";
                  break;
            case 3:
                  message = "\nWarning: one or more actors/types supplied in `omit_dyad` were not found in the edgelist. Therefore the corresponding dyads defined in the `omit_dyad` object were ignored.\n";
                  break;            
      }
      return message;
}


//' errorMessage
std::string errorMessage(int cond){
      std::string message = "undefiend";
      switch(cond){
            case 0:
                  message = "time vector in each element of the list 'omit_dyad' must be sorted so that elements indicate respectively start and stop time when the riskset changed";
                  break;
            case 1:
                  message = "one or more dyad ID's can't be found in the remify object 'x': dyad ID's must range between 1 and x$D";
                  break; 
            case 2:
                  message = "time vector in each element of the list 'omit_dyad' must be of length 2: start and stop time when the riskset changed";
                  break;
            case 3:
                  message = "either start or stop in one of the elements in the list 'omit_dyad' are not found in the edgelist. Please, provide observed time points as start and stop values";
                  break;
            case 4:
                  message = "actor-oriented model can only work with directed networks";
                  break;            
            case 5:
                  message = "time variable can't be negative";
                  break;
      }
      return message;
}

#endif
