#This file includes code to show how to compute sigma-separation using the library "ciflyr"
#We use the following libraries. They can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)

#The next function assumes that the sets of strongly connected components are already given, that is,
#we have a partition for the set of nodes V={1,...,p}=C_1\cup ...\cup C_q, according to the definition
#of strongly connected components. 
#Function to generate rule table for sigma-connection in mixed graphs:
sigma_table_mixed_graphs=function(p){
  
  #Write sets of strongly connected components \sigma(i)=C_i  
  SCC = paste0("C_", 1:p)  
  
  #Write a chain for "current in \sigma(next)"
  current_in_sigma.next = paste(
    paste0("(current in ", SCC, " and next in ", SCC, ")"),
    collapse = " or ")  
  
  rule1 = "current in Z and next not in Z"
  
  rule2 = paste("current in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule3 = rule1
  
  rule4 = rule2
  
  rule5 = "current in Z"
  
  rule6 = rule5
  
  rule7 = paste("current not in Z or (current in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule8 = rule7
  
  rule9 = paste("current not in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule10 = "current not in Z and next not in Z"
  
  rule11 = paste("next in Z and (",current_in_sigma.next,")",sep="")
  
  rule12 = "next not in Z"
  
  rule13 = "current not in Z"
  
  rule14 = "true"
  
  rule15 = rule13
  
  rule16 = current_in_sigma.next
  
  # Write CIfly Rule Table
  sigmaConnected=paste(
    "EDGES --> <--, <->", 
    paste("SETS", paste(c("X","Z", SCC), collapse = ",")),
    "COLORS bidir, rightdir, outZ, former_eq",
    "START <-- [red] AT X",
    "OUTPUT ... [bidir,rightdir,outZ]",
    "",
    paste("--> [rightdir] | <-- [outZ]     |", rule1),
    paste("--> [rightdir] | <-- [former_eq]|", rule2),
    paste("<-> [bidir]    | <-- [outZ]     |", rule3),
    paste("<-> [bidir]    | <-- [former_eq]|", rule4), 
    paste("--> [rightdir] | <-> [bidir]    |", rule5), 
    paste("<-> [bidir]    | <-> [bidir]    |", rule6), 
    paste("--> [rightdir] | --> [rightdir] |", rule7), 
    paste("<-> [bidir]    | --> [rightdir] |", rule8), 
    paste("<-- [outZ]     | <-- [former_eq]|", rule9), 
    paste("<-- [outZ]     | <-- [outZ]     |", rule10), 
    paste("<-- [former_eq]| <-- [former_eq]|", rule11), 
    paste("<-- [former_eq]| <-- [outZ]     |", rule12), 
    paste("<-- [outZ]     | <-> [bidir]    |", rule13), 
    paste("<-- [former_eq]| <-> [bidir]    |", rule14), 
    paste("<-- [outZ]     | --> [rightdir] |", rule15), 
    paste("<-- [former_eq]| --> [rightdir] |", rule16), 
    sep = "\n")
  
  # Print table
  return(sigmaConnected)
}

