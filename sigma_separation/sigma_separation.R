# Title: Algorithm to solve sigma-separation in R. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

#This file includes code to show how to compute sigma-separation using the function reach() of the ciflyr package.

#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)

#The following string is the rule table for finding sigma-connected nodes to a set X by a set Z:
#The following function writes a rule table for sigma-connection.
#It requires the number of strongly connected components q to be specified. 
sigma_table_mixed_graphs <- function(q){
  
  #Write strongly connected components C_1,...,C_q  
  SCC <- paste0("C_", 1:q)  
  
  #Write a chain for "current in \sigma(next)"
  current_in_sigma.next <- paste(
    paste0("(current in ", SCC, " and next in ", SCC, ")"),
    collapse = " or ")  
  
  #We write the rules for table
  rule1 <- "current in Z and next not in Z"
  
  rule2 <- paste("current in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule3 <- rule1
  
  rule4 <- rule2
  
  rule5 <- "current in Z"
  
  rule6 <- rule5
  
  rule7 <- paste("current not in Z or (current in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule8 <- rule7
  
  rule9 <- paste("current not in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule10 <- "current not in Z and next not in Z"
  
  rule11 <- paste("next in Z and (",current_in_sigma.next,")",sep="")
  
  rule12 <- "next not in Z"
  
  rule13 <- "current not in Z"
  
  rule14 <- "true"
  
  rule15 <- rule13
  
  rule16 <- current_in_sigma.next
  
  # Write Rule Table
  sigma_connected_table <- paste(
    "EDGES --> <--, <->", 
    paste("SETS", paste(c("X","Z", SCC), collapse = ", ")),
    "COLORS plain, outZ, formereq",
    "START <-- [outZ] AT X",
    "OUTPUT ... [...]",
    "",
    paste("--> [plain]    | <-- [outZ]     |", rule1),
    paste("--> [plain]    | <-- [formereq] |", rule2),
    paste("<-> [plain]    | <-- [outZ]     |", rule3),
    paste("<-> [plain]    | <-- [formereq] |", rule4), 
    paste("--> [plain]    | <-> [plain]    |", rule5), 
    paste("<-> [plain]    | <-> [plain]    |", rule6), 
    paste("--> [plain]    | --> [plain]    |", rule7), 
    paste("<-> [plain]    | --> [plain]    |", rule8), 
    paste("<-- [outZ]     | <-- [formereq] |", rule9), 
    paste("<-- [outZ]     | <-- [outZ]     |", rule10), 
    paste("<-- [formereq] | <-- [formereq] |", rule11), 
    paste("<-- [formereq] | <-- [outZ]     |", rule12), 
    paste("<-- [outZ]     | <-> [plain]    |", rule13), 
    paste("<-- [formereq] | <-> [plain]    |", rule14), 
    paste("<-- [outZ]     | --> [plain]    |", rule15), 
    paste("<-- [formereq] | --> [plain]    |", rule16), 
    sep = "\n")
  
  # We ask to return table
  return(sigma_connected_table)
}

# Example: Directed cyclic  graph (DCG) as two lists of edges,. 
DCG_edges <- rbind(c(1, 2), c(2, 3), c(4, 3), c(4, 5), c(5, 6), c(6, 8), 
                  c(8, 9), c(9, 5), c(6, 7), c(1, 10), c(11, 10), c(11, 12))

DCG <- graph_from_edgelist(DCG_edges)

# Plot the graph:
plot(DCG,
     #vertex.color = "#6699cc", # Node color
     vertex.size = 12, # Node size
     vertex.label.size = 12 , # Label size
     vertex.label.color = "black", # Label color
     edge.color = "black", # Edge color
     edge.width = 0.6, # Edge width
     edge.arrow.size = 0.5,
     edge.arrow.length = 5,
     edge.length = 10)

# For this DCG the set Y of nodes sigma-connected to X = {1} given Z = {3, 5, 10, 11} is Y = {1, 2, 4, 6, 7, 8, 9}.
Y_true <- c(1, 2, 4, 6, 7, 8, 9)

# Write a function to solve sigma-connection. 
# It requires the number of SCC q.
# It requires a list of SSC of the form C -> list("C_1"=C_1,...,"C_q"=C_q).
# It offers provides a disjoint set from Z.   
sigma_connected_with_string <- function(G, X, Z, C, q){
  Sets <- c(list("X" = X, "Z" = Z), C)
  sigma_connected_table <- sigma_table_mixed_graphs(q)
  Y <- reach(G, Sets, sigma_connected_table, tableAsString = TRUE)
  Y <- setdiff(Y, Z)
  return(sort(Y))
}

# The DG in the plot is stored as required by "reach".
G <- list("-->" = DCG_edges)

# required sets.
X <- c(1)
Z <- c(3, 5, 10, 11)

# We have q = 9 SCC, but only C_1 = {5, 6, 8, 9} is useful
C <- list("C_1" = c(5, 6, 8, 9))
q <- 1

# Compute the set Y
Y <- sigma_connected_with_string(G, X, Z, C, q)
print(Y)

# We test
Y == Y_true
