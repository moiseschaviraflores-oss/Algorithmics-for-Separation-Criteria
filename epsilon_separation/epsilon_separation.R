# Title: Algorithm to solve epsilon-separation in R. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

#This file includes code to show how to compute epsilon-separation using the function reach() of the ciflyr package.

#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)

#The following string is the rule table for finding epsilon-connected nodes to a set A by a set C:
epsilon_rule_table <- "
EDGES --> <--
SETS B, C
COLORS before, after
START <-- [before] AT B
OUTPUT ... [...]

--> [before] | <-- [after]  | current in C
<-- [before] | <-- [before] | true
<-- [before] | --> [before] | current not in B or current not in C
--> [before] | --> [before] | current not in B or current not in C
--> [after]  | <-- [after]  | current in C 
<-- [after]  | <-- [after]  | current not in C
<-- [after]  | --> [after]  | current not in B or current not in C
--> [after]  | --> [after]  | current not in B or current not in C
"

# Example: DG as a list of (directed) edges in a 2-columns matrix
DG_edges <- rbind(c(1, 2), c(2, 3), c(4, 2), c(4, 5), c(5, 4), c(5, 6), c(7, 5))

# Create a igraph object:
DG <- graph_from_edgelist(DG_edges)
E(DG)$curved <- 0 
E(DG)[c(4, 5)]$curved <- 1

# Plot the graph:
plot(DG,
     vertex.color = "#6699cc", # Node color
     vertex.size = 22, # Node size
     vertex.label.size = 14 , # Label size
     vertex.label.color = "black", # Label color
     edge.color = "black", # Edge color
     edge.width = 0.5, # Edge width
     edge.arrow.size = 0.35,
     edge.size = 2.5)

# For the DAG above, the set of nodes epsilon-connected to A = {1} given C = {3, 6} is B = {2, 3, 4, 5, 6}.
B_true <- c(2, 3, 4, 5, 6)

# Write a function to solve epsilon-connection. 
# It requires a rule table as string.
# It offers the possibility of computing a disjoint set form A and C (not disjoint by default).   
epsilon_connected_with_string <- function(G, A, C, epsilon_connected_table, disjoint = FALSE){
  Sets <- list("A" = A, "C" = C)
  B <- reach(G, Sets, epsilon_connected_table, tableAsString = TRUE)
  if(disjoint == TRUE){B <- setdiff(B, c(A, C))}
  return(sort(B))
}

# The DG in the plot is stored as required by "reach".
G <- list("-->" = DG_edges)

# required sets.
A <- c(1)
C <- c(3, 6)

# Compute the set B
B <- epsilon_connected_with_string(G, A, C, epsilon_connected_table)
print(B)

# We test
B == B_true

# If we ask A, B and C to be disjoint we must get B_disj = {2, 4, 5}
B_disj <- epsilon_connected_with_string(G, A, C, epsilon_connected_table, disjoint = TRUE)
print(B_disj)

# Write a function to solve epsilon-connection.
# This function requires to specify path to rule table: 
epsilon_connected_with_txt <- function(G, A, C){
  # Sets
  sets = list("A" = A, "C" = C)
  
  # Path to table
  table_path = "./epsilon_connection_rule_table.txt"
  
  # Compute the set B of all nodes epsilon_connected 
  B = reach(G, sets, table_path)
  
  #Return nodes by order of labels
  return(sort(B))
}


