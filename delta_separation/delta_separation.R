# Title: Algorithm to solve delta-separation in R. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

#This file includes code to show how to compute delta-separation using the function reach() of the ciflyr package.

#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)

#The following string is the rule table for finding delta-connected nodes to a set B by a set C:
delta_connected_table <-"
EDGES --> <--
SETS B, C
START <-- AT B
OUTPUT ...

-->  | <--  | current in C
-->  | -->  | current not in B and current not in C
<--  | <--  | current not in C
<--  | -->  | current not in B and current not in C
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

# For the DG above, the set of nodes delta-connected to B = {4, 6} given C = {2} is A = {4, 5, 6, 7}.
A_true <- c(4, 5, 6, 7)

# Write a function to solve delta-connection. 
# It requires a rule table as string.
# It offers the possibility of computing a disjoint set form A and C (not disjoint by default).   
delta_connected_with_string <- function(G, B, C, delta_connected_table, disjoint = FALSE){
  Sets <- list("B" = B, "C" = C)
  A <- reach(G, Sets, delta_connected_table, tableAsString = TRUE)
  if(disjoint == TRUE){A <- setdiff(A, c(B, C))}
  return(sort(A))
}

# The DG in the plot is stored as required by "reach".
G <- list("-->" = DG_edges)

# required sets.
B <- c(4, 6)
C <- c(2)

# Compute the set A
A <- delta_connected_with_string(G, B, C, delta_connected_table)
print(A)

# We test
A == A_true

# If we ask A, B and C to be disjoint we must get A_disj = {5, 7}
A_disj <- delta_connected_with_string(G, B, C, delta_connected_table, disjoint = TRUE)
print(A_disj)

# Write a function to solve delta-connection.
# This function requires to specify path to rule table: 
delta_connected_with_txt <- function(G, B, C){
  # Sets
  sets = list("B" = B, "C" = C)
  
  # Path to table
  table_path = "./delta_connection_rule_table.txt"
  
  # Compute the set B of all nodes d_connected 
  A = reach(G, sets, table_path)
  
  #Return nodes by order of labels
  return(sort(A))
}
