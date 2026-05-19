# Title: Algorithm to solve d-separation in R. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

#This file includes code to show how to compute d-separation using the function reach() of the ciflyr package.

#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)

#The following string is the rule table for finding d-connected nodes to a set A by a set C:
d_connected_table <- "
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT ... 

--> | <-- | current in C
--> | --> | current not in C
<-- | --> | current not in C
<-- | <-- | current not in C
"

# Example: DAG as a list of (directed) edges in a 2-columns matrix
DAG_edges <- rbind(c(1, 2), c(2, 3), c(4, 2), c(4, 5), c(5, 6), c(7, 5))

# Create a igraph object:
DAG <- graph_from_edgelist(DAG_edges)

# Plot the graph:
plot(DAG,
     vertex.color = "#6699cc", # Node color
     vertex.size = 22, # Node size
     vertex.label.size = 14 , # Label size
     vertex.label.color = "black", # Label color
     edge.color = "black", # Edge color
     edge.width = 0.5, # Edge width
     edge.arrow.size = 0.35,
     edge.size = 2.5)

# For the DAG above, the set of nodes d-connected to A = {1} given C = {3, 6} is B = {1, 2, 3, 4, 5, 6, 7} (not necessarily disjoint to A and C).
B_true <- c(1, 2, 3, 4, 5, 6, 7)

# Write a function to solve d-connection. 
# It requires a rule table as string.
# It offers the possibility of computing a disjoint set form A and C (not disjoint by default).   
d_connected_with_string <- function(G, A, C, d_connected_table, disjoint = FALSE){
  Sets <- list("A" = A, "C" = C)
  B <- reach(G, Sets, d_connected_table, tableAsString = TRUE)
  if(disjoint == TRUE){B <- setdiff(B, c(A, C))}
  return(sort(B))
}

# The DAG in the plot is stored as required by "reach".
G <- list("-->" = DAG_edges)

# required sets.
A <- c(1)
C <- c(3, 6)

# Compute the set B
B <- d_connected_with_string(G, A, C, d_connected_table)
print(B)

# We test
B == B_true

# If we ask A, B and C to be disjoint we must get B_disj = {2, 4, 5, 7}
B_disj <- d_connected_with_string(G, A, C, d_connected_table, disjoint = TRUE)
print(B_disj)

# Write a function to solve d-connection.
# This function requires to specify path to rule table: 
d_connected_with_txt <- function(G, A, C){
  # Sets
  sets = list("A" = A, "C" = C)
  
  # Path to table
  table_path = "./d_connected_rule_table.txt"
  
  # Compute the set B of all nodes d_connected 
  B = reach(G, sets, table_path)
  
  #Return nodes by order of labels
  return(sort(B))
}

