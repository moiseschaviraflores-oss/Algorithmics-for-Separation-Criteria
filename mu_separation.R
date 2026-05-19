# Title: Algorithm to solve mu-separation in R. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

#This file includes code to show how to compute mu-separation using the function reach() of the ciflyr package.

#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)

#The following string is the rule table for finding mu-connected nodes to a set A by a set C:
mu_connected_table <- "
EDGES --> <--, <->
SETS A, C
START <-- AT A
OUTPUT -->, <->

--> | <-- | current in C
<-> | <-- | current in C
--> | <-> | current in C
<-> | <-> | current in C
--> | --> | current not in C
<-- | --> | current not in C
<-- | <-- | current not in C
<-> | --> | current not in C
<-- | <-> | current not in C
"

# Example: DG as a list of (directed) edges in a 2-columns matrix
DG_edges <- rbind(c(1, 2), c(3, 2), c(4, 4), c(4, 2), c(4, 5), c(5, 7), c(6, 5))

# Create a igraph object:
DG <- graph_from_edgelist(DG_edges)

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

# For the DG above, the set of nodes mu-connected to A = {1} given C = {2, 3} is B = {2, 4, 5, 7}.
B_true <- c(2, 4, 5, 7)

# Write a function to solve mu-connection. 
# It requires a rule table as string.
mu_connected_with_string <- function(G, A, C, mu_connected_table){
  Sets <- list("A" = A, "C" = C)
  B <- reach(G, Sets, mu_connected_table, tableAsString = TRUE)
  return(sort(B))
}

# The DG in the plot is stored as required by "reach".
G <- list("-->" = DG_edges)

# Required sets.
A <- c(1)
C <- c(2, 3)

# Compute the set B
B <- mu_connected_with_string(G, A, C, mu_connected_table)
print(B)

# Test
B == B_true

# Write a function to solve d-connection.
# This function requires to specify path to rule table: 
mu_connected_with_txt <- function(G, A, C){
  # Sets
  sets = list("A" = A, "C" = C)
  
  # Path to table
  table_path = "./mu_connection_rule_table.txt"
  
  # Compute the set B of all nodes mu_connected 
  B = reach(G, sets, table_path)
  
  #Return nodes by order of labels
  return(sort(B))
}

