# Title: Algorithm to solve m-separation in R. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

#This file includes code to show how to compute m-separation using the function reach() of the ciflyr package.

#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)

#The following string is the rule table for finding m-connected nodes to a set A by a set C:
m_connected_table <- "
EDGES --> <--, <->
SETS A, C
START <-- AT A
OUTPUT ...

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

# Example: An acyclic directed mixed graph (ADMG) as a list of edges in a 2-columns matrix
ADMG_directed_edges <- rbind(c(2, 1), c(3, 2), c(3, 4), c(4, 5), c(7, 6))
ADMG_bidirected_edges <- rbind(c(6, 5))

# Create a igraph object:
ADMG <- graph_from_edgelist(rbind(ADMG_directed_edges, c(4, 6), c(6, 4)), directed = TRUE)
E(ADMG)$curved <- 0 

# Plot the graph:
plot(ADMG,
     vertex.color = "#6699cc", # Node color
     vertex.size = 22, # Node size
     vertex.label.size = 14 , # Label size
     vertex.label.color = "black", # Label color
     edge.color = "black", # Edge color
     edge.width = 0.5, # Edge width
     edge.arrow.size = 0.35,
     edge.size = 2.5)

# For the ADMG above, the set of nodes mu-connected to A = {1} given C = {5} is B = {1, 2, 3, 4, 5, 6}.
B_true <- c(1, 2, 3, 4, 5, 6)

# Write a function to solve m-connection. 
# It requires a rule table as string.
m_connected_with_string <- function(G, A, C, m_connected_table){
  Sets <- list("A" = A, "C" = C)
  B <- reach(G, Sets, m_connected_table, tableAsString = TRUE)
  return(sort(B))
}

# The DG in the plot is stored as required by "reach".
G <- list("-->" = ADMG_directed_edges, "<->" = ADMG_bidirected_edges)

# Required sets.
A <- c(1)
C <- c(5)

# Compute the set B
B <- m_connected_with_string(G, A, C, m_connected_table)
print(B)

# Test
B == B_true

# Write a function to solve m-connection.
# This function requires to specify path to rule table: 
m_connected_with_txt <- function(G, A, C){
  # Sets
  sets = list("A" = A, "C" = C)
  
  # Path to table
  table_path = "./m_connection_rule_table.txt"
  
  # Compute the set B of all nodes m_connected 
  B = reach(G, sets, table_path)
  
  #Return nodes by order of labels
  return(sort(B))
}


