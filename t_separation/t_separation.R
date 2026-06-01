# Title: Algorithm to solve t-separation in R. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

#This file includes code to show how to compute t-separation using the function "reach" in the ciflyr package.

#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)

#The following string is the rule table for finding t-connected nodes to a set A by the duple of sets (C_A, C_B):
t_connected_table = "
EDGES --> <--
SETS A, C_A, C_B
START <-- AT A
OUTPUT ...

<--  | <--  | current not in C_A
-->  | -->  | current not in C_B
<--  | -->  | current not in C_A and current not in C_B
"

# Example: DAG as a list of (directed) edges in a 2-columns matrix
DAG_edges <- rbind(c(2,1),c(3,2), c(3,4),c(4,5),c(5,6),c(5,8),c(6,7))

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


# For the DAG above, the set of nodes *-connected to A = {1} given (C_A, C_B) = ({5}, {6}) is B = {1, 2, 3, 4, 5, 6, 8}.
# Sets A, C_A, C_B and B do not need to be disjoint.
B_true <- c(1, 2, 3, 4, 5, 6, 8)

# Write a function to solve t-connection. 
# It requires a rule table as string.
t_connected_with_string <- function(G, A, C_A, C_B, t_connected_table, verbose = ){
  Sets <- list("A" = A, "C_A" = C_A, "C_B" = C_B)
  B <- reach(G, Sets, t_connected_table, tableAsString = TRUE)
  return(sort(B))
}

# The DAG in the plot is stored as required by "reach".
G <- list("-->" = DAG_edges)

# required sets.
A <- c(1)
C_A <- c(5) 
C_B <- c(6)

# Compute the set B
B <- t_connected_with_string(G, A, C_A, C_B, t_connected_table)
print(B)

# We test
B == B_true

# Write a function to solve t-connection using a rule table in a .txt file.
t_connected_with_txt <- function(G, A, C_A, C_B){
  Sets = list("A" = A, "C_A" = C_A, "C_B" = C_B)
  table_path = "./t_connection_rule_table.txt"
  B = reach(G, Sets, table_path)
  return(sort(B))
}
