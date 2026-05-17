# Title: Algorithm to solve t-separation in Python. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

# This file includes code to solve the problem of finding the set B of all nodes t-connected to A given (C_A, C_B).

# The following modules can be installed via pip in Python:
import ciflypy as cf
import igraph as ig
import matplotlib.pyplot as plt

#The following string is the rule table for finding t-connected nodes to a set A by the duple of sets (C_A, C_B):
t_connected_table = """
EDGES --> <--
SETS A, C_A, C_B
START <-- AT A
OUTPUT ...

<--  | <--  | current not in C_A
-->  | -->  | current not in C_B
<--  | -->  | current not in C_A and current not in C_B
"""

# Example: A directed acyclic graph (DAG) as a list of edges in a 2-columns matrix
n_nodes = 8
DAG_edges = [(2, 1), (3, 2), (3, 4), (4, 5), (5, 6), (5, 8), (6, 7)]

# Create a graph object:
DAG = ig.Graph(n_nodes, DAG_edges, directed = True)
#We delete the additional 0-labeled node
DAG.delete_vertices(0)

# Plot the graph: 
fig, ax = plt.subplots(figsize=(5, 5))
ig.plot(DAG, target = ax,
    vertex_label= ["1", "2", "3", "4", "5", "6", "7", "8"],   
    vertex_size=20,
    vertex_color="#77BAAA",
    vertex_label_size=14.0,
    vertex_label_color="black",
    edge_width=0.5,
    edge_color="black",
    edge_size=2.5,
    edge_arrow_size = 10,
    edge_arrow_width = 10    
)


# For the DAG above, the set of nodes *-connected to A = {1} given (C_A, C_B) = ({5}, {6}) is B = {1, 2, 3, 4, 5, 6, 8}.
# Sets A, C_A, C_B and B do not need to be disjoint.
B_true = [1, 2, 3, 4, 5, 6, 8]

# Write a function to solve t-connection.
# This function requires a rule table as string: 
def t_connected_with_string(G, A, C_A, C_B, t_connected_table):
    sets = {"A": A, "C_A": C_A, "C_B": C_B}
    B = cf.reach(G, sets, t_connected_table, table_as_string = True)
    return sorted(B)


# The DAG as required by the function. 
G = {"-->": DAG_edges}

# Sets.
A = [1]
C_A = [5] 
C_B = [6]

# Compute the set B
B = t_connected_with_string(G, A, C_A, C_B, t_connected_table)
print(B)

# We test
B == B_true

# Write a function to solve t-connection.
# This function requires to specify path to rule table: 
def t_connected_with_txt(G, A, C_A, C_B):
    sets = {"A": A, "C_A": C_A, "C_B": C_B}
    table_path = "./t_connection_rule_table.txt"
    B = cf.reach(G, sets, table_path)
    return sorted(B)
  
