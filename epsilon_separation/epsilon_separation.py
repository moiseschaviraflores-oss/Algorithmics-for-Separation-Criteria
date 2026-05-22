# Title: Algorithm to solve epsilon-separation in Python. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

# This file includes code to solve the problem of finding the set B of all nodes epsilon-connected to A given C.

# The following modules can be installed via pip in Python:
import ciflypy as cf
import igraph as ig
import matplotlib.pyplot as plt

#The following string is the rule table for finding epsilon-connected nodes to a set A by a set C:
epsilon_connected_table = """
EDGES --> <--
SETS A, C
COLORS before, after
START <-- [before] AT A
OUTPUT --> [...]

--> [before] | <-- [after]  | current in C
--> [after]  | <-- [after]  | current in C
<-- [before] | <-- [before] | current not in C
<-- [after]  | <-- [after]  | current not in C
<-- [before] | --> [before] | current not in C
<-- [after]  | --> [after]  | current not in C
--> [before] | --> [before] | current not in C
--> [after]  | --> [after]  | true
"""

# Example: A directed graph (DG) as a list of edges in a 2-columns matrix
n_nodes = 7
DG_edges = [(1, 2), (2, 3), (4, 2), (4, 5), (5, 4), (5, 6), (7, 5)]

# Create a graph object:
DG = ig.Graph(n_nodes, DG_edges, directed = True)
#We delete the additional 0-labeled node
DG.delete_vertices(0)

# Plot the graph: 
fig, ax = plt.subplots(figsize=(5, 5))
ig.plot(DG, target = ax,
    vertex_label= ["1","2","3","4","5","6","7"],   
    vertex_size=22,
    vertex_color="#77BAAA",
    vertex_label_size=14.0,
    vertex_label_color="black",
    edge_width=0.5,
    edge_color="black",
    edge_size=2.5,
    edge_arrow_size = 10,
    edge_arrow_width = 10 
)

# For the DG above, the set of nodes epsilon-connected to A given C is B = {2, 3, 4, 5, 6}.
B_true = [2, 3, 4, 5, 6]

# Write a function to solve epsilon-connection.
# This function requires a rule table as string: 
def epsilon_connected_with_string(G, A, C, epsilon_connected_table):
    sets = {"A": A, "C": C}
    B = cf.reach(G, sets, epsilon_connected_table, table_as_string = True)
    return sorted(B)

# The DG as required by the function. 
G = {"-->": DG_edges}

# Sets
A = [1]
C = [3, 6]

# Compute the set B
B = epsilon_connected_with_string(G, A, C, epsilon_connected_table)
print(B)

#Test
B == B_true

# Write a function to solve epsilon-connection.
# This function requires to specify path to rule table: 
def epsilon_connected_with_txt(G, A, C):
    sets = {"A": A, "C": C}
    table_path = "./epsilon_connection_rule_table.txt"
    B = cf.reach(G, sets, table_path)
    return sorted(B)
  
  
