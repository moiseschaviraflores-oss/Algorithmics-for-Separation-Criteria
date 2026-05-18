# Title: Algorithm to solve m-separation in Python. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

# This file includes code to solve the problem of finding the set B of all nodes m-connected to A given C.

# The following modules can be installed via pip in Python:
import ciflypy as cf
import igraph as ig
import matplotlib.pyplot as plt

#The following string is the rule table for finding m-connected nodes to a set A by a set C:
m_connected_table = """
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
"""

# Example: An acyclic directed mixed graph (ADMG) as a list of edges in a 2-columns matrix
n_nodes = 7
ADMG_directed_edges = [(2, 1), (3, 2), (3, 4), (4, 5), (7, 6)]
ADMG_bidirected_edges = [(6, 5)]

# Create a graph object:
ADMG = ig.Graph(n_nodes, ADMG_directed_edges + [(4, 6), (6, 4)], directed = True)
#We delete the additional 0-labeled node
ADMG.delete_vertices(0)

# Plot the graph: 
fig, ax = plt.subplots(figsize=(5, 5))
ig.plot(ADMG, target = ax,
    vertex_label= ["1","2","3","4","5","6","7"],   
    vertex_size=22,
    vertex_color="#77BAAA",
    vertex_label_size=14.0,
    vertex_label_color="black",
    edge_width=0.5,
    edge_color="black",
    edge_size=2.5,
    edge_arrow_size = 10,
    edge_arrow_width = 10,
    edge_curved="0"
)

# For the DAG above, the set of nodes m-connected to A = {1} given C = {5} is B = {1, 2, 3, 4, 5, 6} (not necessarily disjoint to A and C).
B_true = [1, 2, 3, 4, 5, 6]

# Write a function to solve m-connection.
# This function requires a rule table as string: 
def m_connected_with_string(G, A, C, m_connected_table):
    sets = {"A": A, "C": C}
    B = cf.reach(G, sets, m_connected_table, table_as_string = True)
    return sorted(B)

# The DAG as required by the function. 
G = {"-->": ADMG_directed_edges, "<->": ADMG_bidirected_edges}

# Sets
A = [1]
C = [5]

# Compute the set B
B = m_connected_with_string(G, A, C, m_connected_table)
print(B)

#Test
B == B_true

# Write a function to solve m-connection.
# This function requires to specify path to rule table: 
def m_connected_with_txt(G, A, C):
    sets = {"A": A, "C": C}
    table_path = "./m_connection_rule_table.txt"
    B = cf.reach(G, sets, table_path)
    return sorted(B)


