# Title: Algorithm to solve d-separation in Python. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

import ciflypy as cf
import igraph as ig
import matplotlib.pyplot as plt

d_connected_table = """
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT ... 

--> | <-- | current in C
--> | --> | current not in C
<-- | --> | current not in C
<-- | <-- | current not in C
"""

# Example: DAG as a list of (directed) edges in a 2-columns matrix
n_nodes = 7
DAG_edges = [(1, 2), (2, 3), (4, 2), (4, 5), (5, 6), (7, 5)]

# Create a graph object:
DAG = ig.Graph(n_nodes, DAG_edges, directed = True)
#We delete the additional 0-labeled node
DAG.delete_vertices(0)

# Plot the graph: 
fig, ax = plt.subplots(figsize=(5, 5))
ig.plot(DAG, target = ax,
    vertex_label= ["1","2","3","4","5","6","7"],   
    vertex_size=22,
    vertex_color="#77BAAA",
    vertex_label_size=14.0,
    vertex_label_color="black",
    edge_width=0.5,
    edge_color="black",
    edge_size=2.5,
    edge_arrow_size=0.35
)

# The following function requires a rule table as string to solve d-separation: 
def d_connected_with_string(G, A, C, d_connected_table):
    sets = {"A": A, "C": C}
    B = cf.reach(G, sets, d_connected_table, table_as_string = True)
    return sorted(B)

#Our graph
G = {"-->": DAG_edges}

#Sets
A=[1]
C=[3, 6]

d_connected_with_string(G, A, C, d_connected_table)
