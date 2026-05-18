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

#The following string is the rule table for finding delta-connected nodes to a set A by a set C:
delta_connected_table <- "
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT --> 

--> | <-- | current in C
--> | --> | current not in C
<-- | --> | current not in C
<-- | <-- | current not in C
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

# For the DAG above, the set of nodes d-connected to A = {1} given C = {3, 6} is B = {2, 3, 4, 5, 6}.
B_true <- c(2, 3, 4, 5, 6)

# Write a function to solve delta-connection. 
# It requires a rule table as string.
# It offers the possibility of computing a disjoint set form A and C (not disjoint by default).   
delta_connected_with_string <- function(G, A, C, delta_connected_table, disjoint = FALSE){
  Sets <- list("A" = A, "C" = C)
  B <- reach(G, Sets, delta_connected_table, tableAsString = TRUE)
  if(disjoint == TRUE){B <- setdiff(B, c(A, C))}
  return(sort(B))
}

# The DG in the plot is stored as required by "reach".
G <- list("-->" = DG_edges)

# required sets.
A <- c(1)
C <- c(3, 6)

# Compute the set B
B <- delta_connected_with_string(G, A, C, delta_connected_table)
print(B)

# We test
B == B_true

# If we ask A, B and C to be disjoint we must get B_disj = {2, 4, 5}
B_disj <- delta_connected_with_string(G, A, C, delta_connected_table, disjoint = TRUE)
print(B_disj)

# Write a function to solve d-connection.
# This function requires to specify path to rule table: 
delta_connected_with_txt <- function(G, A, C){
  # Sets
  sets = list("A" = A, "C" = C)
  
  # Path to table
  table_path = "./delta_connected_rule_table.txt"
  
  # Compute the set B of all nodes d_connected 
  B = reach(G, sets, table_path)
  
  #Return nodes by order of labels
  return(sort(B))
}




#######################################################################################
# This belongs to Time Complexity Analysis
#Now we will use the function "generate_DAG_matrix" to run some simulations for assessing the time
#complexity of this algorithmic implementation:
#Dense graphs: Note that in this case the number of edges m is O(p(p-1)), i.e, we are analysing dense graphs
P=c(8,15,40,80,160,320,640,920,1280,1800,2200,2800)
M=as.integer(0.24*P*(P-1))
#M=as.integer(0.24*P*(sqrt(sqrt(P))))
#M[1]=21
#M[2]=588
#M=as.integer(P)
S=P+M
Time_delta.sep1=rep(0,length(S))
for (i in 1:length(S)){
  V=1:P[i]
  n_sim=20
  time_sim=rep(0,n_sim)
  for (j in 1:n_sim){
    G_edges=generate_directed_graph_matrix(P[i],M[i]) 
    n1=as.integer(P[i]*0.4)
    n2=as.integer(P[i]*0.2)
    A_C=sample(V,n1,replace = FALSE)
    Sets=c(list("A"=A_C[1:n2],"C"=A_C[(n2+1):n1]))
    Sets=parseSets(Sets,deltaConnected)
    G=list("-->"=G_edges)
    G=parseGraph(G,deltaConnected)
    #t <- system.time(reach(G,Sets, deltaConnected, tableAsString = TRUE))
    #cpu_time <- t["user.self"] + t["sys.self"]
    time1=Sys.time()
    reach(G,Sets,deltaConnected)
    time2=Sys.time()
    time_sim[j]=time2-time1
    #time_sim[j]=cpu_time
  }
  
  Time_delta.sep1[i]=mean(time_sim)
}

#We can plot the execution time vs p+m=|V|+|E| to visually assess the increase rate
ggplot() + geom_line(aes(x=P,y=Time_delta.sep1),color='red') + 
  geom_point(aes(x=P,y=Time_delta.sep1),color='red') +
  xlab("p+m") + ylab("Execution time") +
  ggtitle("Time Complexity of delta-separation for dense DAGs")



#We can also analyse time complexity for the case of sparse DAGs whose number of edges is
#m=O(p). 
P=c(10,20,40,80,160,320,640,920,1280,1800,2200,2800)
#M=as.integer(0.24*P*(P-1))
#M=as.integer(0.24*P*(sqrt(sqrt(P))))
#M[1]=21
#M[2]=588
M=as.integer(2*P)
S=P+M
Time_delta.sep2=rep(0,length(S))
for (i in 1:length(S)){
  V=1:P[i]
  n_sim=20
  time_sim=rep(0,n_sim)
  for (j in 1:n_sim){
    G_edges=generate_directed_graph_matrix(P[i],M[i]) 
    n1=as.integer(P[i]*0.4)
    n2=as.integer(P[i]*0.2)
    A_C=sample(V,n1,replace = FALSE)
    Sets=c(list("A"=A_C[1:n2],"C"=A_C[(n2+1):n1]))
    Sets=parseSets(Sets,deltaConnected)
    G=list("-->"=G_edges)
    G=parseGraph(G,deltaConnected)
    #t <- system.time(reach(G,Sets, deltaConnected, tableAsString = TRUE))
    #cpu_time <- t["user.self"] + t["sys.self"]
    time1=Sys.time()
    reach(G,Sets,deltaConnected)
    time2=Sys.time()
    time_sim[j]=time2-time1
    #time_sim[j]=cpu_time    
  }
  
  Time_delta.sep2[i]=mean(time_sim)
}

#We can plot the execution time vs p+m=|V|+|E| to visually assess the increase rate
ggplot() + geom_line(aes(x=P,y=Time_delta.sep2),color='red') + 
  geom_point(aes(x=P,y=Time_delta.sep2),color='red') +
  xlab("p") + ylab("Execution time") +
  ggtitle("Time Complexity of delta-separation for sparse DAGs [m=O(p)]")



