# Title: Algorithm to solve *-separation in R. 
# Date: 17 May 2026
# Authors: Moisés Chavira Flores, Sebastian Weichwald, Leonard Henckel

#This file includes code to show how to compute *-separation using the function reach() of the ciflyr package.

#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)

#The following string is the rule table for finding *-connected nodes to a set A by a set C:
asterisk_connected_table <- "
EDGES --> <--
SETS A, C
COLORS before, after
START <-- [before] AT A
OUTPUT ... [...]

--> [before] | <-- [after]  | current in C
--> [before] | --> [before] | current not in C
<-- [before] | --> [before] | current not in C
<-- [before] | <-- [before] | current not in C
--> [after]  | --> [after]  | current not in C
<-- [after]  | --> [after]  | current not in C
<-- [after]  | <-- [after]  | current not in C
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

# For the DAG above, the set of nodes *-connected to A = {1} given C = {3, 6} is B = {1, 2, 3, 4, 5, 6} (not necessarily disjoint to A and C).
B_true <- c(1, 2, 3, 4, 5, 6)

# Write a function to solve *-connection. 
# It requires a rule table as string.
# It offers the possibility of computing a disjoint set form A and C (not disjoint by default).   
asterisk_connected_with_string <- function(G, A, C, asterisk_connected_table, disjoint = FALSE){
  Sets <- list("A" = A, "C" = C)
  B <- reach(G, Sets, asterisk_connected_table, tableAsString = TRUE)
  if(disjoint == TRUE){B <- setdiff(B, c(A, C))}
  return(sort(B))
}

# The DAG in the plot is stored as required by "reach".
G <- list("-->" = DAG_edges)

# required sets.
A <- c(1)
C <- c(3, 6)

# Compute the set B
B <- asterisk_connected_with_string(G, A, C, asterisk_connected_table)
print(B)

# We test
B == B_true

# If we ask A, B and C to be disjoint we must get B_disj = {2, 4, 5}
B_disj <- asterisk_connected_with_string(G, A, C, asterisk_connected_table, disjoint = TRUE)
print(B_disj)

# Write a function to solve *-connection.
# This function requires to specify path to rule table: 
asterisk_connected_with_txt <- function(G, A, C){
  sets = list("A" = A, "C" = C)
  table_path = "./asterisk_connection_rule_table.txt"
  B = reach(G, sets, table_path)
  return(sort(B))
}







#We can perform a empirical complexity analysis to verify whether the execution time for the task of
#computing the set B increases at linear rate with respect to the size p+m of the input graph G, where
#p=|V| and m=|E| are the numbers of nodes and edges in G respectively.

#The following function creates a DAG with a desired numbers of nodes p and m respectively. 
generate_DAG_matrix <- function(p, m) {
  # Step 1: we set a random topological ordering
  nodes <- sample(1:p, p, replace = FALSE)
  
  # Step 2: We consider all possible edges respecting the given topological ordering.
  possible_edges <- matrix(NA, nrow = p*(p-1)/2, ncol = 2)
  k <- 1
  
  for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      possible_edges[k, ] <- c(nodes[i], nodes[j])
      k <- k + 1
    }
  }
  
  possible_edges <- possible_edges[1:(k-1), , drop = FALSE]
  
  # This checks that the asked number of edges does not overpasses the maximum possible number
  max_edges <- nrow(possible_edges)
  if (m > max_edges) {
    stop("Too many edges requested for a DAG")
  }
  
  # Step 3: sample m edges
  selected_idx <- sample(1:max_edges, m, replace = FALSE)
  selected_edges <- possible_edges[selected_idx, , drop = FALSE]
  
  return(selected_edges)
}


#Now we will use the function "generate_DAG_matrix" to run some simulations for assessing the time
#complexity of this algorithmic implementation:
#DENSE GRAPHS: Note that in this case the number of edges m is O(p(p-1)), i.e, we are analysing dense graphs
P=c(5,10,20,40,80,160,320,640,920,1280,1800)
M=as.integer(0.24*P*(P-1))
#M=as.integer(0.24*P*(sqrt(sqrt(P))))
#M[1]=21
#M[2]=588
#M=as.integer(P)
S=P+M
Time_star.sep=rep(0,length(S))
for (i in 1:length(S)){
  V=1:P[i]
  n_sim=3
  time_sim=rep(0,n_sim)
  for (j in 1:n_sim){
    G_edges=generate_DAG_matrix(P[i],M[i])
    n1=as.integer(P[i]*0.4)
    n2=as.integer(P[i]*0.2)
    A_C=sample(V,n1,replace = FALSE)
    Sets=c(list("A"=A_C[1:n2],"C"=A_C[(n2+1):n1]))
    Sets=parseSets(Sets,StarConnected)
    G=list("-->"=G_edges)
    G=parseGraph(G,StarConnected)
    t <- system.time(reach(G,Sets, StarConnected, tableAsString = TRUE))
    cpu_time <- t["user.self"] + t["sys.self"]
    #time1=Sys.time()
    #reach(G,Sets, StarConnected, tableAsString = TRUE)
    #time2=Sys.time()
    #time_sim[j]=time2-time1
    time_sim[j]=cpu_time
  }
  
  Time_star.sep[i]=mean(time_sim)
}

#We can plot the execution time vs p+m=|V|+|E| to visually assess the increase rate
ggplot() + geom_line(aes(x=P,y=Time_star.sep),color='red') + 
           geom_point(aes(x=P,y=Time_star.sep),color='red') +
           xlab("p") + ylab("Execution time") +
           ggtitle("Time Complexity of *-separation for dense DAGs [m=O(p(p-1))]")


#We can also analyse time complexity for the case of sparse DAGs whose number of edges is
#m=O(p). 
P=c(5,10,20,40,80,160,320,640,920,1280,1800,2300)
#M=as.integer(0.24*P*(P-1))
#M=as.integer(0.24*P*(sqrt(sqrt(P))))
#M[1]=21
#M[2]=588
M=as.integer(2*P)
S=P+M
Time_star.sep=rep(0,length(S))
for (i in 1:length(S)){
  V=1:P[i]
  n_sim=20
  time_sim=rep(0,n_sim)
  for (j in 1:n_sim){
    G_edges=generate_DAG_matrix(P[i],M[i]) 
    n1=as.integer(P[i]*0.4)
    n2=as.integer(P[i]*0.2)
    A_C=sample(V,n1,replace = FALSE)
    Sets=c(list("A"=A_C[1:n2],"C"=A_C[(n2+1):n1]))
    Sets=parseSets(Sets,StarConnected)
    G=list("-->"=G_edges)
    G=parseGraph(G,StarConnected)
    #t = system.time(reach(G,Sets, StarConnected, tableAsString = TRUE))
    #cpu_time = t["user.self"] + t["sys.self"]
    #time_sim[j]=cpu_time
    time1=Sys.time()
    reach(G,Sets, StarConnected, tableAsString = TRUE)
    time2=Sys.time()
    time_sim[j]=time2-time1    
  }
  
  Time_star.sep[i]=mean(time_sim)
}

#We can plot the execution time vs p+m=|V|+|E| to visually assess the increase rate
ggplot() + geom_line(aes(x=P,y=Time_star.sep),color='red') + 
  geom_point(aes(x=P,y=Time_star.sep),color='red') +
  xlab("p") + ylab("Execution time") +
  ggtitle("Time Complexity of *-separation for sparse DAGs [m=O(p)]")









