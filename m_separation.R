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





# Ignore code below
###########################################################
###########################################################
#This file includes code to show how to compute m-separation using the library "ciflyr"
#We use the following libraries. They can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)

#ADMG models: m-separation
#m-connection
mConnected = "
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
mConnected = parseRuletable(mConnected,tableAsString=TRUE)

generate_ADMG <- function(p, m_dir, m_bidir) {
  # Step 1: random topological order
  nodes <- sample(1:p)
  
  # Step 2: all possible directed edges (acyclic)
  possible_dir <- do.call(rbind, lapply(1:(p-1), function(i) {
    cbind(nodes[i], nodes[(i+1):p])
  }))
  
  max_dir <- nrow(possible_dir)
  if (m_dir > max_dir) {
    stop("Too many directed edges requested")
  }
  
  # Sample directed edges
  dir_edges <- possible_dir[sample(max_dir, m_dir), , drop = FALSE]
  colnames(dir_edges) <- c("from", "to")
  
  # Step 3: all possible bidirected edges (unordered pairs)
  possible_bidir <- t(combn(1:p, 2))  # all pairs i < j
  
  # Remove pairs already used as directed edges (optional but cleaner)
  # (so we don't have both x->y and x<->y unless you want that)
  dir_pairs <- t(apply(dir_edges, 1, function(x) sort(x)))
  keep <- !apply(possible_bidir, 1, function(pair) {
    any(apply(dir_pairs, 1, function(dp) all(dp == pair)))
  })
  
  possible_bidir <- possible_bidir[keep, , drop = FALSE]
  
  max_bidir <- nrow(possible_bidir)
  if (m_bidir > max_bidir) {
    stop("Too many bidirected edges requested")
  }
  
  # Sample bidirected edges
  bidir_edges <- possible_bidir[sample(max_bidir, m_bidir), , drop = FALSE]
  colnames(bidir_edges) <- c("node1", "node2")
  
  return(list(
    directed = dir_edges,
    bidirected = bidir_edges
  ))
}

G_edges=generate_ADMG(12,6,6)
G=list("-->"=G_edges$directed, "<->"=G_edges$bidirected)
G=parseGraph(G, mConnected)

Sets=list("A" = c(1), "C" = c(3, 6))
Sets=parseSets(Sets,mConnected)

reach(G,Sets,mConnected)

P=c(8,15,40,80,160,320,640,920,1280,1800,2200,2800)
M1=as.integer(0.24*0.65*P*(P-1))
M2=as.integer(0.24*0.35*P*(P-1))
S=P+M1+M2
Time_m.sep1=rep(0,length(S))
for (i in 1:length(S)){
  V=1:P[i]
  n_sim=2
  time_sim=rep(0,n_sim)
  for (j in 1:n_sim){
    G_edges=generate_ADMG(P[i],M1[i],M2[i]) 
    n1=as.integer(P[i]*0.4)
    n2=as.integer(P[i]*0.2)
    A_C=sample(V,n1,replace = FALSE)
    Sets=c(list("A"=A_C[1:n2],"C"=A_C[(n2+1):n1]))
    Sets=parseSets(Sets,mConnected)
    G=list("-->"=G_edges$directed,"<->"=G_edges$bidirected)
    G=parseGraph(G,mConnected)
    #t <- system.time(reach(G,Sets, deltaConnected, tableAsString = TRUE))
    #cpu_time <- t["user.self"] + t["sys.self"]
    time1=Sys.time()
    reach(G,Sets,mConnected)
    time2=Sys.time()
    time_sim[j]=time2-time1
    #time_sim[j]=cpu_time
  }
  
  Time_m.sep1[i]=mean(time_sim)
}

#We can plot the execution time vs p+m=|V|+|E| to visually assess the increase rate
ggplot() + geom_line(aes(x=P,y=Time_m.sep1),color='red') + 
  geom_point(aes(x=P,y=Time_m.sep1),color='red') +
  xlab("p") + ylab("Execution time") +
  ggtitle("Time Complexity of m-separation for dense ADMGs [m1+m2=O(p*(p-1))]")



#Sparse Graphs:
P=c(8,15,40,80,160,320,640,920,1280,1800,2200,2800)
M1=as.integer(2*0.65*P)
M2=as.integer(2*0.35*P)
S=P+M1+M2
Time_m.sep1=rep(0,length(S))
for (i in 1:length(S)){
  V=1:P[i]
  n_sim=2
  time_sim=rep(0,n_sim)
  for (j in 1:n_sim){
    G_edges=generate_ADMG(P[i],M1[i],M2[i]) 
    n1=as.integer(P[i]*0.4)
    n2=as.integer(P[i]*0.2)
    A_C=sample(V,n1,replace = FALSE)
    Sets=c(list("A"=A_C[1:n2],"C"=A_C[(n2+1):n1]))
    Sets=parseSets(Sets,mConnected)
    G=list("-->"=G_edges$directed,"<->"=G_edges$bidirected)
    G=parseGraph(G,mConnected)
    #t <- system.time(reach(G,Sets, deltaConnected, tableAsString = TRUE))
    #cpu_time <- t["user.self"] + t["sys.self"]
    time1=Sys.time()
    reach(G,Sets,mConnected)
    time2=Sys.time()
    time_sim[j]=time2-time1
    #time_sim[j]=cpu_time
  }
  
  Time_m.sep1[i]=mean(time_sim)
}

#We can plot the execution time vs p+m=|V|+|E| to visually assess the increase rate
ggplot() + geom_line(aes(x=P,y=Time_m.sep1),color='red') + 
  geom_point(aes(x=P,y=Time_m.sep1),color='red') +
  xlab("p") + ylab("Execution time") +
  ggtitle("Time Complexity of m-separation for sparse ADMGs [m1+m2=O(p)]")



