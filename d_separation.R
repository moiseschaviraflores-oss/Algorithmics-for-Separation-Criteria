# Title: Analysis of time-complexity for d-separation algorithms using the ciflyr package. 
# Date: May 7 2026

#This file includes code to show how to compute d-separation using the function reach() of the ciflyr package.
#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)

#The function reach() requires the following input arguments:
#1. "graph" a list which for each type of edges includes a 2-columns matrix of edges; 
#2. "sets" a list representing sequence of sets of nodes stored as vectors;
#3. "ruletable" can be a .txt file or a written string; 
#and two logical arguments: 
#4. "tableAsString" which must be TRUE if the "tablerule" is provided as a string.
#5. "verbose" which controls the printing of messages provided by "reach()".


#TOY EXAMPLE:
#Consider the DAG with adjacency matrix: 
Adj <- matrix(c(0, 1, 0, 0, 0, 0, 0,
                0, 0, 1, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0,
                0, 1, 0, 0, 1, 0, 0,
                0, 0, 0, 0, 0, 1, 0,
                0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 1, 0, 0), nrow = 7, byrow = TRUE)

# Create a graph object:
DAG <- graph_from_adjacency_matrix(Adj)

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


#The following string is the rule table for finding d-connected nodes to a set A by a set C:
d_connected_table <- "
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT ... 

--> | <-- | current in C
--> | --> | current not in C
<-- | --> | current not in C
<-- | <-- | current not in C
"
#We parse the ruletable such that "reach()" does not need addtional to pre-process it. 
d_connected_table <- parseRuletable(d_connected_table, tableAsString = TRUE)

#The DAG in the picture is stored in the format required by reach() as follows:
G <- list("-->" = rbind(c(1, 2), c(2, 3), c(4, 2), c(4, 5), c(5, 6), c(7, 5)))

#We parse the graph such that "reach()" does not need addtional to pre-process
G <- parseGraph(G, d_connected_table)

#We will be interested in the set B of all nodes that are d-connected to A={1} by the set C={3,6}
Sets <- list("A" = c(1), "C" = c(3, 6))
Sets <- parseSets(Sets, d_connected_table)

# If we do not care about output set to be disjoint, it must be
B_true <- c(1, 2, 3, 4, 5, 6, 7)

# The following function requires a rule table as string.
# It offers the possibility of computing a disjoint set form A and C (not disjoint by default).   
d_connected_with_string <- function(G, A, C, d_connected_table, disjoint = FALSE){
  # Store A and C in a list
  Sets <- list("A" = A, "C" = C)
  
  #Compute the set B of all nodes d_connected 
  B <- reach(G, Sets, d_connected_table, tableAsString = TRUE)
  
  #If disjoint == TRUE, then A, B and C must be disjoint
  if(disjoint == TRUE){B <- setdiff(B, c(A, C))}
  
  #Return nodes by order of labels
  return(sort(B))
}

# This provides 1, 2, 3, 4, 5, 6, 7
A <- c(1)
C <- c(3, 6)
B <- d_connected_with_string(G, A, C, d_connected_table)
print(B)

#We test
B == B_true

# If we ask A, B and C to be disjoint we must get 2, 4, 5, 7
B_disj <- d_connected_with_string(G, A, C, d_connected_table, disjoint = TRUE)
print(B_disj)


# A similar function could be writen if the rule table is provided as a .txt file. 
d_connected_with_path <- function(G, A, C, disjoint = FALSE){
  # Path to rule table
  tablePath <- "./d_connection_rule_table.txt"
  
  # Store A and C in a list
  Sets <- list("A" = A, "C" = C)
  
  #Compute the set B of all nodes d_connected 
  B <- reach(G, Sets, tablePath)
  
  #If disjoint == TRUE, then A, B and C must be disjoint
  if(disjoint == TRUE){B <- setdiff(B, c(A, C))}
  
  #Return nodes by order of labels
  return(sort(B))
}


##### This is the Time-Complexity Analysis ####
# It belongs to a nother script and should be ignored for now. 

#We can perform a empirical complexity analysis to verify whether the execution time for the task of
#computing the set B increases at linear rate with respect to the size p+m of the input graph G, where
#p=|V| and m=|E| are the numbers of nodes and edges in G respectively.

#The following function creates a DAG with a desired numbers of nodes and edges p and m respectively. 
generate_DAG_matrix <- function(p, m) {
  # Step 1: we set a random topological ordering
  nodes <- sample(1:p, p, replace = FALSE)
  
  # Step 2: We consider all possible edges respecting the given topological ordering.
  possible_edges <- matrix(NA, nrow = p * (p - 1) / 2, ncol = 2)
  k <- 1
  
  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      possible_edges[k, ] <- c(nodes[i], nodes[j])
      k <- k + 1
    }
  }
  
  possible_edges <- possible_edges[1:(k - 1), , drop = FALSE]
  
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


#generate_dag_matrix <- function(p, m) {
#  nodes <- sample(1:p)
  
#  possible_edges <- do.call(rbind, lapply(1:(p-1), function(i) {
#    cbind(nodes[i], nodes[(i+1):p])
#  }))
  
#  if (m > nrow(possible_edges)) {
#    stop("Too many edges requested for a DAG")
#  }
  
#  selected_edges <- possible_edges[sample(nrow(possible_edges), m), , drop = FALSE]
#  colnames(selected_edges) <- c("from", "to")
  
#  return(selected_edges)
#}


######## Complexity Analysis: 
#To verify how the execution time increases as the size a a graph increases, we generate random DAGs
#for different numbers p. 

#Dense graphs: The number of edges m is O(p(p-1)).
#P is a vector of numbers of nodes p=|V|.
P <- c(10, 20, 40, 80, 160, 320, 640, 920, 1280, 1800, 2000, 2400, 3000, 3600)

#M is a vector of numbers of edges m=|E|=O(p*(p-1))
M <- as.integer(0.24 * P *(P - 1)) 

#This vector will contain the execution average time for each element in P.
Time_d.sep1.1 <- rep(0, length(P)) 
Time_d.sep1.2 <- rep(0, length(P)) 

for (i in 1:length(P)){
  V <- 1:P[i] #We create the set of nodes V={1,...,p} for all p in P. 
  
  n_sim <- 10 #The number of simulated graphs for each graph size. 
  
  time_sim.1 <- rep(0, n_sim) #This vector will contain the number the execution times 
  time_sim.2 <- rep(0, n_sim)
  #for a particular number of nodes p. 
  
  for (j in 1:n_sim){
    G_edges <- generate_DAG_matrix(P[i], M[i]) #This generates a DAG
    n1 <- as.integer(P[i] * 0.4) #This is the number of nodes in the union AUC.
    n_A <- as.integer(P[i] * 0.2) #This is the number of nodes included in A.
    A_union_C <- sample(V, n1, replace = FALSE) #This samples AUC without replacement, 
    #because we take A and C to be disjoint. 
    Sets <- c(list("A" = A_union_C[1:n_A], "C" = A_union_C[(n_A+1):n1]))
    Sets <- parseSets(Sets, dConnected)
    G <- list("-->" = G_edges)
    G <- parseGraph(G, dConnected)
    
    t <- system.time(reach(G, Sets, dConnected, tableAsString = TRUE))
    cpu_time <- t["user.self"] + t["sys.self"]
    time_sim.1[j] <- cpu_time
    
    time1=Sys.time()
    reach(G, Sets, dConnected, tableAsString = TRUE)
    time2=Sys.time()
    time_sim.2[j] <- time2-time1
    
  }
  Time_d.sep1.1[i] <- mean(time_sim.1)
  Time_d.sep1.2[i] <- mean(time_sim.2)
}


#We plot the execution time vs p=|V| to visualize the increase rate
ggplot() + geom_line(aes(x = P, y = Time_d.sep1.1), color = 'red') + 
           geom_point(aes(x = P, y = Time_d.sep1.1), color = 'red') +
           geom_line(aes(x = P, y = Time_d.sep1.2), color = 'blue') + 
           geom_point(aes(x = P, y = Time_d.sep1.2), color = 'blue')
           xlab("p") + 
           ylab("Execution time") +
           ggtitle("Time Complexity of d-separation for dense DAGs m=O(p(p-1))")



#SPARSE GRAPHS: The number of edges m is O(p).
set.seed(1773)
#P <- c(10, 20, 40, 80, 160, 320, 640, 920, 1280, 1800, 2000, 2400, 3000, 3600)
P <- c(10, 50, 250, 1000, 5000, 25000, 100000, 500000, 2500000, 10000000,50000000)
M <- as.integer(1.5 * P)
Time_d.sep2.1 <- rep(0, length(P))
Time_d.sep2.2 <- rep(0, length(P))

for (i in 1:length(P)){
  V <- 1:P[i] #We create the set of nodes V={1,...,p} for all p in P. 
  
  n_sim <- 10 #The number of simulated graphs for each graph size. 
  
  time_sim.1 <- rep(0, n_sim) #This vector will contain the number the execution times 
  #for a particular number of nodes p.
  time_sim.2 <- rep(0, n_sim)
  
  for (j in 1:n_sim){
    G_edges <- generate_DAG_matrix(P[i], M[i]) #This generates a DAG
    n1 <- as.integer(P[i] * 0.4) #This is the number of nodes in the union AUC.
    n_A <- as.integer(P[i] * 0.2) #This is the number of nodes included in A.
    A_union_C <- sample(V, n1, replace = FALSE) #This samples AUC without replacement, 
    #because we take A and C to be disjoint. 
    Sets <- c(list("A" = A_union_C[1:n_A], "C" = A_union_C[(n_A+1):n1]))
    Sets <- parseSets(Sets, dConnected)
    G <- list("-->" = G_edges)
    G <- parseGraph(G, dConnected)
    
    t <- system.time(reach(G, Sets, dConnected, tableAsString = TRUE))
    cpu_time <- t["user.self"] + t["sys.self"]
    time_sim.1[j] <- cpu_time
    
    time1=Sys.time()
    reach(G, Sets, dConnected, tableAsString = TRUE)
    time2=Sys.time()
    time_sim.2[j] <- time2-time1
  }
  Time_d.sep2.1[i] <- mean(time_sim.1)
  Time_d.sep2.2[i] <- mean(time_sim.2)
}


#We can plot the execution time vs p=|V| to visually assess the increase rate
ggplot() + geom_line(aes(x = P, y = Time_d.sep2.1), color = 'red') + 
           geom_point(aes(x = P, y = Time_d.sep2.1), color = 'red') +
           geom_line(aes(x = P, y = Time_d.sep2.2), color = 'blue') + 
           geom_point(aes(x = P, y = Time_d.sep2.2), color = 'blue') +
           xlab("p") + ylab("Execution time") +
           ggtitle("Time Complexity of d-separation for sparse DAGs [m=O(p)]")

















