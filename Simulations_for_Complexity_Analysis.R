#Title: Time-Complexity Analysis for Graphical Separation Criteria:
#Date: May 8th 2026

#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)

#This file includes rule tables written as strings for eight different separation criteria.
#It also include code for generating graphs of different sizes and measuring execution time.
#This code aims to empirically verify the theoretical claim that these algorithms can be run in linear time. 

#For DAG-based models the separation criteria are:
# d-separation;
# *-separation; 
# t-separation.

#The corresponding rule tables are the following.
#We use the function "parseRuletable" to pre-process rule table strings. 
#This avoids that the function "reach" takes additional time.

#Rule table to find nodes d-connected to A by C:
dConnected <- "
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT ...

-->  | <--  | current in C
-->  | -->  | current not in C
<--  | -->  | current not in C
<--  | <--  | current not in C
"
dConnected <- parseRuletable(dConnected, tableAsString = TRUE)


#Rule table to find nodes d-connected to A by C:
starConnected <- "
EDGES --> <--
SETS A, C
COLORS before, after
START <-- [before] AT A
OUTPUT ... [after]

--> [before] | <-- [after]  | current in C
--> [before] | --> [before] | current not in C
<-- [before] | --> [before] | current not in C
<-- [before] | <-- [before] | current not in C
--> [after]  | --> [after]  | current not in C
<-- [after]  | --> [after]  | current not in C
<-- [after]  | <-- [after]  | current not in C
"
starConnected <- parseRuletable(starConnected, tableAsString = TRUE)

#Rule table to find nodes t-connected to A by (C_A, C_B): 
tConnected <- "
EDGES --> <--
SETS A, C_A, C_B
START <-- AT A
OUTPUT ...

<--  | <--  | current not in C_A
-->  | -->  | current not in C_B
<--  | -->  | current not in C_A and current not in C_B
"
tConnected <- parseRuletable(tConnected, tableAsString = TRUE)


#The following function generates a DAG with given number of nodes and edges p and m respectively:
generate_dag_matrix <- function(p, m) {
  nodes <- sample(1:p)
  
  possible_edges <- do.call(rbind, lapply(1:(p-1), function(i) {
    cbind(nodes[i], nodes[(i+1):p])
  }))
  
  if (m > nrow(possible_edges)) {
    stop("Too many edges requested for a DAG")
  }
  
  selected_edges <- possible_edges[sample(nrow(possible_edges), m), , drop = FALSE]
  colnames(selected_edges) <- c("from", "to")
  
  return(selected_edges)
}


#Alternative function
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


######
#SPARSE GRAPHS: The number of edges m is O(p).
#P <- c(10, 20, 40, 80, 160, 320, 640, 920, 1280, 1800, 2000, 2400, 3000, 3600)
#P <- c(10, 50, 250, 1000, 5000, 25000, 100000, 500000, 2500000, 10000000,50000000)
P <- c(100, 1000, 2000, 4000, 6000, 8000, 10000, 12000, 14000) 

#We take the number of edges to grow linearly with respect to the number of nodes. 
M <- as.integer(5 * P)

#This vector will contain the average execution time all numbers of nodes in P. 
Time_d.sep_sparse <- rep(0, length(P)) 
Time_star.sep_sparse <- rep(0, length(P)) 
Time_t.sep_sparse <- rep(0, length(P)) 

set.seed(1773)
for (i in 1:length(P)){
  #We create the set of nodes V={1,...,P[i]}. 
  V <- 1:P[i]  
  
  #The number of simulated graphs for each graph size.
  n_sim <- 12  
  
  #This vector will contain the number the execution times for a particular number of nodes p
  time_sim_d <- rep(0, n_sim) 
  time_sim_star <- rep(0, n_sim) 
  time_sim_t <- rep(0, n_sim) 
  
  for (j in 1:n_sim){
    #Generate random graph with P[i] nodes.
    G_edges <- generate_dag_matrix(P[i], M[i]) #This generates a DAG
    
    #Number of nodes in the union AUC (40% of the size of V)
    n1 <- as.integer(P[i] * 0.4) 
    
    #Number of nodes in A (20% of the size of V) 
    n_A <- as.integer(P[i] * 0.2) 
    
    #We sample AUC without replacement because A and C must be disjoint.
    A_union_C <- sample(V, n1, replace = FALSE)
    
    #Exclusively for t-connection we need two sets C_A and C_B not necessarily disjoint.
    #Both of them will have 15% of the size of V.
    n.C_A <- as.integer(P[i] * 0.15)
    n.C_B <- as.integer(P[i] * 0.15)
    
    C_A <- sample(V, n.C_A, replace = FALSE)
    C_B <- sample(V, n.C_B, replace = FALSE)
    
    #We split the union AUB into A and B, and parse to pre-process. 
    Sets <- list("A" = A_union_C[1:n_A], "C" = A_union_C[(n_A+1):n1])
    #This will be used for d-connection
    Sets_d <- parseSets(Sets, dConnected)
    #This will be used for *-connection
    Sets_star <- parseSets(Sets, starConnected)
    #This will be used for t-connection
    Sets_t_raw <- list("A" = A_union_C[1:n_A], "C_A" = C_A, "C_B" = C_B)
    Sets_t <- parseSets(Sets_t_raw, tConnected)
    
    #We pre-process the generated graph for the three criteria, d, *, and t connection. 
    G <- list("-->" = G_edges)
    G_d <- parseGraph(G, dConnected)
    G_star <- parseGraph(G, starConnected)
    G_t <- parseGraph(G, tConnected)
    
    #We run "reach" and measure execution time for d-connection 
    t_d <- system.time(reach(G_d, Sets_d, dConnected))
    cpu_time_d <- t_d["user.self"] + t_d["sys.self"]
    time_sim_d[j] <- cpu_time_d
    
    #We run "reach" and measure execution time for *-conenction 
    t_star <- system.time(reach(G_star, Sets_star, starConnected))
    cpu_time_star <- t_star["user.self"] + t_star["sys.self"]
    time_sim_star[j] <- cpu_time_star
    
    #We run "reach" and measure execution time for t-conenction 
    t_t <- system.time(reach(G_t, Sets_t, tConnected))
    cpu_time_t <- t_t["user.self"] + t_t["sys.self"]
    time_sim_t[j] <- cpu_time_t
    
    #time1=Sys.time()
    #reach(G, Sets, dConnected, tableAsString = TRUE)
    #time2=Sys.time()
    #time_sim.2[j] <- time2-time1
    
  }
  #We store the average execution time for 
  Time_d.sep_sparse[i] <- mean(time_sim_d)
  Time_star.sep_sparse[i] <- mean(time_sim_star)
  Time_t.sep_sparse[i] <- mean(time_sim_t)
}


#We can plot the execution time vs p=|V| to visually assess the increase rate
plot_d_sparse <- ggplot() + 
                   geom_line(aes(x = P, y = Time_d.sep_sparse), color = 'navyblue') + 
                   geom_point(aes(x = P, y = Time_d.sep_sparse), color = 'navyblue') +
                   xlab("p") + 
                   ylab("Execution time") +
                   ggtitle("Time Complexity of d-separation for sparse DAGs [m=O(p)]")

plot_star_sparse <- ggplot() + 
                    geom_line(aes(x = P, y = Time_star.sep_sparse), color = 'navyblue') + 
                    geom_point(aes(x = P, y = Time_star.sep_sparse), color = 'navyblue') +
                    xlab("p") + 
                    ylab("Execution time") +
                    ggtitle("Time Complexity of *-separation for sparse DAGs [m=O(p)]")

plot_t_sparse <- ggplot() + 
                   geom_line(aes(x = P, y = Time_t.sep_sparse), color = 'navyblue') + 
                   geom_point(aes(x = P, y = Time_t.sep_sparse), color = 'navyblue') +
                   xlab("p") + 
                   ylab("Execution time") +
                   ggtitle("Time Complexity of t-separation for sparse DAGs [m=O(p)]")



#DENSE GRAPHS: The number of edges m is O(p).
#P <- c(10, 20, 40, 80, 160, 320, 640, 920, 1280, 1800, 2000, 2400, 3000, 3600)
#P <- c(10, 50, 250, 1000, 5000, 25000, 100000, 500000, 2500000, 10000000,50000000)
P <- c(100, 1000, 2000, 4000, 8000, 10000, 12000, 14000) 

#We take the number of edges to grow linearly with respect to the number of nodes. 
M <- as.integer(0.20 * P * (P - 1))

#This vector will contain the average execution time all numbers of nodes in P. 
Time_d.sep_dense <- rep(0, length(P)) 
#Time_star.sep_dense <- rep(0, length(P)) 
#Time_t.sep_dense <- rep(0, length(P)) 

set.seed(1773)
for (i in 1:length(P)){
  #We create the set of nodes V={1,...,P[i]}. 
  V <- 1:P[i]  
  
  #The number of simulated graphs for each graph size.
  n_sim <- 12  
  
  #This vector will contain the number the execution times for a particular number of nodes p
  time_sim_d <- rep(0, n_sim) 
  #time_sim_star <- rep(0, n_sim) 
  #time_sim_t <- rep(0, n_sim) 
  
  for (j in 1:n_sim){
    #Generate random graph with P[i] nodes.
    G_edges <- generate_dag_matrix(P[i], M[i]) #This generates a DAG
    
    #Number of nodes in the union AUC (40% of the size of V)
    n1 <- as.integer(P[i] * 0.4) 
    
    #Number of nodes in A (20% of the size of V) 
    n_A <- as.integer(P[i] * 0.2) 
    
    #We sample AUC without replacement because A and C must be disjoint.
    A_union_C <- sample(V, n1, replace = FALSE)
    
    #Exclusively for t-connection we need two sets C_A and C_B not necessarily disjoint.
    #Both of them will have 15% of the size of V.
    #n.C_A <- as.integer(P[i] * 0.15)
    #n.C_B <- as.integer(P[i] * 0.15)
    
    #C_A <- sample(V, n.C_A, replace = FALSE)
    #C_B <- sample(V, n.C_B, replace = FALSE)
    
    #We split the union AUB into A and B, and parse to pre-process. 
    Sets <- list("A" = A_union_C[1:n_A], "C" = A_union_C[(n_A+1):n1])
    #This will be used for d-connection
    Sets_d <- parseSets(Sets, dConnected)
    #This will be used for *-connection
    #Sets_star <- parseSets(Sets, starConnected)
    #This will be used for t-connection
    #Sets_t_raw <- list("A" = A_union_C[1:n_A], "C_A" = C_A, "C_B" = C_B)
    #Sets_t <- parseSets(Sets_t_raw, tConnected)
    
    #We pre-process the generated graph for the three criteria, d, *, and t connection. 
    G <- list("-->" = G_edges)
    G_d <- parseGraph(G, dConnected)
    #G_star <- parseGraph(G, starConnected)
    #G_t <- parseGraph(G, tConnected)
    
    #We run "reach" and measure execution time for d-connection 
    t_d <- system.time(reach(G_d, Sets_d, dConnected))
    cpu_time_d <- t_d["user.self"] + t_d["sys.self"]
    time_sim_d[j] <- cpu_time_d
    
    #We run "reach" and measure execution time for *-conenction 
    #t_star <- system.time(reach(G_star, Sets_star, starConnected))
    #cpu_time_star <- t_star["user.self"] + t_star["sys.self"]
    #time_sim_star[j] <- cpu_time_star
    
    #We run "reach" and measure execution time for t-conenction 
    #t_t <- system.time(reach(G_t, Sets_t, tConnected))
    #cpu_time_t <- t_t["user.self"] + t_t["sys.self"]
    #time_sim_t[j] <- cpu_time_t
    
    #time1=Sys.time()
    #reach(G, Sets, dConnected, tableAsString = TRUE)
    #time2=Sys.time()
    #time_sim.2[j] <- time2-time1
    
  }
  #We store the average execution time for 
  Time_d.sep_dense[i] <- mean(time_sim_d)
  #Time_star.sep_dense[i] <- mean(time_sim_star)
  #Time_t.sep_dense[i] <- mean(time_sim_t)
}


#We can plot the execution time vs p=|V| to visually assess the increase rate
plot_d_dense <- ggplot() + 
  geom_line(aes(x = P+M, y = Time_d.sep_dense), color = 'navyblue') + 
  geom_point(aes(x = P+M, y = Time_d.sep_dense), color = 'navyblue') +
  xlab("p+m") + 
  ylab("Execution time") +
  ggtitle("Time Complexity of d-separation for dense DAGs [m=O(p)]")

plot_star_dense <- ggplot() + 
  geom_line(aes(x = P, y = Time_star.sep_dense), color = 'navyblue') + 
  geom_point(aes(x = P, y = Time_star.sep_dense), color = 'navyblue') +
  xlab("p") + 
  ylab("Execution time") +
  ggtitle("Time Complexity of *-separation for dense DAGs [m=O(p)]")

plot_t_dense <- ggplot() + 
  geom_line(aes(x = P, y = Time_t.sep_dense), color = 'navyblue') + 
  geom_point(aes(x = P, y = Time_t.sep_dense), color = 'navyblue') +
  xlab("p") + 
  ylab("Execution time") +
  ggtitle("Time Complexity of t-separation for dense DAGs [m=O(p)]")



# --------------------------------------
#For models based on directed graphs (not necessarily acyclic) the separation criteria are:
# delta-separation;
# c-separation; 
# epsilon-separation.

#The corresponding rule tables are the following.
#We use the function "parseRuletable" to pre-process rule table strings. 
#This avoids that the function "reach" takes additional time.

#Rule table to find nodes delta-connected to A by C:
deltaConnected = "
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT --> 

--> | <-- | current in C
--> | --> | current not in C
<-- | --> | current not in C
<-- | <-- | current not in C
"

#Rule table to find nodes c-connected to A by C:
cConnected = "
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT ...

--> | <-- | current not in C
"

#Rule table to find nodes c-connected to A by C:
epsilonConnected = "
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
"








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



# DMG (directed mixed graphs) models with cycles: mu-separation and sigma-separation
#mu-connection
muConnected = "
EDGES --> <--, <->
SETS X, Z
START <-- AT X
OUTPUT -->,<->

--> | <-- | current in Z
<-> | <-- | current in Z
--> | <-> | current in Z
<-> | <-> | current in Z
--> | --> | current not in Z
<-- | --> | current not in Z
<-- | <-- | current not in Z
<-> | --> | current not in Z
<-- | <-> | current not in Z
"

#sigma-connection: this separation criterion is different, because the number of sets in the input is
#not fixed. This we write a function to automatically generate a CIfly-rule table for a graph with a 
#particular number of strongly connected components. 

#The next function assumes that the sets of strongly connected components are already given, that is,
#we have a partition for the set of nodes V={1,...,p}=C_1\cup ...\cup C_q, according to the definition
#of strongly connected components. 
#Function to generate rule table for sigma-connection in mixed graphs:
sigma_table_mixed_graphs=function(p){
  
  #Write sets of strongly connected components \sigma(i)=C_i  
  SCC = paste0("C_", 1:p)  
  
  #Write a chain for "current in \sigma(next)"
  current_in_sigma.next = paste(
    paste0("(current in ", SCC, " and next in ", SCC, ")"),
    collapse = " or ")  
  
  rule1 = "current in Z and next not in Z"
  
  rule2 = paste("current in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule3 = rule1
  
  rule4 = rule2
  
  rule5 = "current in Z"
  
  rule6 = rule5
  
  rule7 = paste("current not in Z or (current in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule8 = rule7
  
  rule9 = paste("current not in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule10 = "current not in Z and next not in Z"
  
  rule11 = paste("next in Z and (",current_in_sigma.next,")",sep="")
  
  rule12 = "next not in Z"
  
  rule13 = "current not in Z"
  
  rule14 = "true"
  
  rule15 = rule13
  
  rule16 = current_in_sigma.next
  
  # Write CIfly Rule Table
  sigmaConnected=paste(
    "EDGES --> <--, <->", 
    paste("SETS", paste(c("X","Z", SCC), collapse = ",")),
    "COLORS white, blue, red, green",
    "START <-- [red] AT X",
    "OUTPUT ...",
    "",
    paste("--> [blue]  | <-- [red]   |", rule1),
    paste("--> [blue]  | <-- [green] |", rule2),
    paste("<-> [white] | <-- [red]   |", rule3),
    paste("<-> [white] | <-- [green] |", rule4), 
    paste("--> [blue]  | <-> [white] |", rule5), 
    paste("<-> [white] | <-> [white] |", rule6), 
    paste("--> [blue]  | --> [blue]  |", rule7), 
    paste("<-> [white] | --> [blue]  |", rule8), 
    paste("<-- [red]   | <-- [green] |", rule9), 
    paste("<-- [red]   | <-- [red]   |", rule10), 
    paste("<-- [green] | <-- [green] |", rule11), 
    paste("<-- [green] | <-- [red]   |", rule12), 
    paste("<-- [red]   | <-> [white] |", rule13), 
    paste("<-- [green] | <-> [white] |", rule14), 
    paste("<-- [red]   | --> [blue]  |", rule15), 
    paste("<-- [green] | --> [blue]  |", rule16), 
    sep = "\n")
  
  # Print table
  return(sigmaConnected)
}

#
sigma_table_mixed_graphs2=function(p){
  
  #Write sets of strongly connected components \sigma(i)=C_i  
  SCC = paste0("C_", 1:p)  
  
  #Write a chain for "current in \sigma(next)"
  current_in_sigma.next = paste(
    paste0("(current in ", SCC, " and next in ", SCC, ")"),
    collapse = " or ")  
  
  rule1 = "current in Z and next not in Z"
  
  rule2 = paste("current in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule3 = rule1
  
  rule4 = rule2
  
  rule5 = "current in Z"
  
  rule6 = rule5
  
  rule7 = paste("current not in Z or (current in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule8 = rule7
  
  rule9 = paste("current not in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule10 = "current not in Z and next not in Z"
  
  rule11 = paste("next in Z and (",current_in_sigma.next,")",sep="")
  
  rule12 = "next not in Z"
  
  rule13 = "current not in Z"
  
  rule14 = "true"
  
  rule15 = rule13
  
  rule16 = current_in_sigma.next
  
  # Write CIfly Rule Table
  sigmaConnected=paste(
    "EDGES --> <--, <->", 
    paste("SETS", paste(c("X","Z", SCC), collapse = ",")),
    "COLORS bidir, rightdir, outZ, formereq",
    "START <-- [red] AT X",
    "OUTPUT ...",
    "",
    paste("--> [rightdir] | <-- [outZ]     |", rule1),
    paste("--> [rightdir] | <-- [formereq] |", rule2),
    paste("<-> [bidir]    | <-- [outZ]     |", rule3),
    paste("<-> [bidir]    | <-- [formereq] |", rule4), 
    paste("--> [rightdir] | <-> [bidir]    |", rule5), 
    paste("<-> [bidir]    | <-> [bidir]    |", rule6), 
    paste("--> [rightdir] | --> [rightdir] |", rule7), 
    paste("<-> [bidir]    | --> [rightdir] |", rule8), 
    paste("<-- [outZ]     | <-- [formereq] |", rule9), 
    paste("<-- [outZ]     | <-- [outZ]     |", rule10), 
    paste("<-- [formereq] | <-- [formereq] |", rule11), 
    paste("<-- [formereq] | <-- [outZ]     |", rule12), 
    paste("<-- [outZ]     | <-> [bidir]    |", rule13), 
    paste("<-- [formereq] | <-> [bidir]    |", rule14), 
    paste("<-- [outZ]     | --> [rightdir] |", rule15), 
    paste("<-- [formereq] | --> [rightdir] |", rule16), 
    sep = "\n")
  
  # Print table
  return(sigmaConnected)
}

#Assume we have a graph G, provided as a list of edges in a matrix of two columns. For example,
edges <- matrix(c(
  1, 2,
  2, 3,
  3, 1,
  3, 4,
  4, 5
), byrow = TRUE, ncol = 2)

#We call the library igraph:
library(igraph)

#We create the graph from the edge list:
G <- graph_from_edgelist(edges, directed = TRUE)

#We compute the strongly connected components of that graph
SCC_G <- components(G, mode = "strong")

#We need this number to write the sigma-separation rule table with q strongly connected components.
q=SCC_G$no

#Now we split the set of nodes V into the corresponding SCCs
C = split(V(G), SCC_G$membership)

#Now we write the sequence of sets required by reach()

#We need a function that uses as an input an edge list of a directed graph as a 2-column matrix
#and gives as an output the number of SCC q required to write the the "sigmaConnected" table
#and also a list of sets of SCC. 
names(C)=paste0("C_",1:q)

#We generate the rule table using our function
sigmaConnected=sigma_table_mixed_graphs2(q)

#We apply reach() to compute sigma-separation to our given edge list.  
reach(list("-->"=edges), Sets3, sigmaConnected, tableAsString=TRUE)


#Example: take the following edge list
G_edges=rbind(c(1,2),c(2,3),c(4,3),c(4,5),c(5,6),c(6,8),c(8,9),c(9,5),c(6,7),c(1,10),c(11,10),c(11,12))
G_cyclic=list("-->"=G_edges)
Sets=list("X"=c(1),"Z"=c(3,5,10,11),"C_1"=c(5,6,8,9))

sigmaConnect=sigma_table_mixed_graphs(1)

reach(G_cyclic,Sets, sigmaConnect, tableAsString=TRUE)


#Trial for automation 
G1=graph_from_edgelist(G_edges, directed = TRUE)

SCC_G1 <- components(G1, mode = "strong")

q=SCC_G1$no

C = split(V(G1), SCC_G1$membership)

names(C)=paste0("C_",1:q)

SetsL=c(list("X"=c(1),"Z"=c(3,5,10,11)),C)

sigmaConnect1=sigma_table_mixed_graphs2(q)

reach(G_cyclic,SetsL, sigmaConnect1, tableAsString = TRUE )


#Function to generate automatically directed cyclic graphs of size p+m:
generate_directed_graph_matrix <- function(p, m) {
  # All possible directed edges without self-loops
  possible_edges <- as.matrix(expand.grid(1:p, 1:p))
  possible_edges <- possible_edges[possible_edges[,1] != possible_edges[,2], ]
  
  max_edges <- nrow(possible_edges)
  
  if (m > max_edges) {
    stop("Too many edges requested")
  }
  
  # Sample m edges without replacement
  selected_edges <- possible_edges[sample(max_edges, m), , drop = FALSE]
  
  colnames(selected_edges) <- c("from", "to")
  
  return(selected_edges)
}

P=50*c(.2,1:40)
#M=as.integer(0.24*P*(P-1))
#M=as.integer(0.24*P*(sqrt(sqrt(P))))
#M[1]=21
#M[2]=588
M=as.integer(P)
S=P+M
Q=rep(0,length(S))
Time_sigma.sep=rep(0,length(S))
for (i in 1:length(S)){
  G_edges=generate_directed_graph_matrix(P[i],M[i])
  G_object=graph_from_edgelist(G_edges, directed = TRUE)
  SCC_G = components(G_object, mode = "strong")
  q=SCC_G$no
  q2=0
  C = split(V(G_object), SCC_G$membership)
  for (k in 1:q){
    if(length(C[[k]])>1){q2=q2+1}else{q2=q2+0}
  }
  Q[i]=q2
  names(C)=paste0("C_",1:q)
  V=1:P[i]
  n1=as.integer(P[i]*0.4)
  n2=as.integer(P[i]*0.2)
  X_Z=sample(V,n1,replace = FALSE)
  SetsL=c(list("X"=X_Z[1:n2],"Z"=X_Z[(n2+1):n1]),C)
  sigmaConnect1=sigma_table_mixed_graphs(q)
  G_cyclic=list("-->"=G_edges)
  time1=Sys.time()
  reach(G_cyclic,SetsL, sigmaConnect1, tableAsString = TRUE )
  time2=Sys.time()
  Time_sigma.sep[i]=time2-time1
}

#library(ggplot2)
#plot_sigma=ggplot() + 
#  geom_point(aes(x = S, y = Time_sigma.sep),color="purple") + geom_line(color = "purple")

#ggplot(aes(df, x=P+M, y=Time_sigma.sep)) +
#  geom_line() +
#  geom_point()

#df=data.frame(P+M, Time_sigma.sep)



#ggplot() + geom_point(aes(x = S, y = Q),color="purple") + geom_line(color = "purple")
ggplot() + geom_line(aes(x=S,y=Time_sigma.sep),color='red') + geom_point(aes(x=S,y=Time_sigma.sep),color='red')

ggplot() + geom_line(aes(x=S,y=Q),color='red') + geom_point(aes(x=S,y=Q),color='red')



#How the number of strongly connected components increases as the size of a graph |V|+|E| increases. 
#We write a code that generates many graphs and counts the number of strongly connected components in 
#each one. 
#P=50*c(.2,1:100)
P=rep(100,40)
#M=as.integer(0.24*P*(sqrt(sqrt(P))))
M=61:100
#=as.integer(9*P)
#M[1]=21
#M[2]=588
S=P+M
Q=rep(0,length(S))
for (i in 1:length(S)){
  G_edges=generate_directed_graph_matrix(P[i],M[i])
  G_object=graph_from_edgelist(G_edges, directed = TRUE)
  SCC_G = components(G_object, mode = "strong")
  q=SCC_G$no
  q2=0
  C = split(V(G_object), SCC_G$membership)
  for (k in 1:q){
    if(length(C[[k]])>1){q2=q2+1}else{q2=q2+0}
  }
  Q[i]=q2
}



#The following function computes the SCC of a graph G and also writes the corresponding 
#required table
SCC_and_ruletable = function(G,X,Z){
  G_directed_edges=G$direceted
  
  G1=graph_from_edgelist(G_direceted_edges, directed = TRUE)
  
  SCC_G=components(G1, mode = "strong")
  
  #We need this number for writing the rule table.
  q=SCC_G1$no
  
  #These are the SCC for the input L (introduced as "Sets")
  C = split(V(G1), SCC_G1$membership)
  
  SCC_and_table$'sigma_table'=sigma_table_mixed_graphs2(2)
  
  SCC_and_ruletable$'C'= C
  
  return(SCC_and_ruletable)
}








