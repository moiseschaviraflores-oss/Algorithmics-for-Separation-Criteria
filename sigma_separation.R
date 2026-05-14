# Title: Analysis of time-complexity for sigma-separation cifly-based algorithms. 
# Date: May 7 2026

#This file includes code to show how to compute d-separation using the function reach() of the ciflyr package.
#The following libraries can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)
library(ggplot2)


#The following function writes a rule table for sigma-connection.
#It requires the number of strongly connected components q to be specified. 
sigma_table_mixed_graphs <- function(q){
  
  #Write strongly connected components C_1,...,C_q  
  SCC <- paste0("C_", 1:q)  
  
  #Write a chain for "current in \sigma(next)"
  current_in_sigma.next <- paste(
    paste0("(current in ", SCC, " and next in ", SCC, ")"),
    collapse = " or ")  
  
  #We write the rules for table
  rule1 <- "current in Z and next not in Z"
  
  rule2 <- paste("current in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule3 <- rule1
  
  rule4 <- rule2
  
  rule5 <- "current in Z"
  
  rule6 <- rule5
  
  rule7 <- paste("current not in Z or (current in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule8 <- rule7
  
  rule9 <- paste("current not in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule10 <- "current not in Z and next not in Z"
  
  rule11 <- paste("next in Z and (",current_in_sigma.next,")",sep="")
  
  rule12 <- "next not in Z"
  
  rule13 <- "current not in Z"
  
  rule14 <- "true"
  
  rule15 <- rule13
  
  rule16 <- current_in_sigma.next
  
  # Write Rule Table
  sigmaConnected <- paste(
    "EDGES --> <--, <->", 
    paste("SETS", paste(c("X","Z", SCC), collapse = ", ")),
    "COLORS bidir, rightdir, outZ, formereq",
    "START <-- [outZ] AT X",
    "OUTPUT ... [bidir, rightdir, outZ]",
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
  
  # We ask to return table
  return(sigmaConnected)
}


#The following function is a backup of the previous one:
sigma_table_mixed_graphs2 <- function(q){
  
  #Write sets of strongly connected components \sigma(i)=C_i  
  SCC <- paste0("C_", 1:q)  
  
  #Write a chain for "current in \sigma(next)"
  current_in_sigma.next = paste(
    paste0("(current in ", SCC, " and next in ", SCC, ")"),
    collapse = " or ")  
  
  rule1 <- "current in Z and next not in Z"
  
  rule2 <- paste("current in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule3 <- rule1
  
  rule4 <- rule2
  
  rule5 <- "current in Z"
  
  rule6 <- rule5
  
  rule7 <- paste("current not in Z or (current in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule8 <- rule7
  
  rule9 <- paste("current not in Z and (next in Z and (",current_in_sigma.next,"))",sep="") 
  
  rule10 <- "current not in Z and next not in Z"
  
  rule11 <- paste("next in Z and (",current_in_sigma.next,")",sep="")
  
  rule12 <- "next not in Z"
  
  rule13 <- "current not in Z"
  
  rule14 <- "true"
  
  rule15 <- rule13
  
  rule16 <- current_in_sigma.next
  
  # Write CIfly Rule Table
  sigmaConnected <- paste(
    "EDGES --> <--, <->", 
    paste("SETS", paste(c("X","Z", SCC), collapse = ",")),
    "COLORS bidir, rightdir, outZ, formereq",
    "START <-- [outZ] AT X",
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
  
  #We ask to return rule table
  return(sigmaConnected)
}




#TOY EXAMPLE: 
#We have the following list of edges: 
G_edges <- rbind(c(1, 2), c(2, 3), c(4, 3), c(4, 5), c(5, 6), c(6, 8), 
                 c(8, 9), c(9, 5), c(6, 7), c(1, 10), c(11, 10), c(11, 12))

#We store it in a list as required by "reach"
G_cyclic <- list("-->" = G_edges)

#We introduce the sets manually
#Note that C_1 is the only strongly connected component with more than one element.
Sets <-list("X" = c(1), "Z" = c(3, 5, 10, 11), "C_1" = c(5, 6, 8, 9))

#We write the rule table
sigmaConnect <- sigma_table_mixed_graphs(1)

#And then we use reach to get the set of all nodes sigma-connected to X by Z
reach(G_cyclic, Sets, sigmaConnect, tableAsString = TRUE)


#We can automate this implementation, since it is not practical to introduce SCC manually.  
G1 <- graph_from_edgelist(G_edges, directed = TRUE)

#We use the function "components" from the library "igraph".
SCC_G1 <- components(G1, mode = "strong")

# We need to number of SCC q to generate the table
q <- SCC_G1$no

# We partition nodes in V by SCC
C <- split(V(G1), SCC_G1$membership)

#We label the SCC as C_1,...,C_q
names(C) <- paste0("C_",1:q)

#We write the list of sets required by "reach"
SetsL <- c(list("X"=c(1), "Z"=c(3, 5, 10, 11)), C)

#We write the rule table 
sigmaConnect1 <- sigma_table_mixed_graphs(q)

#We get the same result. 
reach(G_cyclic, SetsL, sigmaConnect1, tableAsString = TRUE )



#This function generates directed mixed cyclic graphs of size p + m_1 + m_2:
generate_DMG <- function(p, m1, m2) {
  
  # Maximum number of unordered node pairs
  max_pairs <- choose(p, 2)
  
  if (m1 + m2 > max_pairs) {
    stop("Too many edges requested: at most one edge per node pair.")
  }
  
  ## 1: Sample unordered node pairs
  # All possible pairs u,v
  all_pairs <- t(combn(1:p, 2))
  
  # Sample total number of required pairs
  selected_pairs <- all_pairs[
    sample(nrow(all_pairs), m1 + m2),
    ,
    drop = FALSE
  ]

  ## 2: Assign first m1 pairs as directed
  dir_pairs <- selected_pairs[1:m1, , drop = FALSE]
  
  # Randomly orient each directed edge
  directions <- sample(c(TRUE, FALSE), m1, replace = TRUE)
  
  dir_edges <- dir_pairs
  
  for (i in 1:m1) {
    if (!directions[i]) {
      dir_edges[i, ] <- rev(dir_edges[i, ])
    }
  }
  
  colnames(dir_edges) <- c("from", "to")
  
 
  ## 3: Remaining m2 pairs become bidirected
  bidir_edges <- selected_pairs[(m1 + 1):(m1 + m2), , drop = FALSE]
  
  colnames(bidir_edges) <- c("node1", "node2")
  

  ## 4: Output DMG
  DMG <- list("directed" = dir_edges, "bidirected" = bidir_edges)
  return(DMG)
}


######## Complexity Analysis: 
#To verify how the execution time increases as the size a a graph increases, we generate random DMGs
#for different numbers p. 

#SPARSE GRAPHS: The number of edges m_1 + m_2 is O(p).
P <- c(100, 1000, 2000, 4000, 6000, 8000, 10000, 12000, 14000) 

#We take the number of edges m_1 + m_2 to grow linearly with respect to the number of nodes. 
M1 <- as.integer(1.6 * P)
M2 <- as.integer(0.6 * P)

#This vector will contain the average execution time all numbers of nodes in P.
Time_sigma.sep_sparse <- rep(0, length(P))

for (i in 1:length(P)){
  #G_edges <- generate_directed_graph_matrix(P[i], M[i])
  DMG <- generate_DMG(P[i], M1[i], M2[i])
  
  #G_object <- graph_from_edgelist(G_edges, directed = TRUE)
  sub_DG <- graph_from_edgelist(DMG$directed, directed = TRUE)
  
  SCC_DMG <- components(sub_DG, mode = "strong")
  
  q <- SCC_DMG$no
  
  C <- split(V(sub_DG), SCC_DMG$membership)
  
  names(C) <- paste0("C_", 1:q)
  
  V <- 1:P[i]
  
  n1 <- as.integer(P[i] * 0.4)
  
  n2 <- as.integer(P[i] * 0.2)
  
  XUZ <- sample(V, n1, replace = FALSE)
  
  sigma_Connect <- sigma_table_mixed_graphs(q)
  sigma_Connect <- parseRuletable(sigma_Connect, tableAsString = TRUE)
  
  SetsL <- c(list("X" = XUZ[1:n2], "Z" = XUZ[(n2 + 1):n1]), C)
  SetsL <- parseSets(SetsL, sigma_Connect)
  
  #G_cyclic <- list("-->"=G_edges)
  DMG_list <- list("-->" = DMG$directed, "<->" = DMG$bidirected)
  DMG_parsed <- parseGraph(DMG_list, sigma_Connect)
  
  t <- system.time(reach(DMG_parsed, SetsL, sigma_Connect))
  cpu_time <- t["user.self"] + t["sys.self"]
  #time_sim.1[j] <- cpu_time
  
  Time_sigma.sep_sparse[i] <- cpu_time
}

#We plot the execution time vs p=|V| to visualize the increase rate
plot_sigma_sep_sparse <- ggplot() + 
  geom_line(aes(x = P, y = Time_sigma.sep_sparse), color = 'navyblue') + 
  geom_point(aes(x = P, y = Time_sigma.sep_sparse), color = 'navyblue') +
  xlab("p") + 
  ylab("Execution time") +
  ggtitle("Time Complexity of sigma-separation for sparse DGs m_1+m_2=O(p)")


#ggplot() + geom_point(aes(x = S, y = Q),color="purple") + geom_line(color = "purple")
#ggplot() + geom_line(aes(x=S,y=Time_sigma.sep),color='red') + geom_point(aes(x=S,y=Time_sigma.sep),color='red')
#ggplot() + geom_line(aes(x=S,y=Q),color='red') + geom_point(aes(x=S,y=Q),color='red')



#DENSE GRAPHS: The number of edges m_1 + m_2 is O(p(p-1)).
P <- c(100, 1000, 2000, 4000, 6000, 8000, 10000, 12000, 14000) 

#We take the number of edges m_1 + m_2 to grow linearly with respect to the number of nodes. 
M1 <- as.integer(1.6 * 0.2 * P * (P - 1))
M2 <- as.integer(0.6 * 0.2 * P * (P - 1))

#This vector will contain the average execution time all numbers of nodes in P.
Time_sigma.sep_dense <- rep(0, length(P))

for (i in 1:length(P)){
  #G_edges <- generate_directed_graph_matrix(P[i], M[i])
  DMG <- generate_DMG(P[i], M1[i], M2[i])
  
  #G_object <- graph_from_edgelist(G_edges, directed = TRUE)
  sub_DG <- graph_from_edgelist(DMG$directed, directed = TRUE)
  
  SCC_DMG <- components(sub_DG, mode = "strong")
  
  q <- SCC_DMG$no
  
  C <- split(V(sub_DG), SCC_DMG$membership)
  
  names(C) <- paste0("C_", 1:q)
  
  V <- 1:P[i]
  
  n1 <- as.integer(P[i] * 0.4)
  
  n2 <- as.integer(P[i] * 0.2)
  
  XUZ <- sample(V, n1, replace = FALSE)
  
  sigma_Connect <- sigma_table_mixed_graphs(q)
  sigma_Connect <- parseRuletable(sigma_Connect, tableAsString = TRUE)
  
  SetsL <- c(list("X" = XUZ[1:n2], "Z" = XUZ[(n2 + 1):n1]), C)
  SetsL <- parseSets(SetsL, sigma_Connect)
  
  #G_cyclic <- list("-->"=G_edges)
  DMG_list <- list("-->" = DMG$directed, "<->" = DMG$bidirected)
  DMG_parsed <- parseGraph(DMG_list, sigma_Connect)
  
  t <- system.time(reach(DMG_parsed, SetsL, sigma_Connect))
  cpu_time <- t["user.self"] + t["sys.self"]
  #time_sim.1[j] <- cpu_time
  
  Time_sigma.sep_dense[i] <- cpu_time
}

#We plot the execution time vs p=|V| to visualize the increase rate
plot_sigma_sep_dense <- ggplot() + 
  geom_line(aes(x = P, y = Time_sigma.sep_dense), color = 'navyblue') + 
  geom_point(aes(x = P, y = Time_sigma.sep_dense), color = 'navyblue') +
  xlab("p") + 
  ylab("Execution time") +
  ggtitle("Time Complexity of sigma-separation for dense DGs m_1+m_2=O(p(p-1))")

#Simple plot
plot(P,Time_sigma.sep_dense,type = "b")




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







