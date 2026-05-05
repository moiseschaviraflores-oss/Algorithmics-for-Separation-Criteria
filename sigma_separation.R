#This file includes code to show how to compute sigma-separation using the library "ciflyr"
#We use the following libraries. They can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)

#The next function assumes that the sets of strongly connected components are already given, that is,
#we have a partition for the set of nodes V={1,...,p}=C_1\cup ...\cup C_q, according to the definition
#of strongly connected components. 
#Function to generate rule table for sigma-connection in mixed graphs:
sigma_table_mixed_graphs=function(q){
  
  #Write strongly connected components C_1,...,C_q  
  SCC = paste0("C_", 1:q)  
  
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
    "OUTPUT ... [bidir,rightdir,outZ]",
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
generate_DMG <- function(p, m1, m2) {
  
  ### ----- Directed edges (x -> y) -----
  
  # All possible directed edges (no self-loops)
  possible_dir <- as.matrix(expand.grid(1:p, 1:p))
  possible_dir <- possible_dir[possible_dir[,1] != possible_dir[,2], ]
  
  max_dir <- nrow(possible_dir)
  if (m1 > max_dir) {
    stop("Too many directed edges requested")
  }
  
  # Sample directed edges
  dir_edges <- possible_dir[sample(max_dir, m1), , drop = FALSE]
  colnames(dir_edges) <- c("from", "to")
  
  
  ### ----- Bidirected edges (u <-> v) -----
  
  # All unordered pairs
  possible_bidir <- t(combn(1:p, 2))
  
  max_bidir <- nrow(possible_bidir)
  if (m2 > max_bidir) {
    stop("Too many bidirected edges requested")
  }
  
  # Sample bidirected edges
  bidir_edges <- possible_bidir[sample(max_bidir, m2), , drop = FALSE]
  colnames(bidir_edges) <- c("node1", "node2")
  
  
  ### ----- Output -----
  
  return(list(
    directed = dir_edges,
    bidirected = bidir_edges
  ))
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
