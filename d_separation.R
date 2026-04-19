#This file includes code to show how to compute *-separation using the library "ciflyr"
#We use the following libraries. They can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)

#We will use the function reach(), which is part of the library "ciflyr", which requires
#1) A "graph" argument as a list of egde list matrices. 
#2) A "sets" argument as a finite sequence of sets 
#3) A "ruletable" argument. It can be a txt file, or a written string. 
#4) A logical argument "tableAsString", which must be TRUE if the "tablerule" argument is a string. 

#Consider the DAG with adjacency matrix: 
Adj <- matrix(c(0,1,0,0,0,0,0,
                0,0,1,0,0,0,0,
                0,0,0,0,0,0,0,
                0,1,0,0,1,0,0,
                0,0,0,0,0,1,0,
                0,0,0,0,0,0,0,
                0,0,0,0,1,0,0), nrow=7, byrow=TRUE)

# create the network object
DAG <- graph_from_adjacency_matrix(Adj)

# plot it
plot(DAG,
     vertex.color = "#6699cc", # Node color
     vertex.size = 22, # Node size
     vertex.label.size = 14 , # Label size
     vertex.label.color = "black", # Label color
     edge.color = "black", # Edge color
     edge.width = 0.5, # Edge width
     edge.arrow.size = 0.35,
     edge.size = 2.5
)


#The following string is the required rule table for finding d-connected nodes to a set A by a set C:
dConnected <- "
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT ... 

--> | <-- | current in C
--> | --> | current not in C
<-- | --> | current not in C
<-- | <-- | current not in C
"

#The DAG in the picture is stored in the format required by reach() as follows:
G <- list("-->" = rbind(c(1,2),c(2,3), c(4,2),c(4,5),c(5,6),c(7,5)))

#We will be interested in the set B of all nodes that are d-connected to A={1} by the set C={3,6}
Sets=list("A" = c(1), "C" = c(3, 6))

#Now the function reach() is used to detect all nodes d-connected to the node 1 by the set {3,6}:
reach(G, Sets, dConnected, tableAsString=TRUE)

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

#Dense graphs: The number of edges m is O(p(p-1)).
P=50*c(.2,1:40)
M=as.integer(0.24*P*(P-1))
#M=as.integer(0.24*P*(sqrt(sqrt(P))))
#M[1]=21
#M[2]=588
#M=as.integer(P)
S=P+M
Time_d.sep=rep(0,length(S))
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
    G=list("-->"=G_edges)
    time1=Sys.time()
    reach(G,Sets, StarConnected, tableAsString = TRUE)
    time2=Sys.time()
    time_sim[j]=time2-time1
  }
  
  Time_d.sep[i]=mean(time_sim)
}

#We plot the execution time vs p+m=|V|+|E| to visualise the increase rate
ggplot() + geom_line(aes(x=S,y=Time_d.sep),color='red') + 
  geom_point(aes(x=S,y=Time_d.sep),color='red') +
  xlab("p+m") + ylab("Execution time") +
  ggtitle("Time Complexity of d-separation for dense DAGs")

#Sparse graphs: The number of edges m is O(p).
P=50*c(.2,1:40)
#M=as.integer(0.24*P*(P-1))
#M=as.integer(0.24*P*(sqrt(sqrt(P))))
#M[1]=21
#M[2]=588
M=as.integer(P)
S=P+M
Time_d.sep=rep(0,length(S))
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
    G=list("-->"=G_edges)
    time1=Sys.time()
    reach(G,Sets, StarConnected, tableAsString = TRUE)
    time2=Sys.time()
    time_sim[j]=time2-time1
  }
  
  Time_d.sep[i]=mean(time_sim)
}

#We can plot the execution time vs p+m=|V|+|E| to visually assess the increase rate
ggplot() + geom_line(aes(x=S,y=Time_d.sep),color='red') + 
  geom_point(aes(x=S,y=Time_d.sep),color='red') +
  xlab("p+m") + ylab("Execution time") +
  ggtitle("Time Complexity of d-separation for sparse DAGs")








