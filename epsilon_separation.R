#This file includes code to show how to compute epsilon-separation using the library "ciflyr"
#We use the following libraries. They can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)

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

epsilonConnected=parseRuletable(epsilonConnected,tableAsString = TRUE)

#We need a function that generates directed graphs with desired number of nodes and edges p and m respectively.  
generate_directed_graph_matrix <- function(p, m) {
  edges <- matrix(NA, nrow = m, ncol = 2)
  colnames(edges) <- c("from", "to")
  
  seen <- new.env(hash = TRUE)
  count <- 0
  
  while (count < m) {
    x <- sample(1:p, 1)
    y <- sample(1:p, 1)
    
    if (x != y) {
      key <- paste(x, y, sep = "-")
      if (!exists(key, envir = seen)) {
        count <- count + 1
        edges[count, ] <- c(x, y)
        assign(key, TRUE, envir = seen)
      }
    }
  }
  
  return(edges)
}

#G_edges=rbind(c(1,2),c(2,3),c(4,3),c(4,5),c(5,6),c(6,8),c(8,9),c(9,5),c(6,7),c(1,10),c(11,10),c(11,12))
G_edges=generate_directed_graph_matrix(12,12)
G_cyclic=list("-->"=G_edges)
G=parseGraph(G_cyclic,epsilonConnected)
Sets=list("A"=c(1),"C"=c(3,5,10,11))
Sets=parseSets(Sets,epsilonConnected)
reach(G,Sets,epsilonConnected)

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
Time_epsilon.sep1=rep(0,length(S))
for (i in 1:length(S)){
  V=1:P[i]
  n_sim=2
  time_sim=rep(0,n_sim)
  for (j in 1:n_sim){
    G_edges=generate_directed_graph_matrix(P[i],M[i]) 
    n1=as.integer(P[i]*0.4)
    n2=as.integer(P[i]*0.2)
    A_C=sample(V,n1,replace = FALSE)
    Sets=c(list("A"=A_C[1:n2],"C"=A_C[(n2+1):n1]))
    Sets=parseSets(Sets,epsilonConnected)
    G=list("-->"=G_edges)
    G=parseGraph(G,epsilonConnected)
    #t <- system.time(reach(G,Sets, deltaConnected, tableAsString = TRUE))
    #cpu_time <- t["user.self"] + t["sys.self"]
    time1=Sys.time()
    reach(G,Sets,epsilonConnected)
    time2=Sys.time()
    time_sim[j]=time2-time1
    #time_sim[j]=cpu_time
  }
  
  Time_epsilon.sep1[i]=mean(time_sim)
}

#We can plot the execution time vs p+m=|V|+|E| to visually assess the increase rate
ggplot() + geom_line(aes(x=P,y=Time_epsilon.sep1),color='red') + 
  geom_point(aes(x=P,y=Time_epsilon.sep1),color='red') +
  xlab("p+m") + ylab("Execution time") +
  ggtitle("Time Complexity of epsilon-separation for dense DAGs")



#We can also analyse time complexity for the case of sparse DAGs whose number of edges is
#m=O(p). 
P=c(10,20,40,80,160,320,640,920,1280,1800,2200,2800)

M=as.integer(2*P)
S=P+M
Time_epsilon.sep2=rep(0,length(S))
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
    #Sets=parseSets(Sets,epsilonConnected)
    G=list("-->"=G_edges)
    #G=parseGraph(G,epsilonConnected)
    #t <- system.time(reach(G,Sets, deltaConnected, tableAsString = TRUE))
    #cpu_time <- t["user.self"] + t["sys.self"]
    time1=Sys.time()
    reach(G,Sets,epsilonConnected,tableAsString =TRUE)
    time2=Sys.time()
    time_sim[j]=time2-time1
    #time_sim[j]=cpu_time    
  }
  
  Time_epsilon.sep2[i]=mean(time_sim)
}

#We can plot the execution time vs p+m=|V|+|E| to visually assess the increase rate
ggplot() + geom_line(aes(x=P,y=Time_epsilon.sep2),color='red') + 
  geom_point(aes(x=P,y=Time_epsilon.sep2),color='red') +
  xlab("p") + ylab("Execution time") +
  ggtitle("Time Complexity of epsilon-separation for sparse DAGs [m=O(p)]")


