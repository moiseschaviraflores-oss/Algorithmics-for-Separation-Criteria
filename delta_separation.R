#This file includes code to show how to compute delta-separation using the library "ciflyr"
#We use the following libraries. They can be downloaded from the CRAN Package Repository:
library(ciflyr)
library(here)
library(igraph)
library(latex2exp)

#delta-connection
deltaConnected <- "
EDGES --> <--
SETS A, C
START <-- AT A
OUTPUT --> 

--> | <-- | current in C
--> | --> | current not in C
<-- | --> | current not in C
<-- | <-- | current not in C
"
deltaConnected=parseRuletable(deltaConnected,tableAsString = TRUE)

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



