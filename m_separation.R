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



