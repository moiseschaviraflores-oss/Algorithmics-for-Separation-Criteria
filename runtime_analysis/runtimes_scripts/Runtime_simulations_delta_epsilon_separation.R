# =================================================================
#   Runtime Complexity Analysis for delta- and epsilon-separation
# =================================================================
#
# Required libraries:
library(ciflyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(latex2exp)
library(scales)
library(bnlearn)
library(igraph)
#
# This code relies on the function 'generate_DG_ER' in the file 'Directed_graphs.R'
#
# Rule table to find nodes delta-connected to B by C:
delta_connected_table <-"
EDGES --> <--
SETS B, C
START <-- AT B
OUTPUT ...

-->  | <--  | current in C
-->  | -->  | current not in B and current not in C
<--  | -->  | current not in C
<--  | <--  | current not in B and current not in C
"
# Preprocess rule table.
delta_connected_table <- parseRuletable(delta_connected_table, tableAsString = TRUE)
#
# Rule table to find nodes epsilon-connected to B by C:
epsilon_connected_table <- "
EDGES --> <--
SETS B, C
COLORS before, after
START <-- [before] AT B
OUTPUT ... [...]

--> [before] | <-- [after]  | current in C
<-- [before] | <-- [before] | true
<-- [before] | --> [before] | current not in B or current not in C
--> [before] | --> [before] | current not in B or current not in C
--> [after]  | <-- [after]  | current in C 
<-- [after]  | <-- [after]  | current not in C
<-- [after]  | --> [after]  | current not in B or current not in C
--> [after]  | --> [after]  | current not in B or current not in C
"
# Preprocess rule table.
epsilon_connected_table <- parseRuletable(epsilon_connected_table, tableAsString = TRUE)
#
#
# --------------------------------------------
# Computation of runtime for delta-separation
# --------------------------------------------
#
# This function computes empirical run time for delta-separation for DGs:
# - P: vector of numbers of nodes, 
# - Prob: vector of probabilities of edges,
# - n_sim: number of graphs of the same size to be generated per each p in P,
# - n_rep: number of times to run reach() for the same graph to avoid zero times by rounding.
# 
runtime_delta_sep <- function(P, Prob, n_sim, n_rep){
  # This vector will contain average run times 
  r_emp_delta <- NULL
  sd_r_emp_delta <- NULL
  
  for (i in 1:length(P)){
    # Create the set of nodes V={1,...,P[i]}.  
    V <- 1:P[i] 
    
    # It sill contain average runtime for graph of size P[i]
    time_sim_delta <- numeric(n_sim)
    
    for (j in 1:n_sim){
      #Generate random DAG with P[i] nodes.
      G_edges <- generate_DG_ER(P[i], Prob[i]) #This generates a DAG
      
      #Number of nodes in the union BUC (40% of the size of V)
      n_BUC <- as.integer(P[i] * 0.4) 
      
      #Number of nodes in B (20% of the size of V) 
      n_B <- as.integer(P[i] * 0.2) 
      
      #We sample BUC without replacement because B and C must be disjoint.
      B_union_C <- sample(V, n_BUC, replace = FALSE)
      
      #We split the union BUC into B and C, and parse to pre-process. 
      Sets <- list("B" = B_union_C[1:n_B], "C" = B_union_C[(n_B+1):n_BUC])
      
      #This will be used for delta-connection
      Sets <- parseSets(Sets, delta_connected_table)
      
      #We pre-process the generated graph for the three criteria, delta-connection. 
      G <- list("-->" = G_edges)
      G <- parseGraph(G, delta_connected_table)
      
      #We run "reach" and measure execution time for d-connection 
      t_delta <- system.time(replicate(n_rep, reach(G, Sets, delta_connected_table)))
      cpu_time_delta <- t_delta["user.self"] + t_delta["sys.self"]
      time_sim_delta[j] <- cpu_time_delta/n_rep
      
    }
    # Store the average runtime for each size in P.
    # Divide by n_rep because we replicate "reach" n_rep times when measuring time. 
    r_emp_delta[i] <- mean(time_sim_delta)
    sd_r_emp_delta[i] <- sd(time_sim_delta)
  }
  r_emp <- list("r" = r_emp_delta, "sd" = sd_r_emp_delta)
  return(r_emp)
}
#
#
# Compute empirical runtime for p = 128, 256, 512, 1024, 2048, 4096, 6144
#
# A vector P with different numbers of nodes.
P <- as.integer(0.2*c(128, 256, 512, 1024, 2048, 4096, 6144))
#
# -----------------------------
# Sparse DGs with m = O(p)
# -----------------------------
#
# A vector M with different numbers of edges increasing at rate O(P).
Prob <- 1.5/(P-1)
#
# Set a seed
set.seed(42069)
#
# Use function 'runtime_d_sep' to compute empirical runtime.
r_emp_delta.sep_sparse <- runtime_delta_sep(P, Prob, n_sim = 12, n_rep = 80)
#
#
# -----------------------------
# Dense DGs with m = O(p(p-1))
# -----------------------------
#
# A vector M with different numbers of edges increasing at O(P).
Prob <- rep(0.15, length(P))
#
# We set a seed
set.seed(42069)
#
# This vector will include emprirical runtime r_emp(p) for different graph sizes p (number of nodes).
r_emp_delta.sep_dense <- runtime_delta_sep(P, Prob, n_sim = 12, n_rep = 80)
#
#
# ----------------------------------------------
# Computation of runtime for epsilon-separation
# ----------------------------------------------
#
# This function computes empirical run time for epsilon-separation for DGs:
# - P: vector of numbers of nodes, 
# - Prob: vector of probabilities of edges,
# - n_sim: number of graphs of the same size to be generated per each p in P,
# - n_rep: number of times to run reach() for the same graph to avoid zero times by rounding.
# 
runtime_epsilon_sep <- function(P, Prob, n_sim, n_rep){
  # This vector will contain average run times 
  r_emp_epsilon <- NULL
  sd_r_emp_epsilon <- NULL
  
  for (i in 1:length(P)){
    # Create the set of nodes V={1,...,P[i]}.  
    V <- 1:P[i] 
    
    # It sill contain average runtime for graph of size P[i]
    time_sim_epsilon <- numeric(n_sim)
    
    for (j in 1:n_sim){
      #Generate random DG with P[i] nodes.
      G_edges <- generate_DG_ER(P[i], Prob[i]) #This generates a DAG
      
      #Number of nodes in the union BUC (40% of the size of V)
      n_BUC <- as.integer(P[i] * 0.4) 
      
      #Number of nodes in B (20% of the size of V) 
      n_B <- as.integer(P[i] * 0.2) 
      
      #We sample BUC without replacement because B and C must be disjoint.
      B_union_C <- sample(V, n_BUC, replace = FALSE)
      
      #We split the union BUC into B and C, and parse to pre-process. 
      Sets <- list("B" = B_union_C[1:n_B], "C" = B_union_C[(n_B+1):n_BUC])
      
      #This will be used for epsilon-connection
      Sets <- parseSets(Sets, epsilon_connected_table)
      
      #We pre-process the generated graph for the three criteria, epsilon-connection. 
      G <- list("-->" = G_edges)
      G <- parseGraph(G, epsilon_connected_table)
      
      #We run "reach" and measure execution time for epsilon-connection 
      t_epsilon <- system.time(replicate(n_rep, reach(G, Sets, epsilon_connected_table)))
      cpu_time_epsilon <- t_epsilon["user.self"] + t_epsilon["sys.self"]
      time_sim_epsilon[j] <- cpu_time_epsilon/n_rep
      
    }
    # Store the average runtime for each size in P.
    # Divide by n_rep because we replicate "reach" n_rep times when measuring time. 
    r_emp_epsilon[i] <- mean(time_sim_epsilon)
    sd_r_emp_epsilon[i] <- sd(time_sim_epsilon)
  }
  r_emp <- list("r" = r_emp_epsilon, "sd" = sd_r_emp_epsilon)
  return(r_emp)
}
#
#
# Compute empirical runtime for p = 128, 256, 512, 1024, 2048, 4096, 6144
#
# A vector P with different numbers of nodes.
P <- as.integer(0.2*c(128, 256, 512, 1024, 2048, 4096, 6144))
#
# -----------------------------
# Sparse DGs with m = O(p)
# -----------------------------
#
# A vector M with different numbers of edges increasing at rate O(P).
Prob <- 1.5/(P-1)
#
# Set a seed
set.seed(42069)
#
# Use function 'runtime_epsilon_sep' to compute empirical runtime.
r_emp_epsilon.sep_sparse <- runtime_epsilon_sep(P, Prob, n_sim = 12, n_rep = 80)
#
#
# -----------------------------
# Dense DGs with m = O(p(p-1))
# -----------------------------
#
# A vector M with different numbers of edges increasing at O(P).
Prob <- rep(0.15, length(P))
#
# We set a seed
set.seed(42069)
#
# This vector will include emprirical runtime r_emp(p) for different graph sizes p (number of nodes).
r_emp_epsilon.sep_dense <- runtime_epsilon_sep(P, Prob, n_sim = 12, n_rep = 80)
#
#
# -----------------------------
# Relative projected runtime
# -----------------------------
#
# Function to implemment relative projected runtime, with the arguments:
#  - P: vector of graph sizes
#  - r: vector of empirical runtimes (equal size as P)
#  - g: order of complexity in c("linear","quadratic","cubic")
#
#
# =================================================================
#    Runtime for delta and epsilon-sep for uniformly sampled DGs
# =================================================================
#
# --------------------------------------------
# Computation of runtime for delta-separation
# --------------------------------------------
#
# This function computes empirical run time for delta-separation for DGs:
# - P: vector of numbers of nodes, 
# - Prob: vector of probabilities of edges,
# - n_sim: number of graphs of the same size to be generated per each p in P,
# - n_rep: number of times to run reach() for the same graph to avoid zero times by rounding.
# 
runtime_delta_sep_unif <- function(P, M, n_sim, n_rep){
  # This vector will contain average run times 
  r_emp_delta <- NULL
  sd_r_emp_delta <- NULL
  
  for (i in 1:length(P)){
    # Create the set of nodes V={1,...,P[i]}.  
    V <- 1:P[i] 
    
    # It sill contain average runtime for graph of size P[i]
    time_sim_delta <- numeric(n_sim)
    
    for (j in 1:n_sim){
      #Generate random DAG with P[i] nodes.
      G_edges <- generate_DG_uniform(P[i], M[i]) #This generates a DAG
      
      #Number of nodes in the union BUC (40% of the size of V)
      n_BUC <- as.integer(P[i] * 0.4) 
      
      #Number of nodes in B (20% of the size of V) 
      n_B <- as.integer(P[i] * 0.2) 
      
      #We sample BUC without replacement because B and C must be disjoint.
      B_union_C <- sample(V, n_BUC, replace = FALSE)
      
      #We split the union BUC into B and C, and parse to pre-process. 
      Sets <- list("B" = B_union_C[1:n_B], "C" = B_union_C[(n_B+1):n_BUC])
      
      #This will be used for delta-connection
      Sets <- parseSets(Sets, delta_connected_table)
      
      #We pre-process the generated graph for the three criteria, delta-connection. 
      G <- list("-->" = G_edges)
      G <- parseGraph(G, delta_connected_table)
      
      #We run "reach" and measure execution time for d-connection 
      t_delta <- system.time(replicate(n_rep, reach(G, Sets, delta_connected_table)))
      cpu_time_delta <- t_delta["user.self"] + t_delta["sys.self"]
      time_sim_delta[j] <- cpu_time_delta/n_rep
      
    }
    # Store the average runtime for each size in P.
    # Divide by n_rep because we replicate "reach" n_rep times when measuring time. 
    r_emp_delta[i] <- mean(time_sim_delta)
    sd_r_emp_delta[i] <- sd(time_sim_delta)
  }
  r_emp <- list("r" = r_emp_delta, "sd" = sd_r_emp_delta)
  return(r_emp)
}
#
#
# Compute empirical runtime for p = 128, 256, 512, 1024, 2048, 4096, 6144
#
# A vector P with different numbers of nodes.
P <- as.integer(0.2*c(128, 256, 512, 1024, 2048, 4096, 6144))
#
# -----------------------------
# Sparse DGs with m = O(p)
# -----------------------------
#
# A vector M with different numbers of edges increasing at rate O(P).
M <- 3 * P
#
# Set a seed
set.seed(42069)
#
# Use function 'runtime_d_sep' to compute empirical runtime.
r_emp_delta.sep_sparse_unif <- runtime_delta_sep_unif(P, M, n_sim = 12, n_rep = 80)
#
#
# -----------------------------
# Dense DGs with m = O(p(p-1))
# -----------------------------
#
# A vector M with different numbers of edges increasing at O(P).
M <- as.integer(0.3*P*(P - 1))
#
# We set a seed
set.seed(42069)
#
# This vector will include emprirical runtime r_emp(p) for different graph sizes p (number of nodes).
r_emp_delta.sep_dense_unif <- runtime_delta_sep_unif(P, M, n_sim = 12, n_rep = 80)
#
#
# ----------------------------------------------
# Computation of runtime for epsilon-separation
# ----------------------------------------------
#
# This function computes empirical run time for epsilon-separation for DGs:
# - P: vector of numbers of nodes, 
# - Prob: vector of probabilities of edges,
# - n_sim: number of graphs of the same size to be generated per each p in P,
# - n_rep: number of times to run reach() for the same graph to avoid zero times by rounding.
# 
runtime_epsilon_sep_unif <- function(P, M, n_sim, n_rep){
  # This vector will contain average run times 
  r_emp_epsilon <- NULL
  sd_r_emp_epsilon <- NULL
  
  for (i in 1:length(P)){
    # Create the set of nodes V={1,...,P[i]}.  
    V <- 1:P[i] 
    
    # It sill contain average runtime for graph of size P[i]
    time_sim_epsilon <- numeric(n_sim)
    
    for (j in 1:n_sim){
      #Generate random DG with P[i] nodes.
      G_edges <- generate_DG_uniform(P[i], M[i]) #This generates a DG
      
      #Number of nodes in the union BUC (40% of the size of V)
      n_BUC <- as.integer(P[i] * 0.4) 
      
      #Number of nodes in B (20% of the size of V) 
      n_B <- as.integer(P[i] * 0.2) 
      
      #We sample BUC without replacement because B and C must be disjoint.
      B_union_C <- sample(V, n_BUC, replace = FALSE)
      
      #We split the union BUC into B and C, and parse to pre-process. 
      Sets <- list("B" = B_union_C[1:n_B], "C" = B_union_C[(n_B+1):n_BUC])
      
      #This will be used for epsilon-connection
      Sets <- parseSets(Sets, epsilon_connected_table)
      
      #We pre-process the generated graph for the three criteria, epsilon-connection. 
      G <- list("-->" = G_edges)
      G <- parseGraph(G, epsilon_connected_table)
      
      #We run "reach" and measure execution time for epsilon-connection 
      t_epsilon <- system.time(replicate(n_rep, reach(G, Sets, epsilon_connected_table)))
      cpu_time_epsilon <- t_epsilon["user.self"] + t_epsilon["sys.self"]
      time_sim_epsilon[j] <- cpu_time_epsilon/n_rep
      
    }
    # Store the average runtime for each size in P.
    # Divide by n_rep because we replicate "reach" n_rep times when measuring time. 
    r_emp_epsilon[i] <- mean(time_sim_epsilon)
    sd_r_emp_epsilon[i] <- sd(time_sim_epsilon)
  }
  r_emp <- list("r" = r_emp_epsilon, "sd" = sd_r_emp_epsilon)
  return(r_emp)
}
#
# Compute empirical runtime for p = 128, 256, 512, 1024, 2048, 4096, 6144
#
# A vector P with different numbers of nodes.
P <- as.integer(0.2*c(128, 256, 512, 1024, 2048, 4096, 6144))
#
# -----------------------------
# Sparse DGs with m = O(p)
# -----------------------------
#
# A vector M with different numbers of edges increasing at rate O(P).
M <- 3 * P
#
# Set a seed
set.seed(42069)
#
# Use function 'runtime_epsilon_sep' to compute empirical runtime.
r_emp_epsilon.sep_sparse_unif <- runtime_epsilon_sep_unif(P, M, n_sim = 12, n_rep = 80)
#
#
# -----------------------------
# Dense DGs with m = O(p(p-1))
# -----------------------------
#
# A vector M with different numbers of edges increasing at O(P).
M <- as.integer(0.3*P*(P-1))
#
# We set a seed
set.seed(42069)
#
# This vector will include emprirical runtime r_emp(p) for different graph sizes p (number of nodes).
r_emp_epsilon.sep_dense_unif <- runtime_epsilon_sep_unif(P, M, n_sim = 12, n_rep = 80)
#
#
#
# ==================================================================
#   Plotting runtime complexity for delta- and epsilon-separation
# ==================================================================
#
# -----------------------------
# Relative projected runtime
# -----------------------------
#
# Function to implement relative projected runtime, with the arguments:
#  - P: vector of graph sizes
#  - r: vector of empirical runtimes (equal size as P)
#  - g: order of complexity in c("linear","quadratic","cubic")
#
Rel_Proj_Runtime <- function(P,r,g =c("linear","quadratic","cubic")){
  
  # Minimum graph size in P
  p_min <- min(P)
  
  # -----------------------------
  # Decide a complexiy
  # -----------------------------
  k <- 1
  if (g == "linear"){k=1} else
    if (g == "quadratic") {k=2} else
      if (g == "cubic") {k=3}
  
  # -----------------------------
  # Compute projected runtime over complexity O(p^k)
  # -----------------------------
  r_proj <- (P^k)*r[1]/(p_min^k)
  
  # -----------------------------
  # Compute relative projected runtime over complexity O(p^k)
  # -----------------------------
  RPR <- r_proj/r
  
  # -----------------------------
  # Output
  # -----------------------------
  return(RPR)
}
#
#
# -----------------------------
# Plot relative projected runtime
# -----------------------------
#
# Store runtimes in dataframes:
#
# Data frame for delta-sep-sparse (US graphs)
df1 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_delta.sep_sparse_unif$r,g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_delta.sep_sparse_unif$r,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_delta.sep_sparse_unif$r,g = "cubic"),
  Context = "delta-sep. - sparse - US"
)
#
## Data frame for delta-sep-dense (US graphs)
df2 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_delta.sep_dense_unif$r, g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_delta.sep_dense_unif$r,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_delta.sep_dense_unif$r,g = "cubic"),
  Context = "delta-sep. - dense - US"
)
#
## Data frame for delta-sep-sparse (ER graphs)
df3 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_delta.sep_sparse$r, g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_delta.sep_sparse$r,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_delta.sep_sparse$r,g = "cubic"),
  Context = "delta-sep. - sparse - ER"
)
#
## Data frame for delta-sep-dense (ER graphs)
df4 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_delta.sep_dense$r, g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_delta.sep_dense$r,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_delta.sep_dense$r,g = "cubic"),
  Context = "delta-sep. - dense - ER"
)
#
## Data frame for epsilon-sep-sparse (US graphs)
df5 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_sparse_unif$r, g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_sparse_unif$r,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_sparse_unif$r,g = "cubic"),
  Context = "epsilon-sep. - sparse - US"
)
#
## Data frame for epsilon-sep-dense (US graphs)
df6 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_dense_unif$r, g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_dense_unif$r,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_dense_unif$r,g = "cubic"),
  Context = "epsilon-sep. - dense - US"
)
#
## Data frame for epsilon-sep-sparse (ER graphs)
df7 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_sparse$r, g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_sparse$r,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_sparse$r,g = "cubic"),
  Context = "epsilon-sep. - sparse - ER"
)

## Data frame for epsilon-sep-dense (ER graphs)
df8 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_dense$r, g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_dense$r,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_epsilon.sep_dense$r,g = "cubic"),
  Context = "epsilon-sep. - dense - ER"
)
#
#
df_sep <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8)
#
df_sep$Context <- factor(
  df_sep$Context,
  levels = c(
    "delta-sep. - sparse - US",
    "delta-sep. - dense - US",
    "delta-sep. - sparse - ER",
    "delta-sep. - dense - ER",
    "epsilon-sep. - sparse - US",
    "epsilon-sep. - dense - US",
    "epsilon-sep. - sparse - ER",
    "epsilon-sep. - dense - ER"
  )
)
#
#
df_long_sep <- pivot_longer(
  df_sep,
  cols = c(Y1, Y2, Y3),
  names_to = "Complexity",
  values_to = "Y"
)
#
#
ggplot(df_long_sep,
       aes(x = X,
           y = Y,
           color = Complexity)) +
  
  geom_line() +
  geom_point() +
  
  facet_wrap(
    ~ Context,
    nrow = 2,
    ncol = 4
  ) +
  scale_color_manual(
    
    values = c(
      Y1 = "#3D8080",
      Y2 = "#277D3E",
      Y3 = "#EFC21F"
    ),
    
    labels = c(
      Y1 = TeX("$O(p)$", italic = TRUE),
      Y2 = TeX("$O(p^2)$", italic = TRUE),
      Y3 = TeX("$O(p^3)$", italic = TRUE)
    )
    
  ) +
  
  scale_y_continuous(trans = "log10", labels = label_log()) +
  
  xlab(TeX("#nodes \\textit{$p$}")) + 
  ylab(TeX("Relative projected runtime")) +
  
  theme_gray() +
  
  theme(
    legend.position = "top"
  )
