# -----------------------------------------------------
# Title: Runtime Complexity Analysis for t-separation:
# -----------------------------------------------------
#
# Required libraries:
library(ciflyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(latex2exp)
library(scales)
#
# This code relies on the function 'generate_DAG_matrix' in the file 'Directed_acyclic_graph.R'
#
# Rule table to find nodes t-connected to A by (C_A,C_B) 
t_connected_table = "
EDGES --> <--
SETS A, C_A, C_B
START <-- AT A
OUTPUT ...

<--  | <--  | current not in C_A
-->  | -->  | current not in C_B
<--  | -->  | current not in C_A and current not in C_B
"
# Preprocess rule table.
t_connected_table <- parseRuletable(t_connected_table,tableAsString = TRUE)
#
#
# This function computes empirical run time for t-separation for DAGs:
# - P: vector of numbers of nodes, 
# - M: vector of numbers  of edges,
# - n_sim: number of graphs of the same size to be generated per each p in P,
# - n_rep: number of times to run reach() for the same graph to avoid zero times by rounding.
# 
runtime_t_sep <- function(P, M, n_sim, n_rep){
  # This vector will contain average run times 
  r_emp_t <- NULL
  
  for (i in 1:length(P)){
    # Create the set of nodes V={1,...,P[i]}.  
    V <- 1:P[i] 
    
    # It sill contain average runtime for graph of size P[i]
    time_sim_t <- numeric(n_sim)
    
    for (j in 1:n_sim){
      #Generate random DAG with P[i] nodes.
      G_edges <- generate_DAG_matrix(P[i], M[i]) #This generates a DAG
      
      n.A <- as.integer(P[i]*0.2)
      
      n.C_A <- as.integer(P[i]*0.1)
      
      n.C_B <- n.C_A
      
      A <- sample(V,n.A,replace = FALSE)
      
      C_A <- sample(V,n.C_A,replace = FALSE)
      
      C_B <-sample(V,n.C_B,replace = FALSE)
      
      #This will be used for t-connection
      Sets <- list("A"=A,"C_A"=C_A,"C_B"=C_B)
      Sets <- parseSets(Sets, t_connected_table)
      
      #We pre-process the generated graph for the three criteria, t connection. 
      G <- list("-->" = G_edges)
      G <- parseGraph(G, t_connected_table)
      
      #We run "reach" and measure execution time for t-connection 
      t_t <- system.time(replicate(n_rep, reach(G, Sets, t_connected_table)))
      cpu_time_t <- t_t["user.self"] + t_t["sys.self"]
      time_sim_t[j] <- cpu_time_t
      
    }
    # Store the average runtime for each size in P.
    # Divide by n_rep because we replicate "reach" n_rep times when measuring time. 
    r_emp_t[i] <- mean(time_sim_t)/n_rep
  }
  return(r_emp_t)
}
#
#
# Compute empirical runtime for p = 128, 256, 512, 1024, 2048, 4096, 6144
#
# A vector P with different numbers of nodes.
#P <- c(128, 256, 512, 1024, 2048, 4096, 6144)
P<-c(256,512,1024,1536)
#
# -----------------------------
# Sparse DAGs with m = O(p)
# -----------------------------
#
# A vector M with different numbers of edges increasing at rate O(P).
M <- as.integer(1.5 * P)
#
# Set a seed
set.seed(1777)
#
# Use function 'runtime_t_sep' to compute empirical runtime.
r_emp_t.sep_sparse <- runtime_t_sep(P, M, n_sim = 12, n_rep = 80)
#
#
# -----------------------------
# Dense DAGs with m = O(p(p-1))
# -----------------------------
#
# A vector M with different numbers of edges increasing at O(P).
M <- as.integer(0.15 * P * (P - 1))
#
# We set a seed
set.seed(1777)
#
# This vector will include emprirical runtime r_emp(p) for different graph sizes p (number of nodes).
r_emp_t.sep_dense <- runtime_t_sep(P, M, n_sim = 12, n_rep = 80)
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
# Data frame for t-sep-sparse
df1 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_t.sep_sparse,g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_t.sep_sparse,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_t.sep_sparse,g = "cubic"),
  Context = "t-sep. - sparse"
)
#
# Data frame for t-sep-dense
df2 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_t.sep_dense,g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_t.sep_dense,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_t.sep_dense,g = "cubic"),
  Context = "t-sep. - dense"
)
#
# Combined dataframe
df <- bind_rows(df1, df2)

df$Context <- factor(
  df$Context,
  levels = c(
    "t-sep. - sparse",
    "t-sep. - dense"
  )
)


df_long <- pivot_longer(
  df,
  cols = c(Y1, Y2, Y3),
  names_to = "Complexity",
  values_to = "Y"
)


ggplot(df_long,
       aes(x = X,
           y = Y,
           color = Complexity)) +
  
  geom_line() +
  geom_point() +
  
  facet_wrap(
    ~ Context,
    nrow = 1,
    ncol = 2
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
  ) +
  
  labs(
    title = "Relative projected time for t-separation:",
    color = NULL
  )



