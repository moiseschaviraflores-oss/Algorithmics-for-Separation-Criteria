# =====================================================
#   Runtime Complexity Analysis for sigma-separation
# =====================================================
#
# Required libraries:
library(ciflyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(latex2exp)
library(scales)
#
# This code relies on the function 'generate_DMG_and_SCC' in the file 'Directed_mixed_graph_with_SCCs.R'
#
# Function to generate a rule table to find nodes *-connected to X by Z.
# It requests the argument:
# - q: number of nontrivial strongly connected components of the input graph G
#  
sigma_table_mixed_graphs <- function(q){
  
  # ------------------------------------------------
  # Write strongly connected components C_1,...,C_q 
  # ------------------------------------------------
  SCC <- paste0("C_", 1:q)  
  
  # ---------------------------------------
  # Write SETS including X, Z and the SCCs
  # ---------------------------------------
  SETS <- paste("SETS", paste(c("X","Z", SCC), collapse = ", ")) 
  if(q == 0) {SETS <- "SETS X, Z"}
  
  # --------------------------------------------
  # Write a chain for "current in \sigma(next)"
  # --------------------------------------------
  current_in_sigma.next <- paste(
    paste0("(current in ", SCC, " and next in ", SCC, ")"),
    collapse = " or ")  
  if(q == 0){current_in_sigma.next <- "false"}
  
  # -----------------------------
  # We write the rules for table
  # -----------------------------
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
  
  # --------------------------------
  # Write rule table for sigma-sep.
  # --------------------------------
  sigma_connected_table <- paste(
    "EDGES --> <--, <->", 
    SETS,
    "COLORS plain, formereq",
    "START <-- [plain] AT X",
    "OUTPUT ... [...]",
    "",
    paste("--> [plain]    | <-- [plain]    |", rule1),
    paste("--> [plain]    | <-- [formereq] |", rule2),
    paste("<-> [plain]    | <-- [plain]    |", rule3),
    paste("<-> [plain]    | <-- [formereq] |", rule4), 
    paste("--> [plain]    | <-> [plain]    |", rule5), 
    paste("<-> [plain]    | <-> [plain]    |", rule6), 
    paste("--> [plain]    | --> [plain]    |", rule7), 
    paste("<-> [plain]    | --> [plain]    |", rule8), 
    paste("<-- [plain]    | <-- [formereq] |", rule9), 
    paste("<-- [plain]    | <-- [plain]    |", rule10), 
    paste("<-- [formereq] | <-- [formereq] |", rule11), 
    paste("<-- [formereq] | <-- [plain]    |", rule12), 
    paste("<-- [plain]    | <-> [plain]    |", rule13), 
    paste("<-- [formereq] | <-> [plain]    |", rule14), 
    paste("<-- [plain]    | --> [plain]    |", rule15), 
    paste("<-- [formereq] | --> [plain]    |", rule16), 
    sep = "\n")
  
  # We ask to return table
  return(sigma_connected_table)
}
#
#
# This function computes empirical run time for sigma-separation for DMGs:
# - P: vector of numbers of nodes, 
# - M1: vector of numbers of directed edges,
# - M2: vector of numbers of bidirected edges,
# - Q: number of equal size strongly connected components
# - n_sim: number of graphs of the same size to be generated per each p in P,
# - n_rep: number of times to run reach() for the same graph to avoid zero times by rounding.
# 
runtime_sigma_sep_q.fix <- function(P, M1, M2, Q, n_sim, n_rep){
  
  r_emp_sigma <- numeric(length(P))
  
  sd_r_emp_sigma <- numeric(length(P))
  
  for (i in 1:length(P)){
    #Set of nodes
    V <- 1:P[i]
    
    n_XUZ <- as.integer(0.4 * P[i])
    
    n_X <- as.integer(0.2 * P[i])
    
    time_sigma_pre <- numeric(n_sim)
    
    for (j in 1:n_sim){
      DMG <-generate_DMG_and_SCC(P[i], M1[i], M2[i], Q[i])
      
      C <- DMG$SCCs
      
      names(C) <- paste0("C_", 1:Q[i])
      
      sigma_rule_table <- sigma_table_mixed_graphs(Q[i])
      
      sigma_rule_table <- parseRuletable(sigma_rule_table, tableAsString = TRUE)
      
      #We sample XUZ without replacement because X and Z must be disjoint.
      XUZ <- sample(V, n_XUZ, replace = FALSE)
      
      sets <- c(list("X" = XUZ[1:n_X], "Z" = XUZ[(n_X + 1):n_XUZ]), C)
      sets <- parseSets(sets, sigma_rule_table)
      
      G <- list("-->" = DMG$directed, "<->"=DMG$bidirected)
      G <- parseGraph(G, sigma_rule_table)
      
      time_sigma <- system.time(replicate(n_rep,reach(G,sets,sigma_rule_table)))
      
      time_sigma_pre[j] <- (time_sigma["user.self"] + time_sigma["sys.self"])/n_rep
    }
    r_emp_sigma[i] <- mean(time_sigma_pre)
    sd_r_emp_sigma[i] <- sd(time_sigma_pre)
  }
  r_emp <- list("r" = r_emp_sigma, "sd" = sd_r_emp_sigma)
  return(r_emp)
}
#
#
# Compute empirical runtime for p = 128, 256, 512, 1024, 2048, 4096
#
# A vector P with different numbers of nodes.
P <- c(128, 256, 512, 1024, 2048, 4096, 6144)
Q <- as.integer(0.03*P)
#
# -----------------------------
# Sparse DMGs with m = O(p)
# -----------------------------
#
# A vector M with different numbers of edges increasing at rate O(P).
M1_sparse <- as.integer(1.5*P)
M2_sparse <- as.integer(0.15*P)
#
# Set a seed
set.seed(1777)
#
# Use function 'runtime_d_sep' to compute empirical runtime.
r_emp_sigma.sep_sparse <- runtime_sigma_sep_q.fix(P, M1_sparse, M2_sparse, Q, n_sim = 12, n_rep = 80)
#
#
# -----------------------------
# Dense DMGs with m = O(p(p-1))
# -----------------------------
#
# A vector M with different numbers of edges increasing at O(P).
M1_dense <- as.integer(0.15*P*(P-1))
M2_dense <- as.integer(0.03*P*(P-1))
#
# We set a seed
set.seed(1777)
#
# This vector will include emprirical runtime r_emp(p) for different graph sizes p (number of nodes).
r_emp_sigma.sep_dense <- runtime_sigma_sep_q.fix(P, M1_dense, M2_dense, Q, n_sim = 12, n_rep = 10)
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
# -----------------------------
# Plot relative projected runtime
# -----------------------------
#
# Store runtimes in dataframes:
#
# Data frame for sigma-sep-sparse
df1 <- data.frame( 
  X = P,              
  Y1 = Rel_Proj_Runtime(P,r_emp_sigma.sep_sparse$r,g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_sigma.sep_sparse$r,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_sigma.sep_sparse$r,g = "cubic"),
  Context = "sigma-sep. - sparse"
)
#
# Data frame for sigma-sep-dense
df2 <- data.frame(
  X = P,
  Y1 = Rel_Proj_Runtime(P,r_emp_sigma.sep_dense$r,g = "linear"),
  Y2 = Rel_Proj_Runtime(P,r_emp_sigma.sep_dense$r,g = "quadratic"),
  Y3 = Rel_Proj_Runtime(P,r_emp_sigma.sep_dense$r,g = "cubic"),
  Context = "sigma-sep. - dense"
)
#
# Combined dataframe
df <- bind_rows(df1, df2)

df$Context <- factor(
  df$Context,
  levels = c(
    "sigma-sep. - sparse",
    "sigma-sep. - dense"
  )
)
#
#
df_long <- pivot_longer(
  df,
  cols = c(Y1, Y2, Y3),
  names_to = "Complexity",
  values_to = "Y"
)
#
#
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
    title = "Relative projected time for sigma-separation:",
    color = NULL
  )
#
#
