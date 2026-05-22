###################################################################################################################
############Time-Complexity Analysis for sigma-separation   ###################

# The following function produces directed mixed graphs (DMGs) for given p and m and q.
# The argument p is the number of nodes, m is the number of edges, and q is the number of strongly connected components.  
# It returns two lists of edges in a 2-column matrix.
# This function generates DMGs for a given size p + m_1 + m_2 with a given number q of SCC.

generate_DMG_with_qSCC <- function(p, m1, m2, q) {
  # Check that the number of SCC is not lager than the number of nodes.
  if (q > p) {
    stop("The numnber of SCC q cannot exceed the number of nodes p.")
  }
  
  #Check that the number of requested edges does not overpass the number of possible edges.
  max_edges <- p * (p - 1) /2
  
  if (m1 + m2 > max_edges) {
    stop("Too many edges requested.")
  }
  
  ## 1: Partition nodes in V into q SCCs
  
  # Set of nodes V = {1,... ,p}
  nodes <- sample(1:p)
  
  # Random partition sizes
  sizes <- rep(1, q)
  
  remaining <- p - q
  
  if (remaining > 0) {
    extra <- sample(1:q, remaining, replace = TRUE)
    for (i in extra) {
      sizes[i] <- sizes[i] + 1
    }
  }
  
  SCCs <- list()
  
  idx <- 1
  
  for (i in 1:q) {
    SCCs[[i]] <- nodes[idx:(idx + sizes[i] - 1)]
    idx <- idx + sizes[i]
  }
  
  
  ## 2: Create directed edges ensuring SCCs
  
  
  dir_edges <- matrix(ncol = 2, nrow = 0)
  
  used_pairs <- list()
  
  pair_key <- function(a, b) {
    paste(sort(c(a, b)), collapse = "-")
  }
  
  ## Create one directed cycle inside each SCC
  for (C in SCCs) {
    
    if (length(C) >= 2) {
      
      for (i in 1:(length(C)-1)) {
        dir_edges <- rbind(dir_edges, c(C[i], C[i+1]))
        used_pairs[[pair_key(C[i], C[i+1])]] <- TRUE
      }
      
      dir_edges <- rbind(dir_edges, c(C[length(C)], C[1]))
      used_pairs[[pair_key(C[length(C)], C[1])]] <- TRUE
    }
  }
  
  ## 3: Include additional directed edges keeping SCCs
  
  current_m1 <- nrow(dir_edges)
  
  possible_dir <- matrix(ncol = 2, nrow = 0)
  
  for (i in 1:q) {
    
    Ci <- SCCs[[i]]
    
    ## Edges within SCCs
    for (u in Ci) {
      for (v in Ci) {
        if (u != v) {
          
          key <- pair_key(u, v)
          
          if (is.null(used_pairs[[key]])) {
            possible_dir <- rbind(possible_dir, c(u, v))
          }
        }
      }
    }
    
    ## Edges between SCCs: only forward direction
    if (i < q) {
      
      for (j in (i+1):q) {
        
        Cj <- SCCs[[j]]
        
        for (u in Ci) {
          for (v in Cj) {
            
            key <- pair_key(u, v)
            
            if (is.null(used_pairs[[key]])) {
              possible_dir <- rbind(possible_dir, c(u, v))
            }
          }
        }
      }
    }
  }
  
  needed <- m1 - current_m1
  
  if (needed < 0) {
    stop("m1 too small to realize q SCCs.")
  }
  
  if (needed > nrow(possible_dir)) {
    stop("Not enough possible directed edges.")
  }
  
  if (needed > 0) {
    
    extra_idx <- sample(nrow(possible_dir), needed)
    
    extra_dir <- possible_dir[extra_idx, , drop = FALSE]
    
    dir_edges <- rbind(dir_edges, extra_dir)
    
    for (k in 1:nrow(extra_dir)) {
      used_pairs[[pair_key(extra_dir[k,1], extra_dir[k,2])]] <- TRUE
    }
  }
  
  colnames(dir_edges) <- c("from", "to")
  
  # 4: Add bidirected edges
  
  all_pairs <- t(combn(1:p, 2))
  
  keep <- apply(all_pairs, 1, function(pair) {
    is.null(used_pairs[[pair_key(pair[1], pair[2])]])
  })
  
  possible_bidir <- all_pairs[keep, , drop = FALSE]
  
  if (m2 > nrow(possible_bidir)) {
    stop("Not enough remaining pairs for bidirected edges.")
  }
  
  bidir_edges <- possible_bidir[
    sample(nrow(possible_bidir), m2),
    ,
    drop = FALSE
  ]
  
  colnames(bidir_edges) <- c("node1", "node2")
  
  
  ## 5 Output: Return directed edges, bidirected edges, and SCC partition.
  DMG <- list("directed" = dir_edges, "bidirected" = bidir_edges, "SCCs" = SCCs)
  
  return(DMG)
}




## Another function:
## This function does not care about the number of SCCs
## The following function produces a DMG with cycles for given p, m1, and m2 
generate_DMG <- function(p, m1, m2) {
  
  if (p < 2) stop("Need at least 2 nodes.")
  
  max_pairs <- choose(p, 2)
  
  if (m1 + m2 > max_pairs) {
    stop("Too many edges: at most one edge per unordered pair.")
  }
  
  # All unordered node pairs
  pairs <- t(combn(1:p, 2))
  
  # Randomly select pairs for all edges (directed + bidirected)
  chosen <- pairs[sample(nrow(pairs), m1 + m2), , drop = FALSE]
  
  # Split allocation
  dir_pairs <- chosen[1:m1, , drop = FALSE]
  bidi_pairs <- chosen[(m1 + 1):(m1 + m2), , drop = FALSE]
  
  # Random orientation for directed edges
  dir_edges <- dir_pairs
  
  if (m1 > 0) {
    flip <- sample(c(TRUE, FALSE), m1, replace = TRUE)
    for (i in 1:m1) {
      if (flip[i]) {
        dir_edges[i, ] <- rev(dir_edges[i, ])
      }
    }
  }
  
  colnames(dir_edges) <- c("from", "to")
  
  # Bidirected edges
  colnames(bidi_pairs) <- c("node1", "node2")
  
  # Output graph
  Graph <- list("directed" = dir_edges, "bidirected" = bidi_pairs)
  
  return(Graph)
}



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
  sigma_connected_table <- paste(
    "EDGES --> <--, <->", 
    paste("SETS", paste(c("X","Z", SCC), collapse = ", ")),
    "COLORS bidir, rightdir, outZ, formereq",
    "START <-- [outZ] AT X",
    "OUTPUT ... [...]",
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
  return(sigma_connected_table)
}


#
# FUNCTION FOR EMPRICAL RUNTIME: 
# This function computes empirical runtime for sigma-separation for DMG with given vectors P and M, and a number q:
runtime_sigma_sep <- function(P, M1, M2, n_sim, n_rep){
  # This vector will contain average run times 
  r_emp_sigma <- NULL
  
  for (i in 1:length(P)){
    # Create the set of nodes V={1,...,P[i]}.  
    V <- 1:P[i] 
    
    # It sill contain average runtime for graph of size P[i]
    time_sim_sigma <- NULL
    
    for (j in 1:n_sim){
      #Generate random DGM with P[i] nodes, M[i] and q or q[i] 
      DMG <- generate_DMG(P[i], M1[i], M2[i]) 
      
      sub_DG <- graph_from_edgelist(DMG$directed, directed = TRUE)
      
      SCC_DMG <- components(sub_DG, mode = "strong")
      
      q <- SCC_DMG$no
      
      C <- split(V(sub_DG), SCC_DMG$membership)
      
      names(C) <- paste0("C_", 1:q)
      
      #Number of nodes in the union XUZ (40% of the size of V)
      n_XUZ <- as.integer(P[i] * 0.4) 
      
      #Number of nodes in X (20% of the size of V) 
      n_X <- as.integer(P[i] * 0.2) 
      
      #We sample XUZ without replacement because X and Z must be disjoint.
      XUZ <- sample(V, n_XUZ, replace = FALSE)
      
      sigma_Connect <- sigma_table_mixed_graphs(q)
      
      sigma_Connect <- parseRuletable(sigma_Connect, tableAsString = TRUE)
      
      #We split the union AUB into A and B, and parse to pre-process. 
      Sets_sigma <- c(list("X" = XUZ[1:n_X], "Z" = XUZ[(n_X + 1):n_XUZ]), C)
      
      Sets_sigma <- parseSets(Sets_sigma, sigma_Connect)
      
      #We pre-process the generated graph for the three criteria, d, *, and t connection. 
      G <- list("-->" = DMG$directed, "<->" = DMG$bidirected)
      G_sigma <- parseGraph(G, sigma_Connect)
      
      #We run "reach" and measure execution time for sigma-connection 
      t_sigma <- system.time(replicate(n_rep, reach(G_sigma, Sets_sigma, sigma_Connect)))
      cpu_time_sigma <- t_sigma["user.self"] + t_sigma["sys.self"]
      time_sim_sigma[j] <- cpu_time_sigma
      
    }
    # Store the average runtime for each size in P.
    # Divide by n_rep because we replicate "reach" n_rep times when measuring time. 
    r_emp_sigma[i] <- mean(time_sim_sigma)/n_rep
  }
  return(r_emp_sigma)
}


## TIME COMPLEXITY ANALYSIS for *-SEPARATION IN SPARSE DAGs 
# Consider DAGs where m = O(p).

# A vector P with different numbers of nodes
P <- c(128, 512, 1024, 2048, 4096, 6144)

# A vector M with different numbers of edges increasing at O(P).
M1 <- as.integer(1.3 * P)
M2 <- as.integer(0.1 * P)

# We set a seed
set.seed(1777)

# This vector will include emprirical runtime r_emp(p) for different graph sizes p (number of nodes).
r_emp_sigma.sep_sparse <- runtime_sigma_sep(P, M1, M2, n_sim = 12, n_rep = 3)

ggplot() + 
  geom_line(aes(x = P, y = r_emp_sigma.sep_sparse), color = 'navyblue') + 
  geom_point(aes(x = P, y = r_emp_sigma.sep_sparse), color = 'navyblue') +
  xlab("p") + 
  ylab("Execution time") +
  ggtitle(TeX("Time Complexity of $\\sigma$-separation for sparse DMGs"))

# Computation of relative projected runtime for different orders of polynomial run time.
# We assess O(p), O(p^2), O(p^3) time-complexities. 

# Projected run time for linear, square and cubic complexities, with p.ast = min(P).
p.ast <- min(P)

# Linear time O(p)
r_proj_lin_sigma.sep_sparse <- P * r_emp_sigma.sep_sparse[1] / p.ast

# Squared time O(p^2)
r_proj_sqr_sigma.sep_sparse <- (P ^ 2) * r_emp_sigma.sep_sparse[1] / (p.ast ^ 2)

# Cubic time O(p^3)
r_proj_cubic_sigma.sep_sparse <- (P ^ 3) * r_emp_sigma.sep_sparse[1] / (p.ast ^ 3)

# Cubic time O(p^2*log(p))
r_proj_2_log_sigma.sep_sparse <- (P ^ 2.5) * r_emp_sigma.sep_sparse[1] / (p.ast ^ 2.5)


# Now we compute relative projected time for O(p), O(p^2) and O(p^3)
# Linear time O(p)
RPR_lin_sigma.sep_sparse <- r_proj_lin_sigma.sep_sparse / r_emp_sigma.sep_sparse

# Squared time O(p^2)
RPR_sqr_sigma.sep_sparse <- r_proj_sqr_sigma.sep_sparse / r_emp_sigma.sep_sparse

# Cubic time O(p^3)
RPR_cubic_sigma.sep_sparse <- r_proj_cubic_sigma.sep_sparse / r_emp_sigma.sep_sparse

RPR_2_log_sigma.sep_sparse <- r_proj_2_log_sigma.sep_sparse / r_emp_sigma.sep_sparse

# Plot relative projected time for O(p), O(p^2) and O(p^3)
ggplot() +
  geom_line(aes(x = P, y = RPR_lin_sigma.sep_sparse, color = "linear")) +
  geom_point(aes(x = P, y = RPR_lin_sigma.sep_sparse, color = "linear")) +
  geom_line(aes(x = P, y = RPR_sqr_sigma.sep_sparse, color = "square")) +
  geom_point(aes(x = P, y = RPR_sqr_sigma.sep_sparse, color = "square")) +
  geom_line(aes(x = P, y = RPR_cubic_sigma.sep_sparse, color = "cubic")) +
  geom_point(aes(x = P, y = RPR_cubic_sigma.sep_sparse, color = "cubic")) +
  geom_line(aes(x = P, y = RPR_2_log_sigma.sep_sparse, color = "Op2.5")) +
  geom_point(aes(x = P, y = RPR_2_log_sigma.sep_sparse, color = "Op2.5")) +
  scale_color_manual(
    values = c(
      linear = "navyblue",
      square = "darkgreen",
      cubic = "orange",
      Op2.5 = "red"
    ),
    name = "Order"
  ) +
  scale_y_continuous(trans = "log10", labels = label_log()) +
  xlab("p") + 
  ylab("Relative Projected Time") +
  ggtitle(TeX("$\\sigma$-separation in sparse DMGs"))




## TIME COMPLEXITY ANALYSIS for sigma-SEPARATION IN DENSE DMGs 
# Consider DMGs where m = O(p(p-1)).

# A vector P with different numbers of nodes
P <- c(128, 512, 1024, 2048, 4096, 6144)

# Vectors M1 and M2 with different numbers of edges increasing at O(p(p-1)).
M1 <- as.integer(0.12 * P * (P - 1))
M2 <- as.integer(0.03 * P * (P - 1))

# We set a seed
set.seed(1777)

# This vector will include emprirical runtime r_emp(p) for different graph sizes p (number of nodes).
r_emp_sigma.sep_dense <- runtime_sigma_sep(P, M1, M2, n_sim = 10, n_rep = 10)

ggplot() + 
  geom_line(aes(x = P, y = r_emp_sigma.sep_dense), color = 'navyblue') + 
  geom_point(aes(x = P, y = r_emp_sigma.sep_dense), color = 'navyblue') +
  xlab("p") + 
  ylab("Execution time") +
  ggtitle(TeX("Time Complexity of $\\sigma$-separation for dense DMGs"))


# Computation of relative projected runtime for different orders of polynomial run time.
# We assess O(p), O(p^2), O(p^3) time-complexities. 

# Projected run time for linear, square and cubic complexities, with p.ast = min(P).
p.ast <- min(P)

# Linear time O(p)
r_proj_lin_sigma.sep_dense <- P * r_emp_sigma.sep_dense[1] / p.ast

# Squared time O(p^2)
r_proj_sqr_sigma.sep_dense <- (P ^ 2) * r_emp_sigma.sep_dense[1] / (p.ast ^ 2)

# Cubic time O(p^3)
r_proj_cubic_sigma.sep_dense <- (P ^ 3) * r_emp_sigma.sep_dense[1] / (p.ast ^ 3)

# Now we compute relative projected time for O(p), O(p^2) and O(p^3)
# Linear time O(p)
RPR_lin_sigma.sep_dense <- r_proj_lin_sigma.sep_dense / r_emp_sigma.sep_dense

# Squared time O(p^2)
RPR_sqr_sigma.sep_dense <- r_proj_sqr_sigma.sep_dense / r_emp_sigma.sep_dense

# Cubic time O(p^3)
RPR_cubic_sigma.sep_dense <- r_proj_cubic_sigma.sep_dense / r_emp_sigma.sep_dense

# Plot relative projected time for O(p), O(p^2) and O(p^3)
ggplot() +
  geom_line(aes(x = P, y = RPR_lin_sigma.sep_dense, color = "linear")) +
  geom_point(aes(x = P, y = RPR_lin_sigma.sep_dense, color = "linear")) +
  geom_line(aes(x = P, y = RPR_sqr_sigma.sep_dense, color = "square")) +
  geom_point(aes(x = P, y = RPR_sqr_sigma.sep_dense, color = "square")) +
  geom_line(aes(x = P, y = RPR_cubic_sigma.sep_dense, color = "cubic")) +
  geom_point(aes(x = P, y = RPR_cubic_sigma.sep_dense, color = "cubic")) +
  scale_color_manual(
    values = c(
      linear = "navyblue",
      square = "darkgreen",
      cubic = "orange"
    ),
    name = "Curve"
  ) +
  scale_y_continuous(trans = "log10", labels = label_log()) +
  xlab("p") + 
  ylab("Relative Projected Time") +
  ggtitle(TeX("$\\sigma$-separation in dense DMGs"))


ggplot() +
  geom_line(aes(x = P, y = RPR_lin_sigma.sep_dense, color = "linear")) +
  geom_point(aes(x = P, y = RPR_lin_sigma.sep_dense, color = "linear")) +
  geom_line(aes(x = P, y = RPR_sqr_sigma.sep_dense, color = "square")) +
  geom_point(aes(x = P, y = RPR_sqr_sigma.sep_dense, color = "square")) +
  geom_line(aes(x = P, y = RPR_cubic_sigma.sep_dense, color = "cubic")) +
  geom_point(aes(x = P, y = RPR_cubic_sigma.sep_dense, color = "cubic")) +
  scale_color_manual(
    values = c(
      linear = "navyblue",
      square = "darkgreen",
      cubic = "orange"
    ),
    name = "Complexity"
  ) +
  scale_y_continuous(trans = "log10", labels = label_log()) +
  xlab("p") + 
  ylab("Relative Projected Time") +
  ggtitle(TeX("$\\sigma$-separation in dense DMGs"))






