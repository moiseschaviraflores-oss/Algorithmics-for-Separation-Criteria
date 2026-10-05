# =========================================================
#   Functions to generate directed acyclic graphs (DAGs)
# =========================================================
#   
# =============================================
#   Generate DAGs via Uniform Sampling:
# =============================================
#
# This function generates directed acyclic graphs (DAGs) using the following arguments:
# - p: number of nodes
# - m: number of edges
#
generate_DAG_matrix <- function(p, m) {
  
  # -----------------
  # 0. Input checks
  # -----------------
  
  max_edges <- p * (p - 1) / 2
  if (m > max_edges) {
    stop("Too many edges requested")
  }
  
  # -----------------------------------------------
  # 1. Create a random topological order for nodes
  # -----------------------------------------------

  nodes <- sample(1:p, p, replace = FALSE)
  
  # -------------------------------------------
  # 2. A matrix considering all possible edges
  # -------------------------------------------
  
  possible_edges <- matrix(NA, nrow = p * (p - 1) / 2, ncol = 2)
  k <- 1

  # ------------------------------------------------
  # 3. Store edges respecting the topological order
  # ------------------------------------------------
  
  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      possible_edges[k, ] <- c(nodes[i], nodes[j])
      k <- k + 1
    }
  }
  
  possible_edges <- possible_edges[1:(k - 1), , drop = FALSE]
  
  # ---------------------------
  # 4. Sample exactly m edges
  # ---------------------------  
  
  sampled_id <- sample(1:max_edges, m, replace = FALSE)
  DAG_edges <- possible_edges[sampled_id, , drop = FALSE]

  # --------------------------------------------------   
  # 5. Output: list of sampled edges in a m*2 matrix 
  # --------------------------------------------------  
  
  return(DAG_edges)
}
#
# ==========================================
#   Generate DAGs using Erdos-Renyi model
# ==========================================
# 
# The following function has the following arguments: 
# - p = number of nodes
# - prob = probability of including each possible edge
#
# Output: A 2-column matrix containing the directed edges.
#
# This function generates a random topological ordering of the nodes.
# It samples edges independently with probability 'prob', but only in 
# the direction allowed by that ordering. Therefore, the resulting graph is 
# guaranteed to be a DAG:
#
generate_ER_DAG_matrix <- function(p, prob) {
  # -----------------
  # 0. Input checks
  # -----------------
  
  if (length(p) != 1 || p < 1 || p != as.integer(p)) {
    stop("'p' must be a positive integer")
  }
  
  if (length(prob) != 1 || prob < 0 || prob > 1) {
    stop("'prob' must be between 0 and 1")
  }
  
  p <- as.integer(p)
  
  # -----------------------------
  # 1. Random topological order
  # -----------------------------
  
  nodes <- sample(1:p, p, replace = FALSE)
  
  # ----------------------------------------------------
  # 2. Generate all possible edges respecting the order
  # ----------------------------------------------------
  
  possible_edges <- matrix(
    NA_integer_,
    nrow = p * (p - 1) / 2,
    ncol = 2
  )
  
  k <- 1
  
  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      possible_edges[k, ] <- c(nodes[i], nodes[j])
      k <- k + 1
    }
  }
  
  # -----------------------------------------------------
  # 3. Sample each edge independently with P(edge) = prob
  # -----------------------------------------------------
  
  include_edge <- runif(nrow(possible_edges)) < prob
  
  DAG_edges <- possible_edges[include_edge, , drop = FALSE]
  
  # If no edges were sampled, return a 0 x 2 matrix
  if (nrow(DAG_edges) == 0) {
    DAG_edges <- matrix(integer(0), ncol = 2)
  }
  
  # -----------------------------
  # 4. Return edge list
  # -----------------------------
  
  return(DAG_edges)
}
#
# ================================================
#   Generate DAGs using the Barbasi-Albert model
# ================================================
#
# This function generates a DAG using the Barbasi-Albert (scale-free) model.
# It has the following arguments:
# - p: number of nodes
# - m_edges : number of edges added in each iteration.
#
# It relies on the function "sample_pa()" in the "igraph" R package (available at CRAN repository).
#
library(igraph)
#
generate_BA_DAG <- function(p, m_edges) {
  
  # --------------------------------------------------
  # 1. Check that p and m_edges are positive integers  
  # --------------------------------------------------
  
  if (p < 1 || p != as.integer(p))
    stop("'p' must be a positive integer")
  
  if (m_edges < 1 || m_edges != as.integer(m_edges))
    stop("'m_edges' must be a positive integer")
  
  # -------------------------------------------------------------------------------
  # 2. Check that the number of attached edges for a new node is not larger than p  
  # -------------------------------------------------------------------------------
  
  if (m_edges >= p)
    stop("'m_edges' must be smaller than 'p'")
  
  # -------------------------------------
  # 3. Generate DAG using "sample_pa()"
  # -------------------------------------
  
  G <- igraph::sample_pa(
    n = p,
    m = m_edges,
    directed = TRUE
  )
  
  # --------------------
  # 4. Return edge list
  # --------------------
  
  return(igraph::as_edgelist(G))
}

}
