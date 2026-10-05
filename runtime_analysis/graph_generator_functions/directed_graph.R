# ===========================================================
#   Functions to generate directed graphs (DGs) with loops
# ===========================================================
#
# =======================================
#  Generate DGs from Erdős–Rényi model:
# =======================================
#
# This function generates a directed Erdős–Rényi random graph with the following arguments:
# - p: number of nodes
# - prob: probability of edges.
#
generate_DG_ER <- function(p, prob) {
  
  # -----------------------------
  # 0. Input checks
  # -----------------------------
  
  if (p <= 0 || p != as.integer(p)) {
    stop("p must be a positive integer")
  }
  
  if (prob < 0 || prob > 1) {
    stop("prob must be between 0 and 1")
  }
  
  # -----------------------------
  # 1. Generate all possible directed edges
  # -----------------------------
  #
  # expand.grid creates all ordered pairs (i, j)
  # corresponding to possible edges i -> j
  
  all_edges <- expand.grid(
    from = 1:p,
    to   = 1:p
  )
  
  # Remove self-loops (i -> i)
  all_edges <- all_edges[all_edges$from != all_edges$to, ]
  
  # -----------------------------
  # 3. It keeps each edge independently with probability "prob"
  # -----------------------------
  
  keep <- rbinom(
    n = nrow(all_edges),
    size = 1,
    prob = prob
  ) == 1
  
  edge_list <- as.matrix(all_edges[keep, ])

  # -----------------------------  
  # 4. Store edges in a matrix (even if no edges are selected)
  # -----------------------------
  
  if (nrow(edge_list) == 0) {
    edge_list <- matrix(
      numeric(0),
      ncol = 2,
      dimnames = list(NULL, c("from", "to"))
    )
  }
  
  # -----------------------------
  # 5. Output
  # -----------------------------
  
  return(edge_list)
}
#
#
# ========================================
#   Generate DGs using Uniform Sampling 
# ========================================
#
# Arguments:
# p = number of nodes
# m = exact number of directed edges
#
# Output: An m x 2 matrix containing the directed edges.
#
# Note that cycles and loops are allowed.
#
generate_DG_uniform <- function(p, m) {
  
  # -----------------------------
  # 0. Input checks
  # -----------------------------
  
  if (length(p) != 1 || p < 1 || p != as.integer(p)) {
    stop("'p' must be a positive integer")
  }
  
  if (length(m) != 1 || m < 0 || m != as.integer(m)) {
    stop("'m' must be a non-negative integer")
  }
  
  p <- as.integer(p)
  m <- as.integer(m)
  
  max_edges <- p * (p - 1)
  
  if (m > max_edges) {
    stop("Too many edges requested: a simple directed graph with ",
         p, " nodes can have at most ", max_edges, " edges")
  }
  
  # ----------------------------------------
  # 1. Generate all possible directed edges
  # ----------------------------------------
  
  possible_edges <- matrix(
    NA_integer_,
    nrow = max_edges,
    ncol = 2
  )
  
  k <- 1
  
  for (i in 1:p) {
    for (j in 1:p) {
      
      # No self-loops
      if (i != j) {
        possible_edges[k, ] <- c(i, j)
        k <- k + 1
      }
    }
  }
  
  # --------------------------------------------------------
  # 2. Sample exactly m edges uniformly without replacement
  # --------------------------------------------------------
  
  if (m == 0) {
    DG_edges <- matrix(integer(0), ncol = 2)
  } else {
    sampled_id <- sample(max_edges, m, replace = FALSE)
    DG_edges <- possible_edges[sampled_id, , drop = FALSE]
  }
  
  # -----------------------------
  # 3. Return edge list
  # -----------------------------
  
  return(DG_edges)
}
