# This function generates a directed Erdős–Rényi random graph with the following arguments:
# - p: number of nodes
# - prob: probability of edges.

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
