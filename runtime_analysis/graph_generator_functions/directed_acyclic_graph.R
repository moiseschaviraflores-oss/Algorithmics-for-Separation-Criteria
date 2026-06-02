# This function generates directed acyclic graphs (DAGs) using the following arguments:
# - p: number of nodes
# - m: number of edges

generate_DAG_matrix <- function(p, m) {
  
  # -----------------------------
  # 0. Input checks
  # -----------------------------
  
  max_edges <- p * (p - 1) / 2
  if (m > max_edges) {
    stop("Too many edges requested")
  }
  
  # -----------------------------
  # 1. Create a random topological order for nodes
  # -----------------------------

  nodes <- sample(1:p, p, replace = FALSE)
  
  # -----------------------------
  # 2. A matrix considering all possible edges
  # -----------------------------
  
  possible_edges <- matrix(NA, nrow = p * (p - 1) / 2, ncol = 2)
  k <- 1

  # -----------------------------
  # 3. Store edges respecting the topological order
  # -----------------------------  
  
  for (i in 1:(p - 1)) {
    for (j in (i + 1):p) {
      possible_edges[k, ] <- c(nodes[i], nodes[j])
      k <- k + 1
    }
  }
  
  possible_edges <- possible_edges[1:(k - 1), , drop = FALSE]
  
  # -----------------------------
  # 4. Sample exactly m edges
  # -----------------------------   
  
  sampled_id <- sample(1:max_edges, m, replace = FALSE)
  DAG_edges <- possible_edges[sampled_id, , drop = FALSE]

  # -----------------------------   
  # 5. Output: list of sampled edges in a m*2 matrix. 
  # -----------------------------  
  
  return(DAG_edges)
}