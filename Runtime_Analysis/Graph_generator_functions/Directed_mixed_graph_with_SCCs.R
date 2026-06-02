# This function generates a directed mixed graph with the requested arguments:
# - p: number of nodes,
# - m1: number of directed edges,
# - m2: number of bidirected edges,
# - q: number of strongly connected components. 

generate_DMG_and_SCC <- function(p, m1, m2, q) {
  
  # -----------------------------
  # 0. Input checks
  # -----------------------------
  
  if (q > p) {
    stop("q cannot exceed p")
  }
  
  if (m1 < p) {
    stop("Need at least p directed edges to create one cycle per SCC")
  }
  
  # -----------------------------
  # 1. Create q SCCs of approximately equal size
  # -----------------------------

  nodes <- sample.int(p)
  
  base_size <- floor(p / q)
  remainder <- p %% q
  
  sizes <- rep(base_size, q)
  if (remainder > 0) {
    sizes[1:remainder] <- sizes[1:remainder] + 1
  }
  
  SCCs <- vector("list", q)
  
  start <- 1
  for (i in seq_len(q)) {
    end <- start + sizes[i] - 1
    SCCs[[i]] <- nodes[start:end]
    start <- end + 1
  }
  
  names(SCCs) <- paste0("C_", seq_len(q))
  
  # -----------------------------
  # 2. Create one directed cycle inside each SCC
  # -----------------------------
  
  directed <- matrix(integer(0), ncol = 2)
  
  for (Ci in SCCs) {
    
    k <- length(Ci)
    
    if (k == 1) {
      next
    }
    
    cycle_edges <- cbind(
      Ci,
      c(Ci[-1], Ci[1])
    )
    
    directed <- rbind(directed, cycle_edges)
  }
  
  colnames(directed) <- c("from", "to")
  
  # -----------------------------
  # 3. Add remaining directed edges without merging SCCs
  # -----------------------------
  
  remaining <- m1 - nrow(directed)
  
  if (remaining > 0) {
    
    SCC_order <- sample.int(q)
    
    possible_dir <- list()
    
    idx <- 1
    
    for (a in seq_len(q - 1)) {
      for (b in (a + 1):q) {
        
        A <- SCCs[[SCC_order[a]]]
        B <- SCCs[[SCC_order[b]]]
        
        tmp <- expand.grid(A, B)
        
        possible_dir[[idx]] <- as.matrix(tmp)
        idx <- idx + 1
      }
    }
    
    if (length(possible_dir) > 0) {
      
      possible_dir <- do.call(rbind, possible_dir)
      
      used <- paste(directed[,1], directed[,2], sep = "_")
      
      keep <- !(paste(possible_dir[,1],
                      possible_dir[,2],
                      sep = "_") %in% used)
      
      possible_dir <- possible_dir[keep, , drop = FALSE]
      
      if (remaining > nrow(possible_dir)) {
        stop("Cannot add requested number of directed edges")
      }
      
      selected <- sample.int(nrow(possible_dir), remaining)
      
      directed <- rbind(
        directed,
        possible_dir[selected, , drop = FALSE]
      )
    }
  }
  
  # -----------------------------
  # 4. Add bidirected edges
  # -----------------------------
  
  all_pairs <- t(combn(seq_len(p), 2))
  
  occupied <- matrix(FALSE, p, p)
  
  if (nrow(directed) > 0) {
    
    for (i in seq_len(nrow(directed))) {
      
      a <- directed[i,1]
      b <- directed[i,2]
      
      occupied[a,b] <- TRUE
      occupied[b,a] <- TRUE
    }
  }
  
  available <- apply(all_pairs, 1, function(x) {
    !occupied[x[1], x[2]]
  })
  
  possible_bidir <- all_pairs[available, , drop = FALSE]
  
  if (m2 > nrow(possible_bidir)) {
    stop("Too many bidirected edges requested")
  }
  
  if (m2 > 0) {
    
    chosen <- sample.int(nrow(possible_bidir), m2)
    
    bidirected <- possible_bidir[chosen, , drop = FALSE]
    
  } else {
    
    bidirected <- matrix(integer(0), ncol = 2)
  }
  
  colnames(bidirected) <- c("node1", "node2")
  
  # -----------------------------
  # 5. Output graph:
  # -----------------------------
  
  Graph <- list(
    "directed"   = directed,
    "bidirected" = bidirected,
    "SCCs"       = SCCs
  )
  
  return(Graph)
}