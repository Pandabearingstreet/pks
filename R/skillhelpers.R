generate.skillfun <- function(seed = 42,
                              sparsity = 0.1,
                              items = 6,
                              skills = 12,
                              conjunctive = FALSE,
                              disjunctive = FALSE,
                              lambda = 1) {

  ## --- Input validation ---
  if (!is.numeric(seed) || length(seed) != 1) stop("`seed` must be a single numeric.")
  if (!is.numeric(sparsity) || length(sparsity) != 1 ||
      sparsity < 0 || sparsity > 1) stop("`sparsity` must be > 0  & < 1.")
  if (!is.numeric(items) || items < 1 || floor(items) != items) stop("`items` must be an integer >= 1.")
  if (!is.numeric(skills) || skills < 1 || floor(skills) != skills) stop("`skills` must be an integer >= 1.")
  if (conjunctive && disjunctive) stop("Set at most one of `conjunctive` or `disjunctive` to TRUE.")
  if (!is.logical(conjunctive) || !is.logical(disjunctive)) stop("`conjunctive` and `disjunctive` must be logical.")

  set.seed(as.integer(seed))

  skill_names <- paste0("s", seq_len(skills))
  item_names  <- paste0("item", seq_len(items))

  ## Helper: check non-conflict (no equality, no subset/subset-of) with an existing matrix
  non_conflict_with_existing <- function(candidate, existing_mat) {
    # existing_mat: matrix with rows as logical vectors (same length as candidate) or matrix(0,ncol=skills)
    if (is.null(existing_mat) || nrow(existing_mat) == 0) return(TRUE)
    # candidate subset of existing: for a row r, all(candidate <= r) -> sum(candidate & !r) == 0
    cand_subset_existing <- rowSums(candidate & (!existing_mat)) == 0
    # existing subset of candidate: for row r, all(r <= candidate) -> sum(r & !candidate) == 0
    existing_subset_cand <- rowSums(existing_mat & (!candidate)) == 0
    !( any(cand_subset_existing) || any(existing_subset_cand) )
  }

  ## Sperner bound for one item (max size of antichain on `skills` elements)
  max_antichain <- choose(skills, floor(skills / 2))

  ## Storage: for general mode we keep a list where each element is a matrix of competencies for that item
  existing_by_item <- vector("list", length = items)
  for (i in seq_len(items)) existing_by_item[[i]] <- matrix(nrow = 0, ncol = skills)

  comp_items <- character(0)  # will parallel rows we create
  existing_rows <- matrix(nrow = 0, ncol = skills) # flattened storage for final assembly (rows in order of creation)

  ## Mode: conjunctive/disjunctive (one competency per item) ----------------
  if (conjunctive || disjunctive) {
    n_competencies <- items
    # sizes ~ Binomial(skills, sparsity)
    sizes <- rbinom(n_competencies, size = skills, prob = sparsity)
    sizes[sizes < 1] <- 1

    for (i in seq_len(items)) {
      target_size <- sizes[i]
      candidate_idx <- sample(skills, target_size)
      candidate <- rep(FALSE, skills)
      candidate[candidate_idx] <- TRUE
      existing_rows <- rbind(existing_rows, as.integer(candidate))
    }
    comp_items <- item_names
  } else {
    ## General model: each item gets multiple alternative conjunctive competencies
    # number of alternatives per item: Poisson(1) + 1 by default (small)
    max_alt_possible <- max_antichain  # per-item upper bound
    num_alt <- pmin(pmax(1, rpois(items, lambda = lambda)), max_alt_possible)

    # safety check: per-item alternatives cannot exceed Sperner bound
    if (any(num_alt > max_antichain)) {
      stop("At least one item requests more alternatives than the Sperner bound allows for the given `skills`.")
    }

    for (i in seq_len(items)) {
      for (rep_idx in seq_len(num_alt[i])) {
        attempts <- 0
        accepted <- FALSE
        while (!accepted) {
          attempts <- attempts + 1
          if (attempts > 5000) {
            break
          }
          # competency size drawn from Binomial; ensure >=1
          size_k <- rbinom(1, size = skills, prob = sparsity)
          if (size_k < 1) size_k <- 1
          candidate_idx <- sample(skills, size_k)
          candidate <- rep(FALSE, skills); candidate[candidate_idx] <- TRUE

          # critical change: check non-conflict only against competencies for the *same* item i
          if (non_conflict_with_existing(candidate, existing_by_item[[i]])) {
            existing_by_item[[i]] <- rbind(existing_by_item[[i]], candidate)
            existing_rows <- rbind(existing_rows, as.integer(candidate))
            comp_items <- c(comp_items, item_names[i])
            accepted <- TRUE
          }
        }
      }
    }
  }

  ## Build output data.frame: first column 'item', then s1..sK
  df <- as.data.frame(existing_rows, stringsAsFactors = FALSE)
  colnames(df) <- skill_names
  df <- cbind(item = comp_items, df, stringsAsFactors = FALSE)
  rownames(df) <- NULL
  if (disjunctive) {
    df <- conjunctive.as.disjunctive(df)
  }
  return(df)
}

disjunctive.as.conjunctive <- function(sf) {
  # Create an empty matrix for the conjunctive context
  unique_objects <- unique(sf$item)
  I_con <- matrix(0, nrow = length(unique_objects), ncol = ncol(sf) - 1)
  colnames(I_con) <- colnames(sf)[-1]

  # Sum the attribute values for each unique object
  for (i in seq_along(unique_objects)) {
    obj <- unique_objects[i]
    obj_rows <- sf[sf$item == obj, -1]
    I_con[i, ] <- as.integer(apply(obj_rows, 2, any))
  }
  

  I_con <- data.frame(item = unique_objects, I_con)
  return(I_con)
}

conjunctive.as.disjunctive <- function(sf) {
  if (!"item" %in% colnames(sf)) stop("Input must have a column named 'item'.")
  
  skill_cols <- setdiff(colnames(sf), "item")
  out_list <- list()
  
  for (i in seq_len(nrow(sf))) {
    item_name <- sf$item[i]
    row_skills <- sf[i, skill_cols, drop = FALSE]
    active_skills <- skill_cols[as.logical(row_skills)]
    
    for (s in active_skills) {
      new_row <- as.list(rep(0, length(skill_cols)))
      names(new_row) <- skill_cols
      new_row[[s]] <- 1
      out_list[[length(out_list) + 1]] <- c(item = item_name, new_row)
    }
  }
  
  out_df <- do.call(rbind, lapply(out_list, as.data.frame, stringsAsFactors = FALSE))
  rownames(out_df) <- NULL
  return(out_df)
}

isCon <- function(skillfun, itemID=1){
  item.names <- as.character(skillfun[, itemID])
  return(length(item.names) == length(unique(item.names)))
}


isDis <- function(skillfun, itemID=1){
  rowSums <- rowSums(skillfun[, -itemID])
  return(all(rowSums<=1))
}


K.are.same <- function(K1, K2){
  if (nrow(K1) != nrow(K2)){
    return(FALSE)
  }
  for (state in as.pattern(K1)){
    if (!(state %in% as.pattern(K2))){
      return(FALSE)
    }
  }
  return(TRUE)
}
