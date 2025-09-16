#' Generate a Random Skill Function
#' 
#' Creates a random skill function with specified properties for testing and
#' simulation purposes.
#' 
#' The skill function indicates which skills are required to solve each item.
#' This function can generate conjunctive, disjunctive, or mixed skill functions
#' with controlled sparsity.
#' 
#' @param seed random seed for reproducibility
#' @param sparsity probability of a skill being required for an item (between 0 and 1)
#' @param uniqueItems number of unique item labels to generate
#' @param items number of items in the skill function
#' @param skills number of skills in the skill function
#' @param conjunctive logical; if TRUE, ensures each item label appears only once
#' @param disjunctive logical; if TRUE, ensures each item requires at most one skill
#' @return A data frame representing the skill function with columns for item labels
#'   and binary indicators for each skill
#' @export generate.skillfun
generate.skillfun <- function(seed = 42, 
                              sparsity = 0.1, 
                              uniqueItems = 7, 
                              items = 6, 
                              skills = 12, 
                              conjunctive = FALSE, 
                              disjunctive = FALSE) {
  set.seed(seed)

  # Validate uniqueness constraint
  if (conjunctive && items > uniqueItems) {
    stop("Cannot generate unique item names: 'items' > 'uniqueItems'")
  }

  # Generate item label pool as "item_1", "item_2", ...
  label_pool <- paste0("item_", 1:uniqueItems)

  # Sample item labels
  item_labels <- if (conjunctive) {
    sample(label_pool, items, replace = FALSE)
  } else {
    sample(label_pool, items, replace = TRUE)
  }

  # Initialize skill matrix
  skill_matrix <- matrix(0, nrow = items, ncol = skills)

  if (disjunctive) {
    # At most one skill per item
    for (i in 1:items) {
      if (runif(1) < sparsity) {
        skill_matrix[i, sample(1:skills, 1)] <- 1
      }
    }
  } else {
    # Multiple skills per item allowed
    skill_matrix <- matrix(rbinom(items * skills, 1, sparsity), nrow = items)
  }

  colnames(skill_matrix) <- paste0("s", 1:skills)
  df <- data.frame(item = item_labels, skill_matrix, row.names = NULL)
  
  # Ensure no empty rows by adding a random skill to any row with all zeros
  empty_rows <- which(rowSums(df[sapply(df, is.numeric)]) == 0)
  for (i in empty_rows) {
    random_skill <- sample(1:skills, 1)
    df[i, random_skill + 1] <- 1  # +1 because first column is item
  }
  
  # Ensure no row for the same item is a subset of another
  for (item in unique(df$item)) {
    item_rows <- which(df$item == item)
    if (length(item_rows) > 1) {
      skill_cols <- which(sapply(df, is.numeric))
      
      # Check each pair of rows
      for (i in 1:(length(item_rows)-1)) {
        for (j in (i+1):length(item_rows)) {
          row_i <- as.logical(as.numeric(df[item_rows[i], skill_cols]))
          row_j <- as.logical(as.numeric(df[item_rows[j], skill_cols]))
          
          # Check if row_i is subset of row_j
          if (all(row_i & row_j == row_i)) {
            # Add a random skill to row_i that's not in row_j
            potential_skills <- which(!row_j & (1:skills == 1:skills))
            if (length(potential_skills) > 0) {
              new_skill <- sample(potential_skills, 1)
              df[item_rows[i], skill_cols[new_skill]] <- 1
            }
          }
          # Check if row_j is subset of row_i
          else if (all(row_j & row_i == row_j)) {
            # Add a random skill to row_j that's not in row_i
            potential_skills <- which(!row_i & (1:skills == 1:skills))
            if (length(potential_skills) > 0) {
              new_skill <- sample(potential_skills, 1)
              df[item_rows[j], skill_cols[new_skill]] <- 1
            }
          }
          # Check if rows are identical
          else if (all(row_i == row_j)) {
            # Add a random skill to row_j
            potential_skills <- which(!row_j & (1:skills == 1:skills))
            if (length(potential_skills) > 0) {
              new_skill <- sample(potential_skills, 1)
              df[item_rows[j], skill_cols[new_skill]] <- 1
            }
          }
        }
      }
    }
  }
  
  return(df)
}

##################################################
# Helper functions for delineation by skill function type

#' Convert a disjunctive skill function to a conjunctive one
#' 
#' @param sf a data frame representing a disjunctive skill function
#' @return a data frame representing the equivalent conjunctive skill function
make_dis_to_con <- function(sf) {
  # Create an empty matrix for the conjunctive context
  unique_objects <- unique(sf$item)
  I_con <- matrix(0, nrow = length(unique_objects), ncol = ncol(sf) - 1)
  colnames(I_con) <- colnames(sf)[-1]
  #rownames(I_con) <- unique_objects
  
  # Sum the attribute values for each unique object
  for (i in seq_along(unique_objects)) {
    obj <- unique_objects[i]
    obj_rows <- sf[sf$item == obj, -1]
    I_con[i, ] <- colSums(obj_rows)
  }
  
  # Convert back to data frame
  I_con <- data.frame(item = unique_objects, I_con)
  return(I_con)
}

#' Convert a conjunctive skill function to a disjunctive one
#' 
#' @param sf a data frame representing a conjunctive skill function
#' @return a data frame representing the equivalent disjunctive skill function
make_con_to_dis <- function(sf) {
  new_objects <- c()
  I_dis <- matrix()
  
  for (i in seq_len(nrow(sf))) {
    row <- sf[i, -1]  # Exclude the item column
    object <- sf$item[i]
    
    # Find indices where the value is 1
    indices <- which(row == 1)
    
    if (length(indices) == 1) {
      I_dis <- rbind(I_dis, row)
      new_objects <- c(new_objects, object)
    } else {
      for (index in indices) {
        new_row <- rep(0, ncol(row))
        new_row[index] <- 1
        I_dis <- rbind(I_dis, new_row)
        new_objects <- c(new_objects, object)
      }
    }
  }
  
  I_dis <- data.frame(item = new_objects, I_dis)
  colnames(I_dis) <- colnames(sf)
  return(I_dis)
}

#' Check if a skill function is conjunctive
#' 
#' @param skillfun a data frame representing a skill function
#' @param itemID index of the column in \code{skillfun} that holds the item indicator
#' @return logical indicating whether the skill function is conjunctive
isCon <- function(skillfun, itemID){
  item.names <- as.character(skillfun[, itemID])
  return(length(item.names) == length(unique(item.names)))
}

#' Check if a skill function is disjunctive
#' 
#' @param skillfun a data frame representing a skill function
#' @param itemID index of the column in \code{skillfun} that holds the item indicator
#' @return logical indicating whether the skill function is disjunctive
isDis <- function(skillfun, itemID){
  rowSums <- rowSums(skillfun[, -itemID])
  return(all(rowSums<=1))
}

#' Compare two knowledge structures for equality
#' 
#' Checks if two knowledge structures contain the same states.
#' 
#' @param K1 first knowledge structure
#' @param K2 second knowledge structure
#' @return logical indicating whether the knowledge structures are equal
#' @export K.are.same
K.are.same <- function(K1, K2){
  for (state in rownames(K1)){
    if (!(state %in% rownames(K2))){
      return(FALSE)
    }
  }
  for (state in rownames(K2)){
    if (!(state %in% rownames(K1))){
      return(FALSE)
    }
  } 
  return(TRUE)
}

#' Additional helper functions
#' Rename duplicate items in a skill function
#' 
#' Adds a numeric prefix to duplicate items in a skill function
#' 
#' @param skillfun a data frame representing a skill function
#' @param itemID index of the column in \code{skillfun} that holds the item indicator
#' @return a data frame representing the skill function with renamed duplicate items
rename_duplicates <- function(skillfun, itemID = 1) {
  items <- skillfun[, itemID]
  counts <- list()
  
  for (i in seq_along(items)) {
    item <- items[i]
    if (item %in% names(counts)) {
      counts[[item]] <- counts[[item]] + 1
      skillfun[i, itemID] <- paste0(counts[[item]], "_", item)
    } else {
      counts[[item]] <- 1
      skillfun[i, itemID] <- paste0("1_", item)
    }
  }
  return(skillfun)
}

#' Collapse states with items with numeric prefixes
#' 
#' Combines items that differ only in numeric prefix into single items, and then reasseses the states
#' 
#' @param bloated_states a matrix representing the knowledge structure with numeric prefixes in the item names
#' @return a matrix representing the knowledge structure with collapsed states
collapse_states <- function(bloated_states) {
  # Remove numeric prefixes followed by underscore (e.g., 1_item_3 → item_3)
  original_names <- gsub("^\\d+_", "", colnames(bloated_states))
  # Collapse columns by group
  collapsed <- do.call(cbind, lapply(unique(original_names), function(name) {
    cols <- which(original_names == name)
    if (length(cols) == 1) {
      return(bloated_states[, cols, drop = FALSE])
    } else {
      return(as.data.frame(as.integer(rowSums(bloated_states[, cols]) > 0)))
    }
  }))

  colnames(collapsed) <- unique(original_names)
  
  # Remove duplicate rows
  collapsed_unique <- unique(collapsed)
  
  # Order rows by rowSums (ascending)
  collapsed_ordered <- collapsed_unique[order(rowSums(collapsed_unique)), ]
  
  # Don't set rownames here - will be done later
  return(collapsed_ordered)
}
