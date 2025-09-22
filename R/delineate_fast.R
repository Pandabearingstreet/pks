delineate_conjunctive_base <- function(skillfun, itemID = 1, was_dis=FALSE, item_idx=NULL, item.names=NULL, give_full_matrix_result=TRUE) {
  if (is.null(item.names)) {
    item.names <- as.character(skillfun[, itemID])
  }
  mu <- as.matrix(skillfun[, -itemID])
  nskills <- ncol(mu)
  skill.names <- colnames(mu)
  I <- !mu

  concepts <- compute_concepts(I, was_dis, item_idx, give_full_matrix_result)

  if (give_full_matrix_result) {  
    states <- +(concepts$Extents)
    colnames(states) <- item.names
    rownames(states) <- concepts$StateNames
    return(states)
  } else {
    return(list(statenames=unlist(concepts$StateNames), items=item.names))
  }

}

delineate_disjunctive_wrapper <- function(skillfun, itemID = 1, give_full_matrix_result=TRUE) {
  skillfun <- make_dis_to_con(skillfun)
  output <- delineate_conjunctive_base(skillfun, itemID, was_dis = TRUE, give_full_matrix_result = give_full_matrix_result)
  return(output)
}

delineate.fast <- function(skillfun, itemID = 1, give_full_matrix_result=TRUE) {
  # check for case 1
  if (isCon(skillfun, itemID)) {
    output <- delineate_conjunctive_base(skillfun, itemID, give_full_matrix_result = give_full_matrix_result)
  } else if (isDis(skillfun, itemID)) { #check for case 2
    output <- delineate_disjunctive_wrapper(skillfun, itemID, give_full_matrix_result = give_full_matrix_result)
  } else {
    # else, must be case 3:
    item.names <- as.character(unique(skillfun[, itemID]))
    rename_list <- rename_duplicates(skillfun, itemID)
    new_sf <- rename_list$sf
    # also save the idx. this way we can directly collapse duplicate item collumns in the C++ code.
    item_idx <- rename_list$item_idx

    output <- delineate_conjunctive_base(new_sf, itemID, item_idx=item_idx, item.names = item.names, give_full_matrix_result = give_full_matrix_result)
    
    # remove duplicate states
    if(give_full_matrix_result) {
      output <- output[!duplicated(rownames(output)), ]
    } else {
      output$statenames <- unique(output$statenames)
    }
  }
  return(output)
}
