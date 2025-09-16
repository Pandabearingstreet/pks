t_delineate_conjunctive_skillfun_only <- function(skillfun, itemID = 1, verbose = FALSE) {
  item.names <- as.character(skillfun[, itemID])
  mu <- as.matrix(skillfun[, -itemID])
  nskills <- ncol(mu)
  skill.names <- colnames(mu)
  I <- mu
  
  if (verbose) {
    cat("Calculating states with Inclose2 in C++:\n")
  }
  if (verbose) {
    print(system.time(concepts <- compute_concepts(I)))
  } else {
    concepts <- compute_concepts(I)
  }
  
  states <- +(!concepts$Extents)
  colnames(states) <- item.names
  rownames(states) <- concepts$ConceptNames
  return(states)
}

t_delineate_disjunctive_skillfun_only <- function(skillfun, itemID = 1, verbose = FALSE) {
  skillfun <- make_dis_to_con(skillfun)
  states <- t_delineate_conjunctive_skillfun_only(skillfun, itemID, verbose = verbose)
  states <- +(states)
  return(states)
}

t_delineate.by.cheating <- function(skillfun, itemID = 1, verbose = FALSE) {
  if (isCon(skillfun, itemID)) {
    states <- t_delineate_conjunctive_skillfun_only(skillfun, itemID, verbose = verbose)
    return(states)
  }
  
  if (isDis(skillfun, itemID)) {
    states <- t_delineate_disjunctive_skillfun_only(skillfun, itemID, verbose = verbose)
    rownames(states) <- concat_rows(states)
    return(states)
  }
  
  new_sf <- rename_duplicates(skillfun, itemID)
  bloated_states <- t_delineate_conjunctive_skillfun_only(new_sf, itemID, verbose = verbose)
  bloated_states <- rbind(0, bloated_states)
  collapsed_states <- collapse_states(bloated_states)
  rm(bloated_states)
  collapsed_logical <- as.matrix(collapsed_states) > 0
  rm(collapsed_states)
  if (verbose) {
    cat("Creating rownames:\n")
    print(system.time(rownames(collapsed_logical) <- concat_rows(collapsed_logical)))
    cat("Garbage collection:\n")
    print(system.time(gc()))
  } else {
    rownames(collapsed_logical) <- concat_rows(collapsed_logical)
    gc()
  }
  
  return(collapsed_logical)
}
