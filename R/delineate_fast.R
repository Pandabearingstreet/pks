delineate_conjunctive_base <- function(skillfun, itemID = 1, is_dis=FALSE, item_idx=NULL, item.names=NULL, states_as_matrix=FALSE, give_intents=FALSE) {
  if (is.null(item.names)) {
    item.names <- as.character(skillfun[, itemID])
  }
  mu <- as.matrix(skillfun[, -itemID])
  nskills <- ncol(mu)
  skill.names <- colnames(mu)
  I <- !mu

  concepts <- compute_concepts(I, is_dis, item_idx, states_as_matrix, give_intents)
  
  if (give_intents) {
    if (!is_dis) {
      concepts$Intents <- +(concepts$Intents)
      rownames(concepts$Intents) <- concepts$StateNames
      colnames(concepts$Intents) <- skill.names    
    } else {
      concepts$Intents <- "for disjunctive case, then amount of intents is too large to be efficiently calculated"
    }
  }

  if (states_as_matrix) {  
    states <- +(concepts$Extents)
    colnames(states) <- item.names
    rownames(states) <- concepts$StateNames
    return(list(K = states, intents = if (give_intents) concepts$Intents else NULL))
  } else {
    return(list(statenames = unlist(concepts$StateNames), items = item.names, intents = if (give_intents) concepts$Intents else NULL))
  }
}

delineate.fast <- function(skillfun, itemID = 1, states_as_matrix=FALSE, give_intents=FALSE) {

  # check for case 1
  if (isCon(skillfun, itemID)) {  
    output <- delineate_conjunctive_base(skillfun, itemID, states_as_matrix = states_as_matrix, give_intents = give_intents)
    return(output)
  } 
  # case 2 & 3
  item.names <- as.character(skillfun[, itemID])
  unique_items <- unique(item.names)
  # get a mapping from each row to the idx of its item in the unique item list
  item_idx <- match(item.names, unique_items)
  
  if (isDis(skillfun, itemID)) { 
    output <- delineate_conjunctive_base(skillfun, itemID, is_dis = TRUE, item_idx = item_idx, item.names = unique_items, states_as_matrix = states_as_matrix, give_intents = give_intents)

  } else {
    # else, must be case 3:
    output <- delineate_conjunctive_base(skillfun, itemID, item_idx = item_idx, item.names = unique_items, states_as_matrix = states_as_matrix, give_intents = give_intents)
    # remove any duplicate states
    if (states_as_matrix) {
      output$K <- output$K[!duplicated(rownames(output$K)), ]
    } else {
      output$statenames <- unique(output$statenames)
    }  
  }

  return(output)
}
