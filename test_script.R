################################################################################
#                               TIMING TEST SCRIPT
################################################################################

library(future.apply)
library(dplyr)
library(readr)
library(progress)

plan(multisession, workers = availableCores() - 1)

# Configuration
OUTPUT_FILE <- "timing_results.csv"
SKILLFUNS_FILE <- "skillfuns.rds"
CHECKPOINT_INTERVAL <- 1  # Save every N completed tests

# Parameter grid
sparsity_seq <- c(0.3, 0.5, 0.7, 0.8, 0.9)
kind_seq <- c("dis", "con", "oth")
skills_seq <- seq(5, 100, by = 5)
items_seq <- c(seq(50, 200, by = 25), seq(250, 1000, by = 50))
n_reps <- 3

# Create full parameter grid
param_grid <- expand.grid(
  sparsity = sparsity_seq,
  kind = kind_seq,
  skills = skills_seq,
  items = items_seq,
  rep = 1:n_reps,
  stringsAsFactors = FALSE
)
param_grid$test_id <- seq_len(nrow(param_grid))
param_grid$seed <- param_grid$test_id + 1000

# Algorithm definitions
algorithms <- list(
#  delineate = function(sf) system.time(delineate(sf))[["elapsed"]],
  delineate_by_cheating = function(sf) system.time(delineate.by.cheating(sf))[["elapsed"]],
 cheating_slow = function(sf) system.time(placeholder_cheating_slow(sf))[["elapsed"]]
#  fcaR = function(sf) system.time(placeholder_fcaR(sf))[["elapsed"]]
)

# Placeholder functions (replace these later)
placeholder_cheating_slow <- function(sf) {
  Sys.sleep(0.01)  # Simulate work
  return(NULL)
}

placeholder_fcaR <- function(sf) {
  Sys.sleep(0.01)  # Simulate work
  return(NULL)
}

# Generate skillfun
generate_skillfun_for_test <- function(seed, sparsity, kind, skills, items) {
  set.seed(seed)
  
  if (kind == "con") {
    sf <- generate.skillfun(
      seed = seed, 
      skills = skills, 
      uniqueItems = items, 
      items = items,
      conjunctive = TRUE, 
      disjunctive = FALSE, 
      sparsity = sparsity
    )
  } else if (kind == "dis") {
    sf <- generate.skillfun(
      seed = seed, 
      skills = skills, 
      items = items, 
      uniqueItems = items,
      conjunctive = FALSE, 
      disjunctive = TRUE, 
      sparsity = sparsity
    )
  } else {  # "oth"
    repeat {
      sf <- generate.skillfun(
        seed = seed, 
        skills = skills, 
        items = items, 
        uniqueItems = items,
        conjunctive = FALSE, 
        disjunctive = FALSE, 
        sparsity = sparsity
      )
      if (!isCon(sf, 1) && !isDis(sf, 1)) break
      seed <- seed + 1
    }
  }
  
  return(sf)
}

# Load existing results
load_existing_results <- function() {
  if (file.exists(OUTPUT_FILE)) {
    return(read_csv(OUTPUT_FILE, show_col_types = FALSE))
  } else {
    return(data.frame())
  }
}

# Save results
save_results <- function(results) {
  write_csv(results, OUTPUT_FILE)
}

# Get completed tests
get_completed_tests <- function(existing_results) {
  if (nrow(existing_results) == 0) return(data.frame())
  
  existing_results %>%
    select(test_id, algorithm) %>%
    distinct()
}

# Main execution
cat("Loading existing results...\n")
existing_results <- load_existing_results()
completed_tests <- get_completed_tests(existing_results)

cat(sprintf("Found %d existing test results\n", nrow(existing_results)))
cat(sprintf("Total tests to run: %d\n", nrow(param_grid) * length(algorithms)))

cat("Starting algorithm testing...\n")

# Progress tracking
total_tests <- nrow(param_grid) * length(algorithms)
completed_count <- nrow(completed_tests)
pb <- progress_bar$new(
  format = "[:bar] :percent :current/:total ETA: :eta",
  total = total_tests,
  clear = FALSE
)
pb$tick(completed_count)

# Run tests
all_results <- existing_results
checkpoint_counter <- 0

for (algo_name in names(algorithms)) {
  cat(sprintf("\nRunning algorithm: %s\n", algo_name))
  
  for (i in 1:nrow(param_grid)) {
    test_id <- param_grid$test_id[i]
    
    # Skip if already completed
    if (any(completed_tests$test_id == test_id & completed_tests$algorithm == algo_name)) {
      pb$tick()
      next
    }
    
    row <- param_grid[i, ]
    # Print current parameters
    cat(sprintf("  Test %d/%d: sparsity=%.1f, kind=%s, skills=%d, items=%d, rep=%d\n", 
      i, nrow(param_grid), row$sparsity, row$kind, row$skills, row$items, row$rep))
    
    # Generate skillfun on-demand
    sf <- generate_skillfun_for_test(row$seed, row$sparsity, row$kind, row$skills, row$items)
    
    # Run algorithm with error handling
    timing <- tryCatch({
      algorithms[[algo_name]](sf)
    }, error = function(e) {
      cat(sprintf("Error in %s for test_id %d: %s\n", algo_name, test_id, e$message))
      NA
    })
    
    # Clear skillfun from memory immediately
    rm(sf)
    gc()
    
    # Store result
    result <- data.frame(
      test_id = test_id,
      algorithm = algo_name,
      sparsity = row$sparsity,
      kind = row$kind,
      skills = row$skills,
      items = row$items,
      rep = row$rep,
      seed = row$seed,
      elapsed_time = timing,
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )
    
    all_results <- rbind(all_results, result)
    pb$tick()
    
    # Checkpoint save
    save_results(all_results)
    cat(sprintf(" [Checkpoint: %d results saved]\n", nrow(all_results)))
  }
}

# Final save
save_results(all_results)
cat(sprintf("\nCompleted! Final results saved to %s\n", OUTPUT_FILE))
cat(sprintf("Total results: %d\n", nrow(all_results)))
