test_delineate_comparison <- function() {
  library(testthat)
  
  # Define ranges for skill function parameters
  seeds <- c(2, 42, 100)
  sparsities <- c(0.1, 0.3, 0.5, 0.8)
  items <- c(4, 6, 10)
  skills <- c(4, 5, 6)
  conjunctive_modes <- c(TRUE, FALSE)
  disjunctive_modes <- c(TRUE, FALSE)
  
  # Loop over all combinations of parameters
  for (seed in seeds) {
    for (sparsity in sparsities) {
      for (item_count in items) {
        for (skill_count in skills) {
          for (conjunctive in conjunctive_modes) {
            for (disjunctive in disjunctive_modes) {
              # Skip invalid combinations
              if (conjunctive && disjunctive) next
              
              # Generate skill function
              skillfun <- generate.skillfun(
                seed = seed,
                sparsity = sparsity,
                items = item_count,
                skills = skill_count,
                conjunctive = conjunctive,
                disjunctive = disjunctive
              )
              
              # Run both delineate functions
              result1 <- delineate(skillfun)
              result2 <- delineate.fast(skillfun, states_as_matrix = TRUE, give_intents = TRUE)
              
              # Compare the K parameter
              test_that("K matrices are the same", {
                if (!K.are.same(result1$K, result2$K)) {
                  cat("\nTest failed for the following skill function:\n")
                  print(skillfun)
                  cat("\nK from delineate:\n")
                  print(result1$K)
                  cat("\nK from delineate.fast:\n")
                  print(result2$K)
                  cat("complete output from delineate.fast:\n")
                  print(result2)
                }
                expect_true(K.are.same(result1$K, result2$K))
              })
            }
          }
        }
      }
    }
  }
  
  cat("All tests passed!\n")
}

# Run the test
test_delineate_comparison()