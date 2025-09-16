################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

#                               TIMING TEST SCRIPT

################################################################################
################################################################################


library(future.apply)
library(dplyr)
library(ggplot2)
library(tidyr)

plan(multisession)

# Generate one test case with timing
run_timed_test <- function(seed, skills, items, type) {
  set.seed(seed)
  if (type == "con") {
    sf <- generate.skillfun(seed, skills = skills, uniqueItems = items, items=items, conjunctive = TRUE, disjunctive = FALSE, sparsity = 0.8)
  } else if (type == "dis") {
    sf <- generate.skillfun(seed, skills = skills, items = items, uniqueItems = items ,conjunctive = FALSE, disjunctive = TRUE, sparsity = 0.8)
  } else {
    repeat {
      sf <- generate.skillfun(seed, skills = skills, items = items, uniqueItems = items, conjunctive = FALSE, disjunctive = FALSE, sparsity = 0.8)
      if (!isCon(sf, 1) && !isDis(sf, 1)) break
      seed <- seed + 1  # Adjust seed to get valid sf
    }
  }
  
  t1 <- system.time(delineate(sf))[["elapsed"]]
  t2 <- system.time(delineate.by.cheating(sf))[["elapsed"]]
  gc()
  return(data.frame(
    seed = seed,
    skills = skills,
    items = items,
    type = type,
    time_delineate = t1,
    time_delineate_by_cheating = t2
  ))
}

# Parameter grid
skills_seq <- c(5, 10, 15, 20)#, 30)
items_seq  <- c(10, 20, 30, 60)#, 120, 250, 500, 750, 1000)
types      <- c("con", "dis","oth.")
n_reps     <- 5

param_grid <- expand.grid(skills = skills_seq, items = items_seq, type = types, rep = 1:n_reps)
param_grid$seed <- seq_len(nrow(param_grid)) + 1000  # Unique reproducible seeds

# Run in parallel
results <- lapply(1:nrow(param_grid), function(i) {
  with(param_grid[i, ], {
    run_timed_test(seed, skills, items, type)
  })
})

# Combine results
df_results <- bind_rows(results)

# Convert to long format for ggplot
df_long <- df_results %>%
  pivot_longer(
    cols = starts_with("time_"),
    names_to = "func",
    values_to = "time"
  ) %>%
  mutate(func = gsub("time_", "", func))

# Plot with ggplot2
ggplot(df_long, aes(x = items, y = time, color=func)) +
  geom_point(alpha = 0.6) +
  facet_grid(type ~ skills, labeller = label_both) +
  theme_minimal() +
  labs(
    title = "Timing Comparison by SF Parameters",
    x = "# of Items",
    y = "Elapsed Time (s)",
    color = "Function"
  )

