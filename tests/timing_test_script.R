benchmark_delineate <- function(varname,
                                varvalues,
                                reps = 3,
                                seed = 42,
                                sparsity = 0.1,
                                items = 6,
                                skills = 12,
                                conjunctive = FALSE,
                                disjunctive = FALSE,
                                max_time = 50,
                                out_csv = NULL,
                                verbose = TRUE) {
  allowed_vars <- c("seed", "sparsity", "items", "skills", "conjunctive", "disjunctive")
  if (!is.character(varname) || length(varname) != 1 || !(varname %in% allowed_vars)) {
    stop("varname must be one of: ", paste(allowed_vars, collapse = ", "))
  }
  if (!exists("generate.skillfun", mode = "function")) stop("generate.skillfun() not found.")
  if (!exists("delineate", mode = "function")) stop("delineate() not found.")
  if (!exists("delineate.fast", mode = "function")) stop("delineate.fast() not found.")
  
  results <- list(); row_id <- 1L
  
  base_args <- function() {
    list(seed = seed,
         sparsity = sparsity,
         items = items,
         skills = skills,
         conjunctive = conjunctive,
         disjunctive = disjunctive)
  }
  
  coerce_value <- function(name, value) {
    if (name %in% c("conjunctive", "disjunctive")) {
      if (is.logical(value)) return(as.logical(value))
      if (is.numeric(value)) return(as.logical(value))
      if (is.character(value)) return(tolower(value) %in% c("true","t","1"))
      stop(sprintf("Cannot coerce value '%s' to logical for %s", as.character(value), name))
    }
    if (name == "seed") return(as.integer(value))
    if (name %in% c("items","skills")) return(as.integer(value))
    if (name == "sparsity") return(as.numeric(value))
    return(value)
  }
  
  funs <- c("delineate.fast", "delineate")
  # per-function stopped flags
  stopped_fn <- setNames(rep(FALSE, length(funs)), funs)
  
  for (val in varvalues) {
    args <- base_args()
    args[[varname]] <- coerce_value(varname, val)
    
    if (verbose) cat(sprintf("[Sweep] %s = %s\n", varname, as.character(val)))
    
    sf <- tryCatch({
      do.call(generate.skillfun, args)
    }, error = function(e) {
      warning(sprintf("generate.skillfun failed for %s=%s: %s", varname, val, conditionMessage(e)))
      NULL
    })
    if (is.null(sf)) {
      # log fail rows for both functions (keep structure)
      for (fn in funs) {
        for (r in seq_len(reps)) {
          results[[row_id]] <- data.frame(parameter = varname, value = val, func = fn, rep = r,
                                          user = NA_real_, system = NA_real_, elapsed = NA_real_,
                                          status = "gen_error", stringsAsFactors = FALSE)
          row_id <- row_id + 1L
        }
      }
      next
    }
    
    for (fn in funs) {
      if (verbose) cat(sprintf("  Preparing to run %s at %s=%s\n", fn, varname, as.character(val)))
      
      # If the function has already been flagged as stopped (from earlier parameter values),
      # skip calling it entirely for this parameter value (do NOT write capped rows).
      if (stopped_fn[[fn]]) {
        if (verbose) cat(sprintf("    Skipping %s entirely because it was stopped previously\n", fn))
        next
      }
      
      # run reps for this fn
      for (r in seq_len(reps)) {
        # Generate a fresh skillfun for each repetition
        rep_args <- args
        rep_args$seed <- args$seed + (r - 1) * 1000
        
        sf_rep <- tryCatch({
          do.call(generate.skillfun, rep_args)
        }, error = function(e) {
          warning(sprintf("generate.skillfun failed for %s=%s rep %d: %s", varname, val, r, conditionMessage(e)))
          NULL
        })
        
        if (is.null(sf_rep)) {
          results[[row_id]] <- data.frame(parameter = varname, value = val, func = fn, rep = r,
                                          user = NA_real_, system = NA_real_, elapsed = NA_real_,
                                          status = "gen_error", stringsAsFactors = FALSE)
          row_id <- row_id + 1L
          next
        }
        
        if (verbose) cat(sprintf("    Running %s (rep %d/%d)... ", fn, r, reps))
        t <- tryCatch({
          tt <- system.time({
            invisible(do.call(get(fn, mode = "function"), list(sf_rep)))
          })
          list(time = tt, error = NULL)
        }, error = function(e) {
          list(time = c(user.self = NA, sys.self = NA, elapsed = NA), error = e)
        })
        
        tt_names <- names(t$time)
        user_time <- if ("user.self" %in% tt_names) as.numeric(t$time["user.self"]) else as.numeric(t$time[1])
        sys_time  <- if ("sys.self"  %in% tt_names) as.numeric(t$time["sys.self"])  else as.numeric(t$time[2])
        elapsed   <- if ("elapsed"   %in% tt_names) as.numeric(t$time["elapsed"])   else as.numeric(t$time[3])
        
        status_msg <- if (is.null(t$error)) "ok" else paste0("error: ", conditionMessage(t$error))
        
        # If elapsed > max_time, mark function stopped for future reps/values.
        # We record THIS row with elapsed = NA (so plotting will break the line at this x),
        # and we do NOT write capped rows for remaining reps for this function at this value.
        if (!is.na(elapsed) && elapsed > max_time) {
          stopped_fn[[fn]] <- TRUE
          status_msg <- "exceeded"
          elapsed_record <- NA_real_   # do not set a numeric value so plotting doesn't extend it
          
          results[[row_id]] <- data.frame(parameter = varname,
                                          value = val,
                                          func = fn,
                                          rep = r,
                                          user = user_time,
                                          system = sys_time,
                                          elapsed = elapsed_record,
                                          status = status_msg,
                                          stringsAsFactors = FALSE)
          if (verbose) cat(sprintf("elapsed=%.3f s --> EXCEEDED max_time=%.3f; recording elapsed=NA and stopping %s for future reps/values\n",
                                   elapsed, max_time, fn))
          row_id <- row_id + 1L
          # stop repeating this fn for this parameter value
          break
        }
        
        # normal case: within time limit
        elapsed_record <- elapsed
        
        results[[row_id]] <- data.frame(parameter = varname,
                                        value = val,
                                        func = fn,
                                        rep = r,
                                        user = user_time,
                                        system = sys_time,
                                        elapsed = elapsed_record,
                                        status = status_msg,
                                        stringsAsFactors = FALSE)
        if (verbose) {
          if (is.null(t$error)) cat(sprintf("elapsed=%.3f s (recorded=%.3f, status=%s)\n", elapsed, elapsed_record, status_msg))
          else cat("ERROR\n")
        }
        row_id <- row_id + 1L
      } # end reps
    } # end functions
    
    rm(sf); gc()
  } # end varvalues
  
  # bind results (handle the case where results might be empty)
  if (length(results) == 0) {
    df <- data.frame(parameter = character(0), value = numeric(0), func = character(0),
                     rep = integer(0), user = numeric(0), system = numeric(0),
                     elapsed = numeric(0), status = character(0), stringsAsFactors = FALSE)
  } else {
    df <- do.call(rbind, lapply(results, function(x) if (is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = FALSE)))
    df$user <- as.numeric(df$user); df$system <- as.numeric(df$system); df$elapsed <- as.numeric(df$elapsed)
    df$rep <- as.integer(df$rep)
  }
  
  if (!is.null(out_csv)) {
    tryCatch(write.csv(df, file = out_csv, row.names = FALSE), error = function(e) warning("Failed to save CSV: ", conditionMessage(e)))
  }
  invisible(df)
}

# ---- USER PARAMETERS (sweep sequences) ----
sparsity_seq <- c(0.4, 0.5, 0.8)
kind_seq     <- c("con", "oth", "dis")
skills_seq   <- seq(6, 40, by = 2)
items_seq    <- c(seq(50, 200, by = 50), seq(250, 850, by = 100))
n_reps       <- 1  # Set repetitions to 1

# ---- Sanity checks ----
required_fns <- c("generate.skillfun", "delineate", "delineate.fast", "benchmark_delineate")
missing <- required_fns[!sapply(required_fns, exists, mode = "function")]
if (length(missing) > 0) {
  stop("Missing required functions in the environment: ", paste(missing, collapse = ", "),
      ". Please source or define them before running this script.")
}

# ---- Helper: map 'kind' to flags ----n
kind_to_flags <- function(kind) {
  if (kind == "con") return(list(conjunctive = TRUE, disjunctive = FALSE))
  if (kind == "dis") return(list(conjunctive = FALSE, disjunctive = TRUE))
  if (kind == "oth") return(list(conjunctive = FALSE, disjunctive = FALSE))
  stop("Unknown kind: ", kind)
}

library(ggplot2)
library(dplyr)

# ---- Define the list of default parameter sets to iterate over ----
defaults_list <- list(

  list(sparsity = 0.8, skills = 10L, items = 100L, kind = 'oth', seed = 1L),
  list(sparsity = 0.3, skills = 10L, items = 100L, kind = 'con', seed = 1L),
  
  list(sparsity = 0.5, skills = 10L, items = 150L, kind = 'oth', seed = 1L),
  list(sparsity = 0.5, skills = 10L, items = 150L, kind = 'con', seed = 1L)
  
)

# ---- Global control ----
global_plot_root <- "bench_plots"  # top-level folder; each default set will get its own subfolder
if (!dir.exists(global_plot_root)) dir.create(global_plot_root)
max_time <- 100  # seconds stop threshold (also used inside benchmark)

# ---- Function: run one full set of sweeps for a given default configuration ----
run_one_default <- function(default_cfg) {
  default_sparsity <- default_cfg$sparsity
  default_skills   <- as.integer(default_cfg$skills)
  default_items    <- as.integer(default_cfg$items)
  default_kind     <- default_cfg$kind
  default_seed     <- as.integer(default_cfg$seed)
  
  # Make a descriptive folder name for these defaults so results are easy to find
  safe_kind <- gsub("[^A-Za-z0-9_-]", "", default_kind)
  plot_dir <- file.path(global_plot_root,
                        sprintf("sparsity_%s_kind_%s_skills_%d_items_%d_seed_%d",
                                format(default_sparsity, trim = TRUE),
                                safe_kind,
                                default_skills,
                                default_items,
                                default_seed))
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  cat("\nRunning benchmarks for defaults:\n")
  print(default_cfg)
  cat("Saving plots + CSVs to:", plot_dir, "\n")
  
  all_results <- list()
  
  # ---- 1) Sweep sparsity_seq ----
  cat("=== Sweeping sparsity ===\n")
  flags <- kind_to_flags(default_kind)
  res_sparsity <- benchmark_delineate(varname = "sparsity",
                                      varvalues = sparsity_seq,
                                      reps = n_reps,
                                      seed = default_seed,
                                      sparsity = default_sparsity,   # will be overwritten per value by benchmark
                                      items = default_items,
                                      skills = default_skills,
                                      conjunctive = flags$conjunctive,
                                      disjunctive = flags$disjunctive,
                                      max_time = max_time,
                                      verbose = TRUE)
  all_results[["sparsity"]] <- res_sparsity
  
  # ---- 2) Sweep kind_seq ----
  cat("=== Sweeping kind ===\n")
  res_kind_list <- list()
  for (k in kind_seq) {
    flags <- kind_to_flags(k)
    res_tmp <- benchmark_delineate(varname = "seed",
                                  varvalues = default_seed,
                                  reps = n_reps,
                                  seed = default_seed,
                                  sparsity = default_sparsity,
                                  items = default_items,
                                  skills = default_skills,
                                  conjunctive = flags$conjunctive,
                                  disjunctive = flags$disjunctive,
                                  max_time = max_time,
                                  verbose = TRUE)
    res_tmp$parameter <- "kind" # overwrite "seed" with "kind"
    res_tmp$kind <- k
    res_kind_list[[k]] <- res_tmp
  }
  res_kind <- do.call(rbind, res_kind_list)
  res_kind$value <- match(res_kind$kind, kind_seq) # Use numeric representation for kind
  res_kind$kind <- NULL
  all_results[["kind"]] <- res_kind
  
  # ---- 3) Sweep skills_seq ----
  cat("=== Sweeping skills ===\n")
  flags <- kind_to_flags(default_kind)
  res_skills <- benchmark_delineate(varname = "skills",
                                    varvalues = skills_seq,
                                    reps = n_reps,
                                    seed = default_seed,
                                    sparsity = default_sparsity,
                                    items = default_items,
                                    skills = default_skills,
                                    conjunctive = flags$conjunctive,
                                    disjunctive = flags$disjunctive,
                                    max_time = max_time,
                                    verbose = TRUE)
  all_results[["skills"]] <- res_skills
  
  # ---- 4) Sweep items_seq ----
  cat("=== Sweeping items ===\n")
  flags <- kind_to_flags(default_kind)
  res_items <- benchmark_delineate(varname = "items",
                                  varvalues = items_seq,
                                  reps = n_reps,
                                  seed = default_seed,
                                  sparsity = default_sparsity,
                                  items = default_items,    # overwritten per value
                                  skills = default_skills,
                                  conjunctive = flags$conjunctive,
                                  disjunctive = flags$disjunctive,
                                  max_time = max_time,
                                  verbose = TRUE)
  all_results[["items"]] <- res_items
  
  # ---- Combine all results ----
  df_all <- do.call(rbind, all_results)
  df_all$elapsed <- as.numeric(df_all$elapsed)
  df_all$rep <- as.integer(df_all$rep)
  df_all$param_value_print <- as.character(df_all$value)

  # ---- Plotting ----
  for (param_name in unique(df_all$parameter)) {
    sub <- df_all[df_all$parameter == param_name, ]

    p <- ggplot(sub, aes(x = value, y = elapsed, color = func, group = func)) +
      geom_point(size = 3) +
      geom_line(na.rm = FALSE) +  # leave NA handling to ggplot so lines break
      labs(
        title = paste("Timing sweep for", param_name),
        x = param_name,
        y = "Elapsed time (s)",
        color = "Function"
      ) +
      theme_minimal(base_size = 13)

    if (param_name == "kind") {
      p <- p + scale_x_continuous(breaks = seq_along(kind_seq), labels = kind_seq)
    }

    fname <- file.path(plot_dir, paste0("timing_sweep_", param_name, ".png"))
    ggsave(fname, plot = p, width = 8, height = 5, dpi = 150)
    cat("Saved plot:", fname, "\n")
  }
  
  # ---- Save CSVs ----
  write.csv(df_all, file = file.path(plot_dir, "benchmark_raw_results.csv"), row.names = FALSE)
  cat("Saved raw results to", plot_dir, "\n")
  
  # Return raw results
  list(cfg = default_cfg, raw_results = df_all)
}

# ---- Run all defaults in sequence ----
results_all_defaults <- list()
for (i in seq_along(defaults_list)) {
  res <- run_one_default(defaults_list[[i]])
  results_all_defaults[[i]] <- res
}

cat("\nAll default configurations completed. Results are stored in subfolders of:", global_plot_root, "\n")

# ---- End ----
