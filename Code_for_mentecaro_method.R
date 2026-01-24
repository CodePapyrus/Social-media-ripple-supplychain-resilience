# ================ Formal Monte Carlo Test Code for Research ================

# Clean environment
rm(list = ls())
gc()

# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(knitr)

# Set random seed for reproducibility
set.seed(2024)

# 1. Define demand function (with additional comments and validation)
demand_function <- function(x, params) {
  # Parameter validation
  stopifnot(
    "Parameters must be a list" = is.list(params),
    "Missing parameter 'a'" = "a" %in% names(params),
    "Missing parameter 'b_list'" = "b_list" %in% names(params),
    "Missing parameter 'T_list'" = "T_list" %in% names(params),
    "Missing parameter 'mu'" = "mu" %in% names(params)
  )
  
  a <- params$a
  b_list <- params$b_list
  T_list <- params$T_list
  mu <- params$mu
  
  # Parameter boundary constraints
  a <- max(0.01, min(0.99, a))  # a between [0.01, 0.99]
  T_list <- sort(T_list)        # Ensure time points are increasing
  
  # If x exceeds maximum T, use mu (baseline value)
  if (x > max(T_list)) {
    return(mu)
  }
  
  # Piecewise function: apply different b values for each time interval
  for (i in 1:length(T_list)) {
    if (x <= T_list[i]) {
      return(-a^x + b_list[i])
    }
  }
  
  return(mu)
}

# 2. Improved Monte Carlo simulation function
run_enhanced_monte_carlo <- function(
    baseline_params = list(
      a = 0.9, 
      b_list = c(100, 85, 70), 
      T_list = c(5, 10, 15), 
      mu = 50
    ),
    n_simulations = 5000,      # Increase simulation count for statistical stability
    n_time_points = 101,       # Increase time resolution
    time_horizon = 20,
    perturbation_level = 0.15,  # Slightly increase perturbation level to test robustness
    check_convergence = TRUE    # Add convergence check
) {
  
  # Generate time points (more dense)
  time_points <- seq(0, time_horizon, length.out = n_time_points)
  
  # Pre-allocate storage
  all_demand_paths <- matrix(NA, nrow = n_simulations, ncol = n_time_points)
  all_params_list <- vector("list", n_simulations)
  
  # Store convergence check data
  convergence_data <- list()
  
  cat("Starting Monte Carlo simulation...\n")
  cat(sprintf("Number of simulations: %d, Time points: %d, Time horizon: 0-%d\n", 
              n_simulations, n_time_points, time_horizon))
  
  # Progress bar
  pb <- txtProgressBar(min = 0, max = n_simulations, style = 3)
  
  for (sim in 1:n_simulations) {
    # Generate parameter perturbations (using truncated normal distribution)
    perturbed_params <- list(
      a = max(0.01, min(0.99, 
                        baseline_params$a * exp(rnorm(1, 0, perturbation_level * 0.08)))),
      b_list = pmax(1, baseline_params$b_list + 
                      rnorm(length(baseline_params$b_list), 0, perturbation_level * 8)),
      T_list = pmax(0.5, baseline_params$T_list * 
                      (1 + rnorm(length(baseline_params$T_list), 0, perturbation_level * 0.15))),
      mu = max(0, baseline_params$mu + 
                 rnorm(1, 0, perturbation_level * 4))
    )
    
    # Ensure time points are ordered
    perturbed_params$T_list <- sort(perturbed_params$T_list)
    
    all_params_list[[sim]] <- perturbed_params
    
    # Calculate entire demand path
    all_demand_paths[sim, ] <- sapply(time_points, demand_function, 
                                      params = perturbed_params)
    
    # Convergence check (every 100 simulations)
    if (check_convergence && sim %% 100 == 0) {
      current_mean <- colMeans(all_demand_paths[1:sim, ], na.rm = TRUE)
      if (sim > 100) {
        prev_mean <- colMeans(all_demand_paths[1:(sim-100), ], na.rm = TRUE)
        rel_change <- mean(abs((current_mean - prev_mean) / prev_mean), 
                           na.rm = TRUE)
        convergence_data[[as.character(sim)]] <- rel_change
      }
    }
    
    setTxtProgressBar(pb, sim)
  }
  
  close(pb)
  
  # Calculate baseline path
  baseline_path <- sapply(time_points, demand_function, params = baseline_params)
  
  cat("Simulation completed!\n")
  
  # Output convergence information
  if (check_convergence && length(convergence_data) > 0) {
    cat("Convergence check:\n")
    conv_df <- data.frame(
      Simulation_Count = as.numeric(names(convergence_data)),
      Relative_Change_Rate = unlist(convergence_data)
    )
    print(conv_df)
  }
  
  return(list(
    time_points = time_points,
    baseline_path = baseline_path,
    all_demand_paths = all_demand_paths,
    all_params = all_params_list,
    n_simulations = n_simulations,
    baseline_params = baseline_params,
    convergence_data = if(check_convergence) convergence_data else NULL
  ))
}

# 3. Enhanced analysis function
enhanced_analyze_results <- function(results) {
  demand_paths <- results$all_demand_paths
  time_points <- results$time_points
  
  # Calculate detailed statistics
  mean_path <- colMeans(demand_paths, na.rm = TRUE)
  std_path <- apply(demand_paths, 2, sd, na.rm = TRUE)
  
  # Multiple quantiles
  quantiles <- c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)
  quantile_matrix <- apply(demand_paths, 2, quantile, probs = quantiles, na.rm = TRUE)
  
  cv <- std_path / abs(mean_path)
  
  # Calculate key time point statistics
  key_times <- unique(c(0, results$baseline_params$T_list, max(results$time_points)))
  key_stats <- list()
  
  for (i in 1:length(key_times)) {
    t <- key_times[i]
    idx <- which.min(abs(time_points - t))
    
    # Get all simulation values at this time point
    time_demands <- demand_paths[, idx]
    
    key_stats[[i]] <- list(
      time = t,
      mean = mean(time_demands, na.rm = TRUE),
      sd = sd(time_demands, na.rm = TRUE),
      median = median(time_demands, na.rm = TRUE),
      cv = sd(time_demands, na.rm = TRUE) / abs(mean(time_demands, na.rm = TRUE)),
      q5 = quantile(time_demands, 0.05, na.rm = TRUE),
      q95 = quantile(time_demands, 0.95, na.rm = TRUE),
      iqr = IQR(time_demands, na.rm = TRUE),
      min = min(time_demands, na.rm = TRUE),
      max = max(time_demands, na.rm = TRUE)
    )
  }
  
  # Calculate overall statistics
  overall_stats <- list(
    overall_mean = mean(demand_paths, na.rm = TRUE),
    overall_sd = sd(demand_paths, na.rm = TRUE),
    overall_cv = sd(demand_paths, na.rm = TRUE) / abs(mean(demand_paths, na.rm = TRUE)),
    skewness = moments::skewness(as.vector(demand_paths), na.rm = TRUE),
    kurtosis = moments::kurtosis(as.vector(demand_paths), na.rm = TRUE)
  )
  
  return(list(
    time_points = time_points,
    mean_path = mean_path,
    std_path = std_path,
    quantile_matrix = quantile_matrix,
    cv_path = cv,
    key_stats = key_stats,
    overall_stats = overall_stats
  ))
}

# 4. Generate detailed report
generate_detailed_report <- function(results, analysis) {
  report_data <- data.frame()
  
  # Basic information
  report_data <- rbind(report_data, data.frame(
    Metric = "Simulation Count",
    Value = as.character(results$n_simulations),
    Unit = "times",
    Description = "Total number of Monte Carlo simulations"
  ))
  
  report_data <- rbind(report_data, data.frame(
    Metric = "Number of Time Points",
    Value = as.character(length(results$time_points)),
    Unit = "points",
    Description = "Time series sampling points"
  ))
  
  # Key time point statistics
  for (i in 1:length(analysis$key_stats)) {
    stats <- analysis$key_stats[[i]]
    report_data <- rbind(report_data, data.frame(
      Metric = paste0("Demand at T=", stats$time),
      Value = sprintf("%.2f", stats$mean),
      Unit = "demand units",
      Description = paste0("SD=", sprintf("%.2f", stats$sd), 
                  ", CV=", sprintf("%.3f", stats$cv))
    ))
  }
  
  # Overall statistics
  report_data <- rbind(report_data, data.frame(
    Metric = "Overall Average Demand",
    Value = sprintf("%.2f", analysis$overall_stats$overall_mean),
    Unit = "demand units",
    Description = paste0("Skewness=", sprintf("%.3f", analysis$overall_stats$skewness),
                ", Kurtosis=", sprintf("%.3f", analysis$overall_stats$kurtosis))
  ))
  
  report_data <- rbind(report_data, data.frame(
    Metric = "Overall Demand Standard Deviation",
    Value = sprintf("%.2f", analysis$overall_stats$overall_sd),
    Unit = "demand units",
    Description = paste0("Coefficient of Variation=", sprintf("%.3f", analysis$overall_stats$overall_cv))
  ))
  
  return(report_data)
}

# 5. Improved visualization function
enhanced_visualization <- function(results, analysis, save_prefix = "results") {
  
  # Create dataframe for plotting
  plot_data <- data.frame(
    time = results$time_points,
    mean = analysis$mean_path,
    baseline = results$baseline_path,
    ci_95_lower = analysis$quantile_matrix["5%", ],
    ci_95_upper = analysis$quantile_matrix["95%", ],
    ci_90_lower = analysis$quantile_matrix["10%", ],
    ci_90_upper = analysis$quantile_matrix["90%", ]
  )
  
  # 1. Main result plot (with confidence intervals)
  p_main <- ggplot(plot_data, aes(x = time)) +
    # 95% confidence interval
    geom_ribbon(aes(ymin = ci_95_lower, ymax = ci_95_upper), 
                fill = "gray80", alpha = 0.5) +
    # 90% confidence interval
    geom_ribbon(aes(ymin = ci_90_lower, ymax = ci_90_upper), 
                fill = "gray60", alpha = 0.5) +
    # Mean path
    geom_line(aes(y = mean), color = "blue", linewidth = 1.2) +
    # Baseline path
    geom_line(aes(y = baseline), color = "red", linetype = "dashed", linewidth = 1) +
    # Phase transition lines
    geom_vline(xintercept = results$baseline_params$T_list, 
               linetype = "dotted", color = "darkgreen", alpha = 0.7) +
    labs(
      x = "Time", 
      y = "Demand", 
      title = "Monte Carlo Simulation Results",
      subtitle = sprintf("Number of simulations: %d, Average demand: %.2f ± %.2f", 
                         results$n_simulations,
                         analysis$overall_stats$overall_mean,
                         analysis$overall_stats$overall_sd),
      caption = "Red dashed: Baseline path | Blue solid: Mean path | Shaded: 90%/95% confidence intervals"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
  
  # 2. Coefficient of variation over time plot
  cv_data <- data.frame(
    time = results$time_points,
    cv = analysis$cv_path
  )
  
  p_cv <- ggplot(cv_data, aes(x = time, y = cv)) +
    geom_line(color = "purple", linewidth = 1) +
    geom_vline(xintercept = results$baseline_params$T_list, 
               linetype = "dotted", color = "darkgreen", alpha = 0.5) +
    labs(
      x = "Time", 
      y = "Coefficient of Variation (CV)", 
      title = "Demand Uncertainty Over Time",
      subtitle = "Coefficient of Variation = Standard Deviation / Mean"
    ) +
    theme_minimal(base_size = 12)
  
  # 3. Key time point distribution plot
  key_times <- c(0, results$baseline_params$T_list, max(results$time_points))
  key_indices <- sapply(key_times, function(t) which.min(abs(results$time_points - t)))
  
  dist_data <- data.frame()
  for (i in 1:length(key_indices)) {
    idx <- key_indices[i]
    demands <- results$all_demand_paths[, idx]
    temp_df <- data.frame(
      time = factor(paste0("T=", round(results$time_points[idx], 1))),
      demand = demands
    )
    dist_data <- rbind(dist_data, temp_df)
  }
  
  p_dist <- ggplot(dist_data, aes(x = demand, fill = time)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~time, scales = "free_y", ncol = 3) +
    labs(
      x = "Demand", 
      y = "Density", 
      title = "Demand Distribution at Key Time Points",
      fill = "Time Point"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  
  # Save charts
  ggsave(paste0(save_prefix, "_main.png"), p_main, width = 12, height = 8, dpi = 300)
  ggsave(paste0(save_prefix, "_cv.png"), p_cv, width = 10, height = 6, dpi = 300)
  ggsave(paste0(save_prefix, "_distribution.png"), p_dist, width = 12, height = 8, dpi = 300)
  
  cat(sprintf("Charts saved to: %s_main.png, %s_cv.png, %s_distribution.png\n", 
              save_prefix, save_prefix, save_prefix))
  
  return(list(main = p_main, cv = p_cv, distribution = p_dist))
}

# 6. Generate detailed statistical tables
generate_statistical_tables <- function(results, analysis) {
  
  # Key time point detailed statistics table
  key_stats_table <- data.frame()
  for (stats in analysis$key_stats) {
    key_stats_table <- rbind(key_stats_table, data.frame(
      Time_Point = stats$time,
      Mean = sprintf("%.2f", stats$mean),
      Std_Dev = sprintf("%.2f", stats$sd),
      Median = sprintf("%.2f", stats$median),
      CV = sprintf("%.3f", stats$cv),
      Min = sprintf("%.2f", stats$min),
      Max = sprintf("%.2f", stats$max),
      Q5 = sprintf("%.2f", stats$q5),
      Q95 = sprintf("%.2f", stats$q95),
      IQR = sprintf("%.2f", stats$iqr)
    ))
  }
  
  # Overall statistics table
  overall_table <- data.frame(
    Statistic = c("Mean", "Standard Deviation", "Coefficient of Variation", "Skewness", "Kurtosis"),
    Value = c(
      sprintf("%.2f", analysis$overall_stats$overall_mean),
      sprintf("%.2f", analysis$overall_stats$overall_sd),
      sprintf("%.3f", analysis$overall_stats$overall_cv),
      sprintf("%.3f", analysis$overall_stats$skewness),
      sprintf("%.3f", analysis$overall_stats$kurtosis)
    )
  )
  
  return(list(
    key_stats = key_stats_table,
    overall_stats = overall_table
  ))
}

# ================ Main Program ================

cat("Starting formal Monte Carlo analysis...\n")

# Run enhanced simulation
results <- run_enhanced_monte_carlo(
  n_simulations = 5000,        # Recommended 5000+ for formal research
  n_time_points = 101,         # High time resolution
  time_horizon = 20,
  perturbation_level = 0.15,    # Medium perturbation level
  check_convergence = TRUE      # Enable convergence check
)

# Analyze results
analysis <- enhanced_analyze_results(results)

# Generate detailed report
report_df <- generate_detailed_report(results, analysis)

cat("\n===== Detailed Report =====\n")
print(knitr::kable(report_df, format = "simple"))

# Generate statistical tables
tables <- generate_statistical_tables(results, analysis)

cat("\n===== Key Time Point Statistics =====\n")
print(knitr::kable(tables$key_stats, format = "simple"))

cat("\n===== Overall Statistics =====\n")
print(knitr::kable(tables$overall_stats, format = "simple"))

# Visualization
plots <- enhanced_visualization(results, analysis, save_prefix = "formal_study")

# Save detailed data
saveRDS(results, "monte_carlo_results.rds")
write.csv(report_df, "formal_report.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(tables$key_stats, "key_time_stats.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\n===== Results Summary =====\n")
cat(sprintf("Simulation completed: %d simulations, %d time points\n", results$n_simulations, length(results$time_points)))
cat(sprintf("Overall average demand: %.2f ± %.2f (CV=%.3f)\n", 
            analysis$overall_stats$overall_mean,
            analysis$overall_stats$overall_sd,
            analysis$overall_stats$overall_cv))
cat(sprintf("Data saved to: \n"))
cat("  - monte_carlo_results.rds (complete R data)\n")
cat("  - formal_report.csv (summary report)\n")
cat("  - key_time_stats.csv (key time point statistics)\n")
cat("  - formal_study_main.png (main chart)\n")
cat("  - formal_study_cv.png (CV chart)\n")
cat("  - formal_study_distribution.png (distribution chart)\n")
