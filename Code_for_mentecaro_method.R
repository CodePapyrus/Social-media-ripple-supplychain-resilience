# Clean environment
rm(list = ls())
gc()

# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Set random seed
set.seed(123)

# 1. Define simple demand function (without using classes)
demand_function <- function(x, params) {
  # params = list(a, b_list, T_list, mu)
  a <- params$a
  b_list <- params$b_list
  T_list <- params$T_list
  mu <- params$mu
  
  # Ensure parameter validity
  a <- max(0.01, min(0.99, a))
  T_list <- sort(T_list)  # Ensure increasing order
  
  if (x > max(T_list)) {
    return(mu)
  }
  
  for (i in 1:length(T_list)) {
    if (x <= T_list[i]) {
      return(-a^x + b_list[i])
    }
  }
  
  return(mu)
}

# 2. Run Monte Carlo simulation function
run_monte_carlo <- function(
    baseline_params = list(a = 0.9, b_list = c(100, 85, 70), 
                           T_list = c(5, 10, 15), mu = 50),
    n_simulations = 200,
    n_time_points = 50,
    time_horizon = 20,
    perturbation_level = 0.1
) {
  
  # Generate time points
  time_points <- seq(0.1, time_horizon, length.out = n_time_points)
  
  # Store results
  all_demand_paths <- matrix(NA, nrow = n_simulations, ncol = n_time_points)
  all_params_list <- vector("list", n_simulations)
  
  cat("Starting Monte Carlo simulation...\n")
  
  for (sim in 1:n_simulations) {
    # Generate perturbed parameters
    perturbed_params <- list(
      a = baseline_params$a * exp(rnorm(1, 0, perturbation_level * 0.1)),
      b_list = baseline_params$b_list + rnorm(length(baseline_params$b_list), 0, perturbation_level * 10),
      T_list = baseline_params$T_list * (1 + rnorm(length(baseline_params$T_list), 0, perturbation_level)),
      mu = baseline_params$mu + rnorm(1, 0, perturbation_level * 5)
    )
    
    # Ensure parameter validity
    perturbed_params$a <- max(0.01, min(0.99, perturbed_params$a))
    perturbed_params$b_list <- pmax(1, perturbed_params$b_list)
    perturbed_params$T_list <- pmax(0.1, perturbed_params$T_list)
    perturbed_params$mu <- max(0, perturbed_params$mu)
    perturbed_params$T_list <- sort(perturbed_params$T_list)
    
    all_params_list[[sim]] <- perturbed_params
    
    # Calculate demand path
    for (t in 1:n_time_points) {
      all_demand_paths[sim, t] <- demand_function(time_points[t], perturbed_params)
    }
    
    if (sim %% 10 == 0) cat(sprintf("Progress: %d/%d\n", sim, n_simulations))
  }
  
  # Calculate baseline path
  baseline_path <- sapply(time_points, demand_function, params = baseline_params)
  
  cat("Simulation completed!\n")
  
  return(list(
    time_points = time_points,
    baseline_path = baseline_path,
    all_demand_paths = all_demand_paths,
    all_params = all_params_list,
    n_simulations = n_simulations,
    baseline_params = baseline_params
  ))
}

# 3. Analysis function
analyze_results <- function(results) {
  demand_paths <- results$all_demand_paths
  
  # Calculate basic statistics
  mean_path <- colMeans(demand_paths, na.rm = TRUE)
  std_path <- apply(demand_paths, 2, sd, na.rm = TRUE)
  ci_90_lower <- apply(demand_paths, 2, quantile, 0.05, na.rm = TRUE)
  ci_90_upper <- apply(demand_paths, 2, quantile, 0.95, na.rm = TRUE)
  cv <- std_path / abs(mean_path)
  
  # Stage statistics
  stage_T <- results$baseline_params$T_list
  stage_stats <- list()
  
  for (i in 1:length(stage_T)) {
    idx <- which.min(abs(results$time_points - stage_T[i]))
    stage_demands <- demand_paths[, idx]
    stage_stats[[i]] <- list(
      stage = i,
      time = stage_T[i],
      mean = mean(stage_demands, na.rm = TRUE),
      sd = sd(stage_demands, na.rm = TRUE),
      cv = sd(stage_demands, na.rm = TRUE) / abs(mean(stage_demands, na.rm = TRUE))
    )
  }
  
  return(list(
    mean_path = mean_path,
    std_path = std_path,
    ci_90_lower = ci_90_lower,
    ci_90_upper = ci_90_upper,
    cv = cv,
    stage_stats = stage_stats
  ))
}

# 4. Generate report function
generate_report <- function(results, analysis) {
  report_data <- data.frame()
  
  # Basic information
  report_data <- rbind(report_data, data.frame(
    Indicator = "Number of simulations",
    Value = as.character(results$n_simulations),
    Unit = "times",
    Description = "Total number of Monte Carlo simulations"
  ))
  
  # Stage statistics
  for (i in 1:length(analysis$stage_stats)) {
    stats <- analysis$stage_stats[[i]]
    report_data <- rbind(report_data, data.frame(
      Indicator = paste0("Stage ", i, " transition point demand mean"),
      Value = sprintf("%.2f", stats$mean),
      Unit = "Demand units",
      Description = paste0("Time point T=", stats$time, ", CV=", sprintf("%.3f", stats$cv))
    ))
  }
  
  # Overall statistics
  report_data <- rbind(report_data, data.frame(
    Indicator = "Average demand",
    Value = sprintf("%.2f", mean(results$all_demand_paths, na.rm = TRUE)),
    Unit = "Demand units",
    Description = "Average demand across all simulations and time points"
  ))
  
  report_data <- rbind(report_data, data.frame(
    Indicator = "Demand standard deviation",
    Value = sprintf("%.2f", sd(results$all_demand_paths, na.rm = TRUE)),
    Unit = "Demand units",
    Description = "Overall standard deviation of demand"
  ))
  
  return(report_data)
}

# 5. Visualization function
visualize_results <- function(results, analysis, save_path = NULL) {
  plot_data <- data.frame(
    time = results$time_points,
    mean = analysis$mean_path,
    baseline = results$baseline_path,
    ci_lower = analysis$ci_90_lower,
    ci_upper = analysis$ci_90_upper
  )
  
  p <- ggplot(plot_data, aes(x = time)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                fill = "gray70", alpha = 0.5) +
    geom_line(aes(y = baseline), color = "red", linewidth = 1) +
    geom_line(aes(y = mean), color = "blue", linetype = "dashed", linewidth = 1) +
    labs(x = "Time", y = "Demand", 
         title = "Monte Carlo Simulation Results",
         subtitle = paste0("Number of simulations: ", results$n_simulations)) +
    theme_minimal()
  
  # Add stage transition lines
  for (T in results$baseline_params$T_list) {
    p <- p + geom_vline(xintercept = T, linetype = "dotted", color = "darkgreen", alpha = 0.5)
  }
  
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 10, height = 6, dpi = 300)
    cat("Chart saved to:", save_path, "\n")
  }
  
  return(p)
}

# ================ Main Program ================

cat("Starting Monte Carlo test...\n")

# Run simulation
results <- run_monte_carlo(
  n_simulations = 200,  # Number of simulations
  n_time_points = 50,   # Number of time points
  time_horizon = 20,    # Time horizon
  perturbation_level = 0.1  # Perturbation level
)

# Analyze results
analysis <- analyze_results(results)

# Generate report
report_df <- generate_report(results, analysis)
cat("\n===== Report Results =====\n")
print(report_df)

# Visualize
p <- visualize_results(results, analysis, save_path = "final_results.png")
print(p)

# Save report
write.csv(report_df, "final_report.csv", row.names = FALSE)
cat("\nReport saved to: final_report.csv\n")
cat("Chart saved to: final_results.png\n")

# Display detailed stage statistics
cat("\n===== Detailed Stage Statistics =====\n")
for (i in 1:length(analysis$stage_stats)) {
  stats <- analysis$stage_stats[[i]]
  cat(sprintf("Stage %d (T=%.1f):\n", i, stats$time))
  cat(sprintf("  Mean: %.2f\n", stats$mean))
  cat(sprintf("  Standard deviation: %.2f\n", stats$sd))
  cat(sprintf("  Coefficient of variation: %.3f\n", stats$cv))
  cat("\n")
}
