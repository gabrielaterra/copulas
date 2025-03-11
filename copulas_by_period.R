# ---------- Fit copulas by period ----------
# Function to fit copulas by period
fit_copulas_by_period <- function(asset_pairs, data, periods) {
  results <- list()
  
  for (p in periods) {
    period_data <- data %>% filter(Period == p)
    
    period_results <- list()
    for (i in 1:nrow(asset_pairs)) {
      asset1 <- asset_pairs$Asset1[i]
      asset2 <- asset_pairs$Asset2[i]
      
      cat("Fitting copulas for", asset1, "and", asset2, "in period", p, "\n")
      
      period_results[[paste(asset1, asset2, sep = "_")]] <- 
        fit_copulas(asset1, asset2, period_data)
    }
    
    results[[p]] <- period_results
  }
  
  return(results)
}

# Fit copulas by period
copula_results_by_period <- fit_copulas_by_period(asset_pairs, retornos_df, periods)

# Create summary table by period
create_period_summary <- function(period_results, period_name) {
  data.frame(
    Period = period_name,
    Pair = names(period_results),
    BestModel = sapply(period_results, function(x) x$BestModel),
    Param1 = sapply(period_results, function(x) x$BestFit$par),
    Param2 = sapply(period_results, function(x) ifelse(is.null(x$BestFit$par2), NA, x$BestFit$par2)),
    Tau = sapply(period_results, function(x) x$BestFit$tau),
    AIC = sapply(period_results, function(x) x$BestFit$AIC)
  )
  
  
  
  # Create summary for all periods
  period_summaries <- do.call(rbind, lapply(names(copula_results_by_period), 
                                            function(p) create_period_summary(copula_results_by_period[[p]], p)))
  xtable(period_summaries, caption = "Summary of Best Copula Models by Period", label = "tab:copula_period")
  
  # Print LaTeX table
  print(latex_table, include.rownames = FALSE, sanitize.text.function = identity)
  # Visualize distribution of best models by period
  ggplot(period_summaries, aes(x = BestModel, fill = Period)) +
    geom_bar(position = "dodge") +
    theme_minimal() +
    labs(title = "Best Copula Models by Period", 
         x = "Model", y = "Frequency")
  
  # Summary of dependency parameters (Kendall's tau) by period
  ggplot(period_summaries, aes(x = Period, y = Tau, fill = Period)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Dependency Parameter (Tau) by Period", 
         x = "Period", y = "Kendall's Tau")
  
# ---------- Copula Visualization ----------
  # Function to visualize copulas 
  plot_copula <- function(asset1, asset2, data, best_model, params) {
    # Extract returns for the two assets
    x <- data[[asset1]]
    y <- data[[asset2]]
    
    # Transform to uniform distribution (0,1)
    u <- rank(x)/(length(x)+1)
    v <- rank(y)/(length(y)+1)
    
    # Original data
    pairs_data <- data.frame(u = u, v = v)
    
    # Create copula object based on best model
    if (best_model == "Normal") {
      cop <- normalCopula(params[1])
    } else if (best_model == "t_Student") {
      cop <- tCopula(params[1], df = ifelse(is.na(params[2]), 4, params[2]))
    } else if (best_model == "Clayton") {
      cop <- claytonCopula(params[1])
    } else if (best_model == "Gumbel") {
      cop <- gumbelCopula(params[1])
    } else if (best_model == "Frank") {
      cop <- frankCopula(params[1])
    }
    
    # Simulate data from copula
    sim_data <- rCopula(1000, cop)
    sim_df <- data.frame(u = sim_data[,1], v = sim_data[,2])
    
    # Plot original and simulated side by side
    p1 <- ggplot(pairs_data, aes(x = u, y = v)) +
      geom_point(alpha = 0.5, color = "blue") +
      theme_minimal() +
      labs(title = paste("Real Data:", asset1, "vs", asset2),
           x = asset1, y = asset2)
    
    p2 <- ggplot(sim_df, aes(x = u, y = v)) +
      geom_point(alpha = 0.5, color = "red") +
      theme_minimal() +
      labs(title = paste("Simulated Copula:", best_model),
           x = "u", y = "v")
    
    # Combine plots
    library(gridExtra)
    combined_plot <- grid.arrange(p1, p2, ncol = 2)
    
    return(combined_plot)
  }
  
  # Visualize selected copulas
  sample_pairs <- sample(1:nrow(asset_pairs), 3)  # Select 3 random pairs
  
  for (i in sample_pairs) {
    asset1 <- asset_pairs$Asset1[i]
    asset2 <- asset_pairs$Asset2[i]
    pair_name <- paste(asset1, asset2, sep = "_")
    
    # Get results for the pair
    result <- copula_results[[pair_name]]
    
    # Plot copula
    plot <- plot_copula(
      asset1, asset2, retornos_df, 
      result$BestModel, 
      c(result$BestFit$par, result$BestFit$par2)
    )
    
    ggsave(paste0("copula_", gsub("\\.", "_", asset1), "_", gsub("\\.", "_", asset2), ".png"), 
           plot = plot, width = 10, height = 5)
  }
  
  library(ggplot2)
  library(gridExtra)
  
  # Create an empty list to store plots
  copula_plots <- list()
  
  # Select 15 random asset pairs (if there are at least 15 pairs)
  set.seed(123) 
  num_pairs <- min(15, nrow(asset_pairs))
  sample_pairs <- sample(1:nrow(asset_pairs), num_pairs)
  
  # Generate copula plots for selected pairs
  for (i in seq_along(sample_pairs)) {
    asset1 <- asset_pairs$Asset1[sample_pairs[i]]
    asset2 <- asset_pairs$Asset2[sample_pairs[i]]
    pair_name <- paste(asset1, asset2, sep = "_")
    
    # Get copula results for the pair
    result <- copula_results[[pair_name]]
    
    # Generate plot
    copula_plots[[i]] <- plot_copula(
      asset1, asset2, retornos_df, 
      result$BestModel, 
      c(result$BestFit$par, result$BestFit$par2)
    )
  }
  
  final_plot <- marrangeGrob(copula_plots, nrow = 5, ncol = 3)
  
  # Save the arranged plot as a single image
  ggsave("copula_grid_updated.png", final_plot, width = 12, height = 18, dpi = 300)
  