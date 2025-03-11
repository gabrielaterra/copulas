#### ---- Copulas ----

# ---------- Correlation analysis by period ----------
# Calculate correlations for different economic periods
periods <- unique(retornos_df$Period)

# Function to calculate correlations by period
calculate_correlations_by_period <- function(asset_pairs, data, periods) {
  results <- data.frame()
  
  for (p in periods) {
    period_data <- data %>% filter(Period == p)
    
    for (i in 1:nrow(asset_pairs)) {
      asset1 <- asset_pairs$Asset1[i]
      asset2 <- asset_pairs$Asset2[i]
      
      pearson <- cor(period_data[[asset1]], period_data[[asset2]], method = "pearson")
      spearman <- cor(period_data[[asset1]], period_data[[asset2]], method = "spearman")
      kendall <- cor(period_data[[asset1]], period_data[[asset2]], method = "kendall")
      
      results <- rbind(results, data.frame(
        Period = p,
        Asset1 = asset1,
        Asset2 = asset2,
        Pearson = pearson,
        Spearman = spearman,
        Kendall = kendall
      ))
    }
  }
  
  return(results)
}

# Calculate correlations by period
correlations_by_period <- calculate_correlations_by_period(asset_pairs, retornos_df, periods)
print(xtable(correlations_by_period), include.rownames = FALSE)

# Ensure correlations_by_period is properly formatted
correlations_by_period_long <- correlations_by_period %>% 
  pivot_longer(cols = c(Pearson, Spearman, Kendall), 
               names_to = "Method", values_to = "Correlation")

# Create boxplot for correlations by period
boxplot_period <- ggplot(correlations_by_period_long, 
                         aes(x = Method, y = Correlation, fill = Period)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Correlations by Period", x = "Method", y = "Correlation") +
  scale_fill_brewer(palette = "Set1")

# Save the boxplot image
ggsave("correlation_boxplot_period.png", plot = boxplot_period, width = 8, height = 6, dpi = 300)

# Function to create and save heatmaps for each period
plot_and_save_heatmap <- function(period_data, period_name) {
  # Ensure we only keep numeric correlation values
  period_data <- period_data %>% select(Asset1, Asset2, Pearson) %>% drop_na()
  
  # Convert to wide matrix format, filling missing values with zero
  period_matrix <- reshape2::acast(period_data, Asset1 ~ Asset2, value.var = "Pearson", fill = 0)
  
  # Generate heatmap
  heatmap_plot <- ggplot(melt(period_matrix), aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    labs(title = paste("Pearson Correlation Heatmap -", period_name),
         x = "Asset 1", y = "Asset 2", fill = "Correlation") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save heatmap
  ggsave(filename = paste0("heatmap_pearson_", period_name, ".png"), 
         plot = heatmap_plot, width = 8, height = 6, dpi = 300)
  
  return(heatmap_plot)
}

# Loop through each period and generate heatmaps
heatmap_list <- list()
for (p in periods) {
  period_data <- correlations_by_period %>% filter(Period == p)
  
  # Only generate heatmap if there is sufficient data
  if (nrow(period_data) > 1) {
    heatmap_list[[p]] <- plot_and_save_heatmap(period_data, p)
  } else {
    message("Skipping heatmap for period: ", p, " (insufficient data)")
    heatmap_list[[p]] <- NULL
  }
}

# Arrange and save all heatmaps into a single image
valid_heatmaps <- heatmap_list[!sapply(heatmap_list, is.null)]  # Remove NULL entries
if (length(valid_heatmaps) > 0) {
  png("heatmaps_pearson_all_periods.png", width = 12, height = 18, units = "in", res = 300)
  grid.arrange(grobs = valid_heatmaps, nrow = length(valid_heatmaps), ncol = 1)
  dev.off()
}
# ---------- Copula Estimation ----------
# Function to fit copulas and select the best one
fit_copulas <- function(asset1, asset2, data) {
  # Extract returns for the two assets
  x <- data[[asset1]]
  y <- data[[asset2]]
  
  # Transform to uniform distribution (0,1) using empirical distribution function
  u <- rank(x)/(length(x)+1)
  v <- rank(y)/(length(y)+1)
  
  # Copula data
  copula_data <- cbind(u, v)
  
  # Fit different types of copulas
  fit_normal <- BiCopEst(u, v, family = 1)  # Normal/Gaussian
  fit_t <- BiCopEst(u, v, family = 2)       # t-Student
  fit_clayton <- BiCopEst(u, v, family = 3) # Clayton
  fit_gumbel <- BiCopEst(u, v, family = 4)  # Gumbel
  fit_frank <- BiCopEst(u, v, family = 5)   # Frank
  
  # Calculate information criteria
  models <- list(
    Normal = fit_normal,
    t_Student = fit_t,
    Clayton = fit_clayton,
    Gumbel = fit_gumbel,
    Frank = fit_frank
  )
  
  # Compare models by AIC
  aic_values <- sapply(models, function(m) m$AIC)
  best_model <- names(which.min(aic_values))
  
  # Return results
  return(list(
    Asset1 = asset1,
    Asset2 = asset2,
    Models = models,
    AIC = aic_values,
    BestModel = best_model,
    BestFit = models[[best_model]]
  ))
}

# Fit copulas for each asset pair
copula_results <- list()
for (i in 1:nrow(asset_pairs)) {
  asset1 <- asset_pairs$Asset1[i]
  asset2 <- asset_pairs$Asset2[i]
  
  cat("Fitting copulas for", asset1, "and", asset2, "\n")
  
  # Fit for all data
  copula_results[[paste(asset1, asset2, sep = "_")]] <- 
    fit_copulas(asset1, asset2, retornos_df)
}


# Summarize copula results
copula_summary <- data.frame(
  Pair = names(copula_results),
  BestModel = sapply(copula_results, function(x) x$BestModel),
  Param1 = sapply(copula_results, function(x) x$BestFit$par),
  Param2 = sapply(copula_results, function(x) ifelse(is.null(x$BestFit$par2), NA, x$BestFit$par2)),
  Tau = sapply(copula_results, function(x) x$BestFit$tau),
  AIC = sapply(copula_results, function(x) x$BestFit$AIC)
)

print(xtable(copula_summary), include.rownames = FALSE)
