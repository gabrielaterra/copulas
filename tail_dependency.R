# ---------- Tail Dependence Analysis ----------
# Calculate tail dependence for each pair
tail_dependence <- data.frame()

for (i in 1:nrow(asset_pairs)) {
  asset1 <- asset_pairs$Asset1[i]
  asset2 <- asset_pairs$Asset2[i]
  pair_name <- paste(asset1, asset2, sep = "_")
  
  # Get results for the pair
  result <- copula_results[[pair_name]]
  
  # Calculate tail dependence
  if (result$BestModel == "Normal") {
    lower_tail <- 0  # Normal copula has no tail dependence
    upper_tail <- 0
  } else if (result$BestModel == "t_Student") {
    # t-copula has symmetric tail dependence
    nu <- ifelse(is.na(result$BestFit$par2), 4, result$BestFit$par2)
    rho <- result$BestFit$par
    tail_prob <- 2 * pt(-sqrt((nu + 1) * (1 - rho) / (1 + rho)), nu + 1)
    lower_tail <- tail_prob
    upper_tail <- tail_prob
  } else if (result$BestModel == "Clayton") {
    # Clayton has lower tail dependence
    theta <- result$BestFit$par
    lower_tail <- 2^(-1/theta)
    upper_tail <- 0
  } else if (result$BestModel == "Gumbel") {
    # Gumbel has upper tail dependence
    theta <- result$BestFit$par
    lower_tail <- 0
    upper_tail <- 2 - 2^(1/theta)
  } else if (result$BestModel == "Frank") {
    # Frank has no tail dependence
    lower_tail <- 0
    upper_tail <- 0
  }
  
  tail_dependence <- rbind(tail_dependence, data.frame(
    Pair = pair_name,
    Model = result$BestModel,
    LowerTail = lower_tail,
    UpperTail = upper_tail
  ))
}

# Visualize tail dependence
ggplot(tail_dependence, aes(x = LowerTail, y = UpperTail, color = Model)) +
  geom_point(size = 3) +
  geom_text(aes(label = Pair), hjust = 0, vjust = 0, nudge_x = 0.01, nudge_y = 0.01, size = 3) +
  theme_minimal() +
  labs(title = "Tail Dependence by Asset Pair", 
       x = "Lower Tail Dependence", 
       y = "Upper Tail Dependence")