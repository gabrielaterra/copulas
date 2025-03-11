#### ---- Correlations ----

pearson_correlation <- asset_pairs %>%
  mutate(Pearson = map2_dbl(Asset1, Asset2, ~ cor(retornos_df[[.x]], retornos_df[[.y]], method = "pearson")))

spearman_correlation <- asset_pairs %>%
  mutate(Spearman = map2_dbl(Asset1, Asset2, ~ cor(retornos_df[[.x]], retornos_df[[.y]], method = "spearman")))

kendall_correlation <- asset_pairs %>%
  mutate(Kendall = map2_dbl(Asset1, Asset2, ~ cor(retornos_df[[.x]], retornos_df[[.y]], method = "kendall")))

library(reshape2)

### Criar dataframes para cada correlação
pearson_matrix <- reshape2::acast(pearson_correlation, Asset1 ~ Asset2, value.var = "Pearson")
spearman_matrix <- reshape2::acast(spearman_correlation, Asset1 ~ Asset2, value.var = "Spearman")
kendall_matrix <- reshape2::acast(kendall_correlation, Asset1 ~ Asset2, value.var = "Kendall")

library(xtable)
print(xtable(kendall_correlation), include.rownames = FALSE)

library(reshape2)

# Function to create a heatmap from a correlation matrix
plot_heatmap <- function(cor_matrix, title) {
  cor_df <- melt(cor_matrix, na.rm = TRUE)  # Reshape the matrix for ggplot
  ggplot(cor_df, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    labs(title = title, x = "Asset 1", y = "Asset 2", fill = "Correlation") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create heatmaps for each correlation type
heatmap_pearson <- plot_heatmap(pearson_matrix, "Pearson Correlation Heatmap")
heatmap_spearman <- plot_heatmap(spearman_matrix, "Spearman Correlation Heatmap")
heatmap_kendall <- plot_heatmap(kendall_matrix, "Kendall Correlation Heatmap")

# Save all three heatmaps in a single image (3 rows × 1 column)
png("heatmaps_correlations.png", width = 10, height = 15, units = "in", res = 300)
grid.arrange(heatmap_pearson, heatmap_spearman, heatmap_kendall, nrow = 3, ncol = 1)
dev.off()

library(ggplot2)
library(gridExtra)
library(purrr)

scatter_plots <- pmap(asset_pairs, function(Asset1, Asset2) {
  ggplot(retornos_df, aes(x = .data[[Asset1]], y = .data[[Asset2]])) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "red", se = FALSE) +
    theme_minimal() +
    labs(title = paste("Scatter Plot:", Asset1, "vs", Asset2),
         x = paste(Asset1, "Return"),
         y = paste(Asset2, "Return"))
})

png("scatter_plots_grid.png", width = 15, height = 9, units = "in", res = 300)
grid.arrange(grobs = scatter_plots, nrow = 3, ncol = 5)
dev.off()
