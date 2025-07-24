# Load required libraries
library(ggplot2)
library(tidyr)
# Set seed for reproducibility
set.seed(42)

# Get names of columns 9 to 45
cols_to_test <- names(combined_data_zscores)[9:45]

# Convert selected columns to numeric
ref_data_zscores[cols_to_test] <- lapply(ref_data_zscores[cols_to_test], function(x) as.numeric(as.character(x)))
treated_data_zscores[cols_to_test] <- lapply(treated_data_zscores[cols_to_test], function(x) as.numeric(as.character(x)))

# Permutation test function
test_exchangeability <- function(ref_data, treat_data, col, N = 100000) {
  true_delta <- abs(mean(ref_data[[col]], na.rm = TRUE) - mean(treat_data[[col]], na.rm = TRUE))
  
  ref_data$label <- "ref"
  treat_data$label <- "treat"
  combined_data <- rbind(ref_data, treat_data)
  
  values <- combined_data[[col]]
  labels <- combined_data$label
  
  indicators <- numeric(N)
  
  for (i in 1:N) {
    permuted_labels <- sample(labels)
    rf_values <- values[permuted_labels == "ref"]
    tt_values <- values[permuted_labels == "treat"]
    
    if (length(rf_values) == 0 || length(tt_values) == 0) next
    
    cur_delta <- abs(mean(rf_values, na.rm = TRUE) - mean(tt_values, na.rm = TRUE))
    indicators[i] <- as.integer(cur_delta >= true_delta)
  }
  
  p_value <- sum(indicators, na.rm = TRUE) / sum(!is.na(indicators))
  return(p_value)
}

# Run test on all columns and store results efficiently
results_list <- vector("list", length(cols_to_test))

for (i in seq_along(cols_to_test)) {
  col <- cols_to_test[i]
  p <- test_exchangeability(ref_data_zscores, treated_data_zscores, col)
  results_list[[i]] <- data.frame(Column = col, P_Value = p)
  cat(sprintf("P-value for %s: %.4f\n", col, p))
}

# Combine results into a data frame
results_zscores <- do.call(rbind, results_list)

# Adjust for multiple comparisons
results_zscores$Adjusted_P <- p.adjust(results_zscores$P_Value, method = "BH")

# View final results
print(results_zscores)


# Load dplyr (if not already loaded)
library(dplyr)

# Reshape to long format
results_long <- results_zscores %>%
  tidyr::pivot_longer(cols = c("P_Value", "Adjusted_P"),
                      names_to = "Type",
                      values_to = "Value")

# Optional: Set factor levels to control bar order in legend
results_long$Type <- factor(results_long$Type, levels = c("P_Value", "Adjusted_P"))

# Plot
ggplot(results_long, aes(x = reorder(Column, Value), y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  coord_flip() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(title = "Raw and Adjusted P-values from Permutation Test",
       x = "Variable", y = "P-value") +
  scale_fill_manual(values = c("P_Value" = "steelblue", "Adjusted_P" = "orange"),
                    labels = c("Raw P-Value", "Adjusted P-Value")) +
  theme_minimal()

