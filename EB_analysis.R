
treated_data_zscores$yj=treated_data_zscores$totpedcr+treated_data_zscores$ksi

# Variables selected by backward elimination (from selected_data columns)
selected_vars <- names(selected_data)

# Use intersect to avoid errors if some variables are missing in treated_data
cols_to_keep <- intersect(selected_vars, names(treated_data_zscores))

treated_data__zscores_subset <- treated_data_zscores[, cols_to_keep]


# Extract intercept
intercept <- coeff[1]

# Extract coefficients vector (excluding intercept)
beta <- coeff[-1]

# If selected_vars includes yj, exclude it for predictors matrix:
predictor_vars <- selected_vars[selected_vars != "yj"]

# Subset predictors matrix in correct order:
X <- as.matrix(treated_data__zscores_subset[, predictor_vars])

# # Calculate mu_j 
mu_j <- exp(intercept + X %*% beta)

# Assuming gamma and mu_j are defined as before
alpha_j <- gamma / (gamma + mu_j)

# Calculate E(m_j | y_j)
E_mj_yj <- alpha_j * mu_j + (1 - alpha_j) * treated_data__zscores_subset$yj

# Summary
before_sum <- sum(treated_data$ksi + treated_data$totpedcr)
after_sum <- sum(treated_data$totpedcr_after)
cat("Before speed cams :", before_sum, "\n")
cat("RTM :", sum(E_mj_yj), "\n")
cat("After speed cams :", after_sum, "\n")
cat("Observed difference :", after_sum - before_sum, "\n")
cat("Actual difference :", after_sum - sum(E_mj_yj), "\n")




