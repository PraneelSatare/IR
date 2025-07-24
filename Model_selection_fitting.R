library(MASS)  # for glm.nb()
library(car)

# Select variables with adjusted p-value > 0.05 
vars_to_keep <- results_zscores$Column[results_zscores$Adjusted_P > 0.05]


# Make sure these vars exist in ref_data
vars_to_keep <- intersect(vars_to_keep, names(ref_data))

# Subset ref_data to keep only these variables
filtered_data <- ref_data_zscores[, vars_to_keep, drop = FALSE]

# Add the yj column
filtered_data$yj <- ref_data_zscores$totpedcr + ref_data_zscores$ksi

# View filtered_data
# head(filtered_data)


# Remove NAs
filtered_data <- filtered_data[complete.cases(filtered_data), ]

#  Fit a linear model to calculate VIFs
vif_model <- lm(yj ~ ., data = filtered_data)

# Calculate VIFs
vif_values <- vif(vif_model)

# Store VIFs as a dataframe for later viewing
vif_df <- data.frame(Variable = names(vif_values), VIF = as.numeric(vif_values))
print(vif_df[order(-vif_df$VIF), ])  # sorted by VIF descending


# Remove columns with zero or near-zero variance
nzv <- sapply(filtered_data[, -which(names(filtered_data) == "yj")], function(x) var(x, na.rm = TRUE)) > 1e-8
filtered_data <- filtered_data[, c(names(nzv)[nzv], "yj")]
filtered_data <- filtered_data[complete.cases(filtered_data), ]


#Fit the full model
nb_full <- glm.nb(
  yj ~ .,
  data = filtered_data,
  control = glm.control(maxit = 100)
)

# Variable selection using backward elemination
nb_step<-step(nb_full,direction = "backward")

# View Summary
summary(nb_step)

# View the final selected Variables
formula(nb_step)

# Get the formula of the final model
final_formula <- formula(nb_step)



#  Extract variable names from the formula (excluding response)
selected_vars <- all.vars(final_formula)[-1]  # all.vars includes response as first

#  Construct new dataframe with only selected variables + response yj
selected_data <- filtered_data[, c(selected_vars, "yj")]

# # View the new dataset
# head(selected_data)

# Extract Coeff from the final model
nb_final=glm.nb(yj ~ . ,data=selected_data,control = glm.control(maxit = 100))

coeff <- coef(nb_final)

# Extract kappa (theta) from the fitted negative binomial model
kappa <- nb_final$theta   # or nb_step$theta if you want from stepwise model

# Calculate gamma = 1 / kappa
gamma <- 1 / kappa

