# -------------------------------
# Load Data
# -------------------------------

combined_data <- read.csv("C:/Users/prane/Desktop/Road_Safety/Data for Cape Town Pedestrian Crash Models (1).csv")
combined_data_zscores <- read.csv("C:/Users/prane/Desktop/Road_Safety/zscores.csv")

# -------------------------------
# Clean log population column in z-score data
# -------------------------------

# Replace "#NULL!" with NA
combined_data_zscores$logpopu[combined_data_zscores$logpopu == "#NULL!"] <- NA

# Convert column to numeric
combined_data_zscores$logpopu <- as.numeric(combined_data_zscores$logpopu)

# Replace NA with the minimum non-NA value
min_z <- min(combined_data_zscores$logpopu, na.rm = TRUE)
combined_data_zscores$logpopu[is.na(combined_data_zscores$logpopu)] <- min_z

# -------------------------------
# Assign Labels Based on totpedcr_after
# -------------------------------

# If totpedcr_after is NA → "ref", else "treated"
combined_data$label <- ifelse(is.na(combined_data$totpedcr_after), "ref", "treated")

# -------------------------------
# Sort both datasets by Census_2011 to align rows
# -------------------------------

combined_data <- combined_data[order(combined_data$Census_2011), ]
combined_data_zscores <- combined_data_zscores[order(combined_data_zscores$Census_2011), ]

# Copy labels from combined_data to combined_data_zscores
combined_data_zscores$label <- combined_data$label

# -------------------------------
# Split Data by Label
# -------------------------------

# Z-scored data
treated_data_zscores <- combined_data_zscores[combined_data_zscores$label == "treated", ]
ref_data_zscores     <- combined_data_zscores[combined_data_zscores$label == "ref", ]

# Raw data
treated_data <- combined_data[combined_data$label == "treated", ]
ref_data     <- combined_data[combined_data$label == "ref", ]

# -------------------------------
# Quick sanity check
# -------------------------------

table(combined_data$label)


