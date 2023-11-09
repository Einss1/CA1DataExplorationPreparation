# a
# Loading necessary libraries.
library(factoextra)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(naniar)

# Loading the dataset.
data <- read.csv("data.csv", header = TRUE)

# Converting categorical variables to readable labels.
data$SEX <- factor(data$SEX, levels = c(1, 2), labels = c("Female", "Male"))
data$ICU <- factor(data$ICU, levels = c(1, 2), labels = c("Admitted", "Not Admitted"))

# Converting boolean features (1 and 2) to categorical 'Yes' and 'No'.
boolean_features <- c("PNEUMONIA", "DIABETES", "HIPERTENSION", "OTHER_DISEASE", "CARDIOVASCULAR", "OBESITY", "RENAL_CHRONIC")
data[boolean_features] <- lapply(data[boolean_features], function(x) {
  factor(x, levels = c(1, 2), labels = c("Yes", "No"))
})

# Replacing missing values (represented by - 97, 98, 99) with NA.
data[data == 97 | data == 98 | data == 99] <- NA

# Checking data structure to identify variable types (categorical, discrete, continuous).
str(data)

# Visualizing distributions to further understand data type and check for missing values.
for(var in names(data)) {
  print(
    if(is.factor(data[[var]])) {
      ggplot(data, aes(x = .data[[var]])) + 
        geom_bar() +
        theme_minimal() +
        labs(title = paste("Distribution of", var))
    } else {
      ggplot(data %>% filter(!is.na(.data[[var]])), aes(x = .data[[var]])) + 
        geom_histogram(binwidth = 1) +
        theme_minimal() +
        labs(title = paste("Distribution of", var))
    }
  )
}

# Visualizing patterns of missing data.
missing_cols <- sum(colSums(is.na(data)) > 0)
if (missing_cols >= 2) {
  gg_miss_upset(data)
}

# Displaying count of missing values per variable.
colSums(is.na(data))


#b
# Calculate descriptive statistics for AGE (mean, median, min, max, and SD)
age_stats <- data.frame(
  Mean = mean(data$AGE, na.rm = TRUE),
  Median = median(data$AGE, na.rm = TRUE),
  Minimum = min(data$AGE, na.rm = TRUE),
  Maximum = max(data$AGE, na.rm = TRUE),
  Standard_Deviation = sd(data$AGE, na.rm = TRUE)
)
print(age_stats)

#c
# Scale AGE using different techniques

# Min-Max normalization
min_val <- min(data$AGE, na.rm = TRUE)
max_val <- max(data$AGE, na.rm = TRUE)
data$AGE_min_max <- (data$AGE - min_val) / (max_val - min_val)

# Z-score standardization
mean_val <- mean(data$AGE, na.rm = TRUE)
sd_val <- sd(data$AGE, na.rm = TRUE)
data$AGE_z_score <- (data$AGE - mean_val) / sd_val

# Robust scaling using IQR
q1 <- quantile(data$AGE, 0.25, na.rm = TRUE)
q3 <- quantile(data$AGE, 0.75, na.rm = TRUE)
data$AGE_robust <- (data$AGE - q1) / (q3 - q1)

# Display first few rows of original and transformed AGE
print(data[, c("AGE", "AGE_min_max", "AGE_z_score", "AGE_robust")][1:10, ])

#d
# Calculate and visualize correlations between numeric features

# Calculate correlation matrix.
cor_matrix <- cor(data[sapply(data, is.numeric)], use = "complete.obs")

# Create heatmap for visual representation of correlations.
corrplot(cor_matrix, method = "color", type = "upper", title = "Heatmap of Correlations", tl.cex = 0.7)

#e
# Further exploration and visualization.

# Correlation between AGE and ICU.
cor_age_icu <- cor.test(data$AGE, as.numeric(data$ICU), method = "pearson", use = "complete.obs")
print(paste("Correlation between Age and ICU Admission: ", round(cor_age_icu$estimate, 3)))

# Bar plots for comorbidity frequencies in ICU patients.
comorbidity_vars <- c("PNEUMONIA", "DIABETES", "HIPERTENSION", "OTHER_DISEASE", "CARDIOVASCULAR", "OBESITY", "RENAL_CHRONIC")
for(var in comorbidity_vars) {
  print(
    ggplot(data[data$ICU == "Admitted",], aes(x = .data[[var]])) +
      geom_bar() +
      theme_minimal() +
      labs(title = paste("Frequency of", var, "among ICU Admitted Patients"))
  )
}

# Grouping and visualizing AGE in categories.
data$age_group <- cut(data$AGE, breaks = c(0, 18, 35, 50, 65, 100), labels = c("0-18", "19-35", "36-50", "51-65", "66+"), include.lowest = TRUE)

# Display age group distributions and ICU admissions by age group.
print(ggplot(data, aes(x = age_group)) +
        geom_bar() +
        theme_minimal() +
        labs(title = "Distribution of Patients by Age Groups"))

print(ggplot(data, aes(x = age_group, fill = ICU)) +
        geom_bar(position = "dodge") +
        theme_minimal() +
        labs(title = "ICU Admissions by Age Groups"))