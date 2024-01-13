
# Import Libraries
library(ggplot2)
library(summarytools)
#library(Hmisc)
#library(pastecs)
library(psych)

# install impute package
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("impute")


# General information about dataframe
str(csibank)


# Set ordered levels for Education column
distinct_values <- c( "Unfinished", "Elementary", "Highschool", "Undergrad", "Graduated")

# Replace Education column with factorized ordered feature 
csibank$Education <- factor(csibank$Education, levels = distinct_values, ordered = TRUE)

# Create average of feature columns according to patterns
feat_list <- c("imag", "expe", "qual", "val", "sat", "loy")

for (feat in feat_list) {
  # Identify columns with pattern like name
  feat_cols <- csibank[, grep(feat, names(csibank))]
  
  # Perform average of those columns
  avg_feat <- rowMeans(feat_cols)
  
  # Assign corresponding average of feature
  csibank[,paste0(feat, "_avg")] <- avg_feat
}

# Rename columns for better graph explainability
replace_patterns <- list(
  c("imag1", "reputation"),
  c("imag2", "trustworthiness"),
  c("imag3", "seriousness"),
  c("imag4", "solidness"),
  c("imag5", "caring"),
  c("expe1", "prod_services"),
  c("expe2", "cust_services"),
  c("expe3", "solutions"),
  c("expe4", "expectations_qual"),
  c("qual1", "reliable_services"),
  c("qual2", "range_products"),
  c("qual3", "pers_advice"),
  c("qual4", "overall_qual"),
  c("val1", "beneficial_services"),
  c("val2", "valuable_investments"),
  c("val3", "quality_to_price"),
  c("val4", "price_to_quality"),
  c("sat1", "fulfilled_expectations"),
  c("sat2", "satisf_against_other_banks"),
  c("sat3", "perf_against_ideal_bank"),
  c("loy1", "return"),
  c("loy2", "recommend"),
  c("loy3", "sense_loyalty")
)

for (pattern in replace_patterns) {
  old_pattern <- pattern[1]
  new_pattern <- pattern[2]
  
  # Check if the old pattern exists in the dataframe
  if (old_pattern %in% names(csibank)) {
    # Find the index of the old pattern
    col_index <- which(names(csibank) == old_pattern)
    
    # Replace the old pattern with the new pattern
    names(csibank)[col_index] <- new_pattern
  }
}

# Convert all string columns to factors
csibank[] <- lapply(csibank, function(col) {
  if (is.character(col)) {
    return(as.factor(col))
  }
  return(col)
})

# Extract column names of factor type columns
factor_columns <- names(csibank)[sapply(csibank, is.factor)]

# Extract column names of real data type columns (numeric or double)
real_columns <- names(csibank)[sapply(csibank, is.numeric)]

# Get dataframe column names
column_names <- names(csibank)

# Convert the vector of column names to a string list (optional)
column_names_list <- as.list(column_names)

# Print frequence summaries for all features in dataframe
for (col in column_names_list) {
  print(freq(csibank[,col]))
}

# Print countplots for categorical features in dataframe
for (col in factor_columns) {
  # Check if the column is categorical
  if (is.factor(csibank[[col]])) {
    p <- ggplot(csibank, aes_string(x=col)) + geom_bar() + labs(x=col, y="Count")
    print(p)
  }
}

# Perform describe on dataset
describe(csibank)


# Print histograms for continuous features in dataframe
for (col in real_columns) {
  # Check if the column is categorical
  if (is.numeric(csibank[[col]])) {
    p <- ggplot(csibank, aes_string(x=col)) + geom_histogram(bins=10)
    print(p)
  }
}


# Print Violin Boxplots between continuous and factor variables

for (col in real_columns) {
  # Check if the column is categorical
  for (col2 in factor_columns) {
    p <- ggplot(csibank, aes_string(x=col2, y=col)) + 
                geom_violin(fill = "skyblue", color = "blue", alpha = 0.5) + 
                geom_boxplot(width = 0.2, fill = "white", color = "blue") + 
                theme_minimal()
    print(p)
  }
}

# ====================================================================================
# Missing values imputation

library(impute)
# Identify columns with a suitable data type for k-NN imputation (e.g., numeric columns)
numeric_columns <- sapply(csibank, is.numeric)

# Apply k-NN imputation only to numeric columns
impute_model <- impute.knn(csibank[, numeric_columns])

# Impute missing values in the original dataframe
csibank[, numeric_columns] <- complete(impute_model)


# ====================================================================================

# perform t-tests between 2 independent populations

# check age and loyalty
detach("package:psych", unload = TRUE)
library(car)


leveneTest(log(loy_avg) ~ Age,
          center=mean,
          data=csibank)
