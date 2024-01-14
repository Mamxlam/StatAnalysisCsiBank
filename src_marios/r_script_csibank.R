
# Import Libraries
library(ggplot2)
library(summarytools)
#library(Hmisc)
#library(pastecs)
library(psych)

# install impute package
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#
#BiocManager::install("impute")


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

#library(impute)
# Identify columns with a suitable data type for k-NN imputation (e.g., numeric columns)
#numeric_columns <- sapply(csibank, is.numeric)

# Apply k-NN imputation only to numeric columns
#impute_model <- impute.knn(csibank[, numeric_columns], k=3)

# Impute missing values in the original dataframe
#csibank[, numeric_columns] <- complete(impute_model)


library(tidyverse)
csibank_imputed <- csibank %>% 
  mutate(across(where(~!is.factor(.)), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))



# ====================================================================================

# perform t-tests between 2 independent populations

# check age and loyalty
detach("package:psych", unload = TRUE)
library(car)

# Perform tranformation of dependent variable
log_loy <- log(csibank$loy_avg)
log_loy[log_loy == -Inf] <- NA


subset_data <- subset(csibank, Age %in% c("<=25", "36-45"))
log_loy_sub <- log(subset_data$loy_avg)
log_loy_sub[log_loy_sub == -Inf] <- NA

# perform test for homogeneity of variance
leveneTest(log_loy_sub ~ Age,
          center=mean,
          data=subset_data)

# Levene's Test for Homogeneity of Variance (center = mean)
#        Df F value Pr(>F)
# group   1  0.2664  0.606
#      507   

t.test(log_loy_sub ~ Age,
       data=subset_data,
       var.equal=T)

# Two Sample t-test
# 
# data:  log_loy_sub by Age
# t = 0.93665, df = 507, p-value = 0.3494
# alternative hypothesis: true difference in means between group <=25 and group 36-45 is not equal to 0
# 95 percent confidence interval:
#   -0.06360923  0.17952222
# sample estimates:
#   mean in group <=25 mean in group 36-45 
# 1.916506            1.858549 

# -------------------------------------------------------

# perform test for homogeneity of variance
leveneTest(log_loy ~ Gender,
           center=mean,
           data=csibank)
# Levene's Test for Homogeneity of Variance (center = mean)
#         Df F value Pr(>F)
# group    1  1.0484  0.306


# variance hypothesis is satisfied. Proceeding with t-test
t.test(log_loy ~ Gender,
       data=csibank,
       var.equal=T)


# Two Sample t-test
# 
# data:  log_loy by Gender
# t = -0.016037, df = 1673, p-value = 0.9872
# alternative hypothesis: true difference in means between group Female and group Male is not equal to 0
# 95 percent confidence interval:
#   -0.04160523  0.04093040
# sample estimates:
#   mean in group Female   mean in group Male 
# 1.884426             1.884764 

# -------------------------------------------------------



subset_data <- subset(csibank, Region %in% c("East", "North"))
log_loy_sub <- log(subset_data$loy_avg)
log_loy_sub[log_loy_sub == -Inf] <- NA

# perform test for homogeneity of variance
leveneTest(log_loy_sub ~ Region,
           center=mean,
           data=subset_data)

# Levene's Test for Homogeneity of Variance (center = mean)
#         Df F value Pr(>F)
# group    1  1.2011 0.2733
#       1076           

t.test(log_loy_sub ~ Region,
       data=subset_data,
       var.equal=T)

# Two Sample t-test
# 
# data:  log_loy_sub by Region
# t = 0.91154, df = 1076, p-value = 0.3622
# alternative hypothesis: true difference in means between group East and group North is not equal to 0
# 95 percent confidence interval:
#   -0.02677549  0.07323725
# sample estimates:
#   mean in group East mean in group North 
# 1.913579            1.890348 



# -------------------------------------------------------

# T-test for Manager - MediumEmploy in relation to loyalty

subset_data <- subset(csibank, Occupation %in% c("Manager", "MediumEmplo"))
log_loy_sub <- log(subset_data$loy_avg)
log_loy_sub[log_loy_sub == -Inf] <- NA

# perform test for homogeneity of variance
leveneTest(log_loy_sub ~ Occupation,
           center=mean,
           data=subset_data)

# Levene's Test for Homogeneity of Variance (center = mean)
#         Df F value Pr(>F)
# group    1  1.2011 0.2733
#       1076           

t.test(log_loy_sub ~ Occupation,
       data=subset_data,
       var.equal=T)

# Two Sample t-test
# 
# data:  log_loy_sub by Occupation
# t = -1.4676, df = 810, p-value = 0.1426
# alternative hypothesis: true difference in means between group Manager and group MediumEmplo is not equal to 0
# 95 percent confidence interval:
#   -0.13431523  0.01939391
# sample estimates:
#   mean in group Manager mean in group MediumEmplo 
# 1.809779                  1.867239 


# -------------------------------------------------------

# T-test for Manager - MediumEmploy in relation to loyalty

subset_data <- subset(csibank, Occupation %in% c("Manager", "Retired"))
log_loy_sub <- log(subset_data$loy_avg)
log_loy_sub[log_loy_sub == -Inf] <- NA

# perform test for homogeneity of variance
leveneTest(log_loy_sub ~ Occupation,
           center=mean,
           data=subset_data)

# Levene's Test for Homogeneity of Variance (center = mean)
#        Df F value  Pr(>F)  
# group   1  4.9333 0.02684 *
#       456                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1      


t.test(log_loy_sub ~ Occupation,
       data=subset_data,
       var.equal=F)

# Welch Two Sample t-test
# 
# data:  log_loy_sub by Occupation
# t = -2.4179, df = 312.56, p-value = 0.01618
# alternative hypothesis: true difference in means between group Manager and group Retired is not equal to 0
# 95 percent confidence interval:
#   -0.20089605 -0.02062864
# sample estimates:
#   mean in group Manager mean in group Retired 
# 1.809779              1.920541


# Statistical significant difference between managers and retired over loyalty towards the bank.



# -------------------------------------------------------

# T-test for Unfinished - Elementary in relation to loyalty

subset_data <- subset(csibank, Education %in% c("Unfinished", "Elementary"))
log_loy_sub <- log(subset_data$loy_avg)
log_loy_sub[log_loy_sub == -Inf] <- NA

# perform test for homogeneity of variance
leveneTest(log_loy_sub ~ Education,
           center=mean,
           data=subset_data)

# Levene's Test for Homogeneity of Variance (center = mean)
#        Df F value Pr(>F)
# group   1  0.0565 0.8123
#       385       


t.test(log_loy_sub ~ Education,
       data=subset_data,
       var.equal=T)

# Two Sample t-test
# 
# data:  log_loy_sub by Education
# t = -0.67512, df = 385, p-value = 0.5
# alternative hypothesis: true difference in means between group Unfinished and group Elementary is not equal to 0
# 95 percent confidence interval:
#   -0.18105371  0.08849767
# sample estimates:
#   mean in group Unfinished mean in group Elementary 
# 1.894819                 1.941097


# No Statisticaly significant difference between Unfinished and Elementary over loyalty towards the bank.


# -------------------------------------------------------

# T-test for Unfinished - Graduated in relation to loyalty

subset_data <- subset(csibank, Education %in% c("Unfinished", "Graduated"))
log_loy_sub <- log(subset_data$loy_avg)
log_loy_sub[log_loy_sub == -Inf] <- NA

# perform test for homogeneity of variance
leveneTest(log_loy_sub ~ Education,
           center=mean,
           data=subset_data)

# Levene's Test for Homogeneity of Variance (center = mean)
#        Df F value Pr(>F)
# group   1   1.769  0.186
#       121  


t.test(log_loy_sub ~ Education,
       data=subset_data,
       var.equal=T)

# Two Sample t-test
# 
# data:  log_loy_sub by Education
# t = 1.2778, df = 121, p-value = 0.2038
# alternative hypothesis: true difference in means between group Unfinished and group Graduated is not equal to 0
# 95 percent confidence interval:
#   -0.07108186  0.32984140
# sample estimates:
#   mean in group Unfinished  mean in group Graduated 
# 1.894819                 1.765440 


# No Statistically significant difference between Unfinished and Graduated over loyalty towards the bank.

# -------------------------------------------------------

library(GGally)

selected_columns <- csibank %>% select(ends_with("_avg"))
selected_columns <- log(selected_columns)
selected_columns[selected_columns == -Inf] <- NA

ggpairs(
  selected_columns,
  upper = list(
    continuous = wrap("cor", method = "pearson", size = 5) # As we use log than we can use pearson (parametric)
  ),
  diag = list(
    continuous = wrap("densityDiag", alpha = 0.5)
  ),
  lower = list(
    continuous = wrap("points", size = 1, alpha = 0.8)
  )
) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "bold"),
    strip.text.x = element_text(size = 12, hjust = 0.5, vjust = 0.5, face = 'bold'),
    strip.text.y = element_text(size = 6, hjust = 0.5, vjust = 0.5, face = 'bold'),
    axis.text.x = element_text(face = "bold", color = "black", size = 14),
    axis.text.y = element_text(face = "bold", color = "black", size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", color = "black", size = 20),
    plot.title = element_text(face = "bold", color = "black", size = 22, hjust = 0.5),
    panel.grid = element_blank())


# -------------------------------------------------------

# From histograms, imag_avg and reputation do not follow normal dist
calculate_pears_correlation <- function(df1, x_column, y_column) {
  # Log transformation and handling -Inf values for df1
  log_y_df1 <- log(df1[[y_column]])
  log_y_df1[is.infinite(log_y_df1)] <- NA
  
  log_x_df1 <- log(df1[[x_column]])
  log_x_df1[is.infinite(log_x_df1)] <- NA
  
  
  # Correlation test
  correlation_result <- cor.test(log_y_df1, log_x_df1, method = "pearson")
  
  # Print or return the result
  print(correlation_result)
  # Alternatively, you can return the result if you want to store or further process it.
  # return(correlation_result)
}


# From histograms, imag_avg and reputation do not follow normal dist
calculate_spear_correlation <- function(df1, x_column, y_column) {
  # Correlation test
  correlation_result <- cor.test(df1[[y_column]],df1[[x_column]], method = "spearman")
  
  # Print or return the result
  print(correlation_result)
  # Alternatively, you can return the result if you want to store or further process it.
  # return(correlation_result)
}

calculate_pears_correlation(csibank, "imag_avg", "reputation") # p-value < 2.2e-16 , 0.809899
calculate_pears_correlation(csibank, "imag_avg", "trustworthiness") # p-value < 2.2e-16 , 0.7882289
calculate_pears_correlation(csibank, "imag_avg", "seriousness") # p-value < 2.2e-16 , 0.7662788  
calculate_pears_correlation(csibank, "imag_avg", "solidness") # p-value < 2.2e-16 , 0.7559177
calculate_pears_correlation(csibank, "imag_avg", "caring") # p-value < 2.2e-16 , 0.613812

calculate_pears_correlation(csibank, "expe_avg", "prod_services") 
calculate_pears_correlation(csibank, "expe_avg", "cust_services") 
calculate_pears_correlation(csibank, "expe_avg", "solutions") 
calculate_pears_correlation(csibank, "expe_avg", "expectations_qual")

calculate_pears_correlation(csibank, "qual_avg", "reliable_services") 
calculate_pears_correlation(csibank, "qual_avg", "range_products") 
calculate_pears_correlation(csibank, "qual_avg", "pers_advice") 
calculate_pears_correlation(csibank, "qual_avg", "overall_qual")

calculate_pears_correlation(csibank, "val_avg", "beneficial_services") 
calculate_pears_correlation(csibank, "val_avg", "valuable_investments") 
calculate_pears_correlation(csibank, "val_avg", "quality_to_price") 
calculate_pears_correlation(csibank, "val_avg", "price_to_quality")

calculate_pears_correlation(csibank, "sat_avg", "fulfilled_expectations") 
calculate_pears_correlation(csibank, "sat_avg", "satisf_against_other_banks") 
calculate_pears_correlation(csibank, "sat_avg", "perf_against_ideal_bank") 
