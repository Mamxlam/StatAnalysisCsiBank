
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


# No missing values
install.packages("visdat")
library(visdat)
vis_miss(csibank)

# General information about dataframe
str(csibank)



# Find zero values in data that will mess up the log transformation
zero_count <- colSums(is.na(csibank) | csibank == 0, na.rm = TRUE)

# Display the result
print(zero_count)


# Gender                        Age                  Education                 Occupation                     Region                 reputation            trustworthiness 
# 0                          0                          0                          0                          0                         10                         15 
# seriousness                  solidness                     caring              prod_services              cust_services                  solutions          expectations_qual 
# 9                         19                         30                         12                         17                         26                         11 
# reliable_services             range_products                pers_advice               overall_qual        beneficial_services       valuable_investments           quality_to_price 
# 22                         21                         20                         21                         55                         26                         35 
# price_to_quality     fulfilled_expectations satisf_against_other_banks    perf_against_ideal_bank                     return                  recommend              sense_loyalty 
# 136                         25                         25                         43                         97                         64                        111 
# imag_avg                   expe_avg                   qual_avg                    val_avg                    sat_avg                    loy_avg 
# 2                          2                          3                         14                         15                         32 







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

calculate_pears_correlation(csibank, "imag_avg", "reputation") # p-value < 2.2e-16 , 0.809899 ***
calculate_pears_correlation(csibank, "imag_avg", "trustworthiness") # p-value < 2.2e-16 , 0.7882289
calculate_pears_correlation(csibank, "imag_avg", "seriousness") # p-value < 2.2e-16 , 0.7662788  
calculate_pears_correlation(csibank, "imag_avg", "solidness") # p-value < 2.2e-16 , 0.7559177
calculate_pears_correlation(csibank, "imag_avg", "caring") # p-value < 2.2e-16 , 0.613812

calculate_pears_correlation(csibank, "expe_avg", "prod_services") # p-value < 2.2e-16 , 0.4564061 
calculate_pears_correlation(csibank, "expe_avg", "cust_services") # p-value < 2.2e-16 , 0.4653631 
calculate_pears_correlation(csibank, "expe_avg", "solutions") # p-value < 2.2e-16 , 0.8536487   ***
calculate_pears_correlation(csibank, "expe_avg", "expectations_qual") # p-value < 2.2e-16 , 0.613812

calculate_pears_correlation(csibank, "qual_avg", "reliable_services") # p-value < 2.2e-16 , 0.613812
calculate_pears_correlation(csibank, "qual_avg", "range_products") # p-value < 2.2e-16 , 0.5966636 
calculate_pears_correlation(csibank, "qual_avg", "pers_advice") # p-value < 2.2e-16 , 0.5284636 
calculate_pears_correlation(csibank, "qual_avg", "overall_qual") # p-value < 2.2e-16 , 0.7190517 ***

calculate_pears_correlation(csibank, "val_avg", "beneficial_services") # p-value < 2.2e-16 , 0.5522145  
calculate_pears_correlation(csibank, "val_avg", "valuable_investments") # p-value < 2.2e-16 , 0.9993053  ***
calculate_pears_correlation(csibank, "val_avg", "quality_to_price") # p-value < 2.2e-16 , 0.5012689  
calculate_pears_correlation(csibank, "val_avg", "price_to_quality") # p-value < 2.2e-16 , 0.3122601  

calculate_pears_correlation(csibank, "sat_avg", "fulfilled_expectations") # p-value < 2.2e-16 , 0.7003888   
calculate_pears_correlation(csibank, "sat_avg", "satisf_against_other_banks") # p-value < 2.2e-16 , 0.9996887   ***
calculate_pears_correlation(csibank, "sat_avg", "perf_against_ideal_bank") # p-value < 2.2e-16 , 0.6137677   

calculate_pears_correlation(csibank, "loy_avg", "imag_avg") # p-value < 2.2e-16 , 0.5141267 
calculate_pears_correlation(csibank, "loy_avg", "expe_avg") # p-value < 2.2e-16 , 0.4840054 
calculate_pears_correlation(csibank, "loy_avg", "qual_avg") # p-value < 2.2e-16 , 0.5295417   
calculate_pears_correlation(csibank, "loy_avg", "val_avg") # p-value < 2.2e-16 , 0.6306719 
calculate_pears_correlation(csibank, "loy_avg", "sat_avg") # p-value < 2.2e-16 , 0.7026083


# Non - parametric
calculate_spear_correlation(csibank, "imag_avg", "reputation") # p-value < 2.2e-16 , 0.7923146  
calculate_spear_correlation(csibank, "imag_avg", "trustworthiness") # p-value < 2.2e-16 , 0.7901882 
calculate_spear_correlation(csibank, "imag_avg", "seriousness") # p-value < 2.2e-16 , 0.7776717   
calculate_spear_correlation(csibank, "imag_avg", "solidness") # p-value < 2.2e-16 , 0.7980856
calculate_spear_correlation(csibank, "imag_avg", "caring") # p-value < 2.2e-16 , 0.6523445 

calculate_spear_correlation(csibank, "expe_avg", "prod_services") # p-value < 2.2e-16 , 0.6303961  
calculate_spear_correlation(csibank, "expe_avg", "cust_services") # p-value < 2.2e-16 , 0.6103734  
calculate_spear_correlation(csibank, "expe_avg", "solutions") # p-value < 2.2e-16 , 0.6656642   
calculate_spear_correlation(csibank, "expe_avg", "expectations_qual") # p-value < 2.2e-16 , 0.8619103 

calculate_spear_correlation(csibank, "qual_avg", "reliable_services") # p-value < 2.2e-16 , 0.5423402 
calculate_spear_correlation(csibank, "qual_avg", "range_products") # p-value < 2.2e-16 , 0.6441465 
calculate_spear_correlation(csibank, "qual_avg", "pers_advice") # p-value < 2.2e-16 , 0.6261824  
calculate_spear_correlation(csibank, "qual_avg", "overall_qual") # p-value < 2.2e-16 , 0.7440752

calculate_spear_correlation(csibank, "val_avg", "beneficial_services") # p-value < 2.2e-16 , 0.7627603   
calculate_spear_correlation(csibank, "val_avg", "valuable_investments") # p-value < 2.2e-16 , 0.980741
calculate_spear_correlation(csibank, "val_avg", "quality_to_price") # p-value < 2.2e-16 , 0.7840264   
calculate_spear_correlation(csibank, "val_avg", "price_to_quality") # p-value < 2.2e-16 , 0.5965029   

calculate_spear_correlation(csibank, "sat_avg", "fulfilled_expectations") # p-value < 2.2e-16 , 0.8720271    
calculate_spear_correlation(csibank, "sat_avg", "satisf_against_other_banks") # p-value < 2.2e-16 , 0.9835097 
calculate_spear_correlation(csibank, "sat_avg", "perf_against_ideal_bank") # p-value < 2.2e-16 , 0.7725236    

calculate_spear_correlation(csibank, "loy_avg", "imag_avg") # p-value < 2.2e-16 , 0.5866996  
calculate_spear_correlation(csibank, "loy_avg", "expe_avg") # p-value < 2.2e-16 , 0.6739011  
calculate_spear_correlation(csibank, "loy_avg", "qual_avg") # p-value < 2.2e-16 , 0.6837332    
calculate_spear_correlation(csibank, "loy_avg", "val_avg") # p-value < 2.2e-16 , 0.63419 
calculate_spear_correlation(csibank, "loy_avg", "sat_avg") # p-value < 2.2e-16 , 0.7117097 


# ====================================================================================

one_hot_encode <- function(df) {
  factor_columns <- sapply(df, is.factor)
  
  # Check if there are any factor columns
  if (any(factor_columns)) {
    # Identify factor columns
    factor_names <- names(df)[factor_columns]
    
    # Perform one-hot encoding for factor columns
    encoded_df <- df
    for (col in factor_names) {
      encoded_df <- cbind(encoded_df, model.matrix(~ . - 1, data = df[, col, drop = FALSE]))
    }
    
    # Remove the original factor columns
    encoded_df <- encoded_df[, !factor_columns, drop = FALSE]
    
    # Convert the result back to a data frame
    encoded_df <- as.data.frame(encoded_df)
    
    return(encoded_df)
  } else {
    cat("No factor columns found in the data frame.\n")
    return(df)
  }
}

# Copy data of csibank
csibank_no_zeros <- csibank

# Remove zeros to not mess up log transforms
csibank_no_zeros[real_columns] <- lapply(csibank[real_columns], function(x) ifelse(x == 0, 0.01, x))

# Perform one hot encoding to categorical variables
csibank_no_zeros <- one_hot_encode(csibank_no_zeros)


# Approach -> Will add variables (constructs only) that most correlate with the dependent variable

# With NAs in zeros

# Train linear regression model on data
mdl1 <- lm(log(loy_avg) ~ log(sat_avg),
           data = csibank_no_zeros)
summary(mdl1)

# Call:
# lm(formula = log(loy_avg) ~ log(qual_avg), data = csibank_no_zeros)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -3.10643 -0.08743  0.07162  0.17955  2.04792 
# 
#   Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    0.27715    0.06357    4.36 1.38e-05 ***
#   log(qual_avg)  0.80870    0.03167   25.53  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3603 on 1673 degrees of freedom
# (32 observations deleted due to missingness)
# Multiple R-squared:  0.2804,	Adjusted R-squared:   0.28 
# F-statistic: 651.9 on 1 and 1673 DF,  p-value: < 2.2e-16

mdl2 <- lm(log(loy_avg) ~ log(sat_avg) + log(val_avg),
           data = csibank_no_zeros)
summary(mdl2)

# Call:
#   lm(formula = log(loy_avg) ~ log(sat_avg) + log(val_avg), data = csibank_no_zeros)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.73670 -0.07667  0.03692  0.13382  1.47053 
# 
#   Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  -0.26481    0.05000  -5.296 1.34e-07 ***
#   log(sat_avg)  0.73176    0.03215  22.763  < 2e-16 ***
#   log(val_avg)  0.38208    0.03171  12.049  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2866 on 1664 degrees of freedom
# (40 observations deleted due to missingness)
# Multiple R-squared:  0.5347,	Adjusted R-squared:  0.5342 
# F-statistic: 956.2 on 2 and 1664 DF,  p-value: < 2.2e-16

mdl3 <- lm(log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg),
           data = csibank_no_zeros)
summary(mdl3)


# Call:
#   lm(formula = log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg), 
#      data = csibank_no_zeros)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.78386 -0.07451  0.03469  0.13262  1.37408 
# 
#   Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   -0.41138    0.05733  -7.176 1.08e-12 ***
#   log(sat_avg)   0.66637    0.03438  19.382  < 2e-16 ***
#   log(val_avg)   0.33504    0.03280  10.216  < 2e-16 ***
#   log(qual_avg)  0.18253    0.03574   5.107 3.65e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2844 on 1663 degrees of freedom
# (40 observations deleted due to missingness)
# Multiple R-squared:  0.5419,	Adjusted R-squared:  0.5411 
# F-statistic: 655.8 on 3 and 1663 DF,  p-value: < 2.2e-16



mdl4 <- lm(log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + log(imag_avg),
           data = csibank_no_zeros)
summary(mdl4) # Didn;t improve much. little impact of imag_avg variable. So removing


# Call:
#   lm(formula = log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#        log(imag_avg), data = csibank_no_zeros)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.78960 -0.07645  0.03285  0.13137  1.38394 
# 
#    Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)   -0.48573    0.06900  -7.040 2.81e-12 ***
#   log(sat_avg)   0.65633    0.03474  18.891  < 2e-16 ***
#   log(val_avg)   0.31441    0.03446   9.123  < 2e-16 ***
#   log(qual_avg)  0.15813    0.03788   4.175 3.13e-05 ***
#   log(imag_avg)  0.09153    0.04736   1.933   0.0534 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2842 on 1662 degrees of freedom
# (40 observations deleted due to missingness)
# Multiple R-squared:  0.5429,	Adjusted R-squared:  0.5418 
# F-statistic: 493.6 on 4 and 1662 DF,  p-value: < 2.2e-16


mdl5 <- lm(log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + log(expe_avg),
           data = csibank_no_zeros)
summary(mdl5) # Again, expe_avg didn't improve our model much. Actually the residual standard error dropped a little

# Call:
#   lm(formula = log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#        log(expe_avg), data = csibank_no_zeros)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.78099 -0.07481  0.03369  0.13232  1.36393 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -0.44055    0.06441  -6.840 1.11e-11 ***
#   log(sat_avg)   0.66214    0.03464  19.113  < 2e-16 ***
#   log(val_avg)   0.32884    0.03338   9.850  < 2e-16 ***
#   log(qual_avg)  0.16614    0.03936   4.221 2.57e-05 ***
#   log(expe_avg)  0.04034    0.04061   0.993    0.321    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2844 on 1662 degrees of freedom
# (40 observations deleted due to missingness)
# Multiple R-squared:  0.5422,	Adjusted R-squared:  0.5411 
# F-statistic: 492.1 on 4 and 1662 DF,  p-value: < 2.2e-16

mdl6 <- lm(log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) +log(imag_avg) + log(expe_avg),
           data = csibank_no_zeros)
summary(mdl6)


# Call:
#   lm(formula = log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#        log(imag_avg) + log(expe_avg), data = csibank_no_zeros)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.1787 -0.0414  0.0957  0.2200  3.5765 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -0.68778    0.12299  -5.592 2.61e-08 ***
#   log(sat_avg)   0.45174    0.04165  10.845  < 2e-16 ***
#   log(val_avg)   0.38435    0.03941   9.754  < 2e-16 ***
#   log(qual_avg)  0.61750    0.08994   6.865 9.26e-12 ***
#   log(imag_avg) -0.46029    0.08020  -5.739 1.12e-08 ***
#   log(expe_avg)  0.27685    0.09247   2.994  0.00279 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7208 on 1701 degrees of freedom
# Multiple R-squared:  0.4984,	Adjusted R-squared:  0.497 
# F-statistic: 338.1 on 5 and 1701 DF,  p-value: < 2.2e-16


# ============  Replaced 0 with 0.01 ======================

# Seems like the impact of imag_avg changes when setting zeros with 0.01

# Call:
#   lm(formula = log(loy_avg) ~ log(sat_avg), data = csibank_no_zeros)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.4511 -0.0620  0.0594  0.1626  2.0492 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.28729    0.05926  -4.848 1.36e-06 ***
#   log(sat_avg)  1.10377    0.03003  36.753  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3787 on 1667 degrees of freedom
# (38 observations deleted due to missingness)
# Multiple R-squared:  0.4476,	Adjusted R-squared:  0.4473 
# F-statistic:  1351 on 1 and 1667 DF,  p-value: < 2.2e-16



# Call:
#   lm(formula = log(loy_avg) ~ log(sat_avg) + log(val_avg), data = csibank_no_zeros)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.3768 -0.0037  0.1233  0.2164  5.8924 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -0.33405    0.05691   -5.87 5.23e-09 ***
#   log(sat_avg)  0.60243    0.03438   17.52  < 2e-16 ***
#   log(val_avg)  0.49362    0.03555   13.89  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.743 on 1704 degrees of freedom
# Multiple R-squared:  0.4662,	Adjusted R-squared:  0.4655 
# F-statistic:   744 on 2 and 1704 DF,  p-value: < 2.2e-16



# Call:
#   lm(formula = log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg), 
#      data = csibank_no_zeros)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.2290 -0.0221  0.0981  0.2156  5.6306 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -0.90220    0.08813 -10.237   <2e-16 ***
#   log(sat_avg)   0.47214    0.03716  12.704   <2e-16 ***
#   log(val_avg)   0.34564    0.03912   8.835   <2e-16 ***
#   log(qual_avg)  0.56552    0.06790   8.329   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7285 on 1703 degrees of freedom
# Multiple R-squared:  0.4871,	Adjusted R-squared:  0.4861 
# F-statistic:   539 on 3 and 1703 DF,  p-value: < 2.2e-16


# Call:
#   lm(formula = log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#        log(imag_avg), data = csibank_no_zeros)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.1681 -0.0368  0.0988  0.2187  3.6660 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -0.52890    0.11121  -4.756 2.14e-06 ***
#   log(sat_avg)   0.50709    0.03742  13.553  < 2e-16 ***
#   log(val_avg)   0.36498    0.03896   9.368  < 2e-16 ***
#   log(qual_avg)  0.76076    0.07634   9.966  < 2e-16 ***
#   log(imag_avg) -0.43369    0.07989  -5.429 6.50e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7225 on 1702 degrees of freedom
# Multiple R-squared:  0.4958,	Adjusted R-squared:  0.4946 
# F-statistic: 418.4 on 4 and 1702 DF,  p-value: < 2.2e-16



# Call:
#   lm(formula = log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#        log(expe_avg), data = csibank_no_zeros)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.2403 -0.0259  0.0956  0.2138  5.4708 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -1.04538    0.10703  -9.767  < 2e-16 ***
#   log(sat_avg)   0.42686    0.04182  10.208  < 2e-16 ***
#   log(val_avg)   0.35996    0.03954   9.103  < 2e-16 ***
#   log(qual_avg)  0.44325    0.08545   5.187 2.39e-07 ***
#   log(expe_avg)  0.21807    0.09275   2.351   0.0188 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7276 on 1702 degrees of freedom
# Multiple R-squared:  0.4887,	Adjusted R-squared:  0.4875 
# F-statistic: 406.7 on 4 and 1702 DF,  p-value: < 2.2e-16

# Copy data of csibank
log_csibank <- csibank

# Remove zeros to not mess up log transforms
log_csibank[real_columns] <- lapply(csibank[real_columns], function(x) ifelse(x == 0, 0.01, x))

log_csibank[real_columns] <- sapply(log_csibank[real_columns], function(x) {
  if (is.numeric(x)) {
    log(x)
  } else {
    x
  }
})

# Perform one hot encoding to categorical variables
log_csibank_no_zeros <- one_hot_encode(log_csibank)
log_csibank_no_zeros <- subset(log_csibank_no_zeros, select=-c(return, recommend, sense_loyalty))


mdl_all <- lm(loy_avg ~ . ,
           data = log_csibank_no_zeros)
summary(mdl_all)

# Call:
#   lm(formula = loy_avg ~ ., data = log_csibank_no_zeros)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.2093 -0.0893  0.0627  0.1996  3.6910 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.455373   0.159135  -2.862 0.004268 ** 
#   reputation                  0.090345   0.048055   1.880 0.060277 .  
# trustworthiness             0.047582   0.040774   1.167 0.243390    
# seriousness                 0.036840   0.045644   0.807 0.419716    
# solidness                   0.039372   0.029944   1.315 0.188734    
# caring                     -0.084747   0.022632  -3.745 0.000187 ***
#   prod_services              -0.066733   0.034725  -1.922 0.054807 .  
# cust_services               0.020476   0.027417   0.747 0.455256    
# solutions                  -0.023732   0.023281  -1.019 0.308179    
# expectations_qual          -0.281757   0.052899  -5.326 1.14e-07 ***
#   reliable_services          -0.042309   0.027087  -1.562 0.118493    
# range_products              0.043108   0.029275   1.473 0.141069    
# pers_advice                -0.104007   0.036783  -2.828 0.004746 ** 
#   overall_qual                0.225125   0.037889   5.942 3.43e-09 ***
#   beneficial_services         0.115824   0.023015   5.032 5.37e-07 ***
#   valuable_investments       -0.211061   0.051982  -4.060 5.13e-05 ***
#   quality_to_price            0.029209   0.026395   1.107 0.268625    
# price_to_quality            0.063240   0.011472   5.512 4.09e-08 ***
#   fulfilled_expectations      0.090234   0.041203   2.190 0.028662 *  
#   satisf_against_other_banks  0.254882   0.050003   5.097 3.84e-07 ***
#   perf_against_ideal_bank     0.103126   0.023309   4.424 1.03e-05 ***
#   imag_avg                   -0.577832   0.123117  -4.693 2.91e-06 ***
#   expe_avg                    0.595360   0.131338   4.533 6.23e-06 ***
#   qual_avg                    0.348201   0.118058   2.949 0.003228 ** 
#   val_avg                     0.469646   0.080370   5.844 6.13e-09 ***
#   sat_avg                    -0.097261   0.085787  -1.134 0.257065    
# `Age36-45`                 -0.006368   0.045331  -0.140 0.888293    
# `Age46-55`                 -0.028832   0.046698  -0.617 0.537046    
# `Age56-65`                  0.011390   0.050192   0.227 0.820505    
# EducationUnfinished         0.121847   0.129293   0.942 0.346121    
# EducationElementary         0.206963   0.082414   2.511 0.012124 *  
#   EducationHighschool         0.200071   0.078098   2.562 0.010500 *  
#   EducationUndergrad          0.114317   0.076874   1.487 0.137187    
# EducationGraduated                NA         NA      NA       NA    
# OccupationManager           0.099561   0.070378   1.415 0.157355    
# OccupationMediumEmplo       0.030443   0.052403   0.581 0.561364    
# OccupationNotemploy         0.029505   0.063521   0.465 0.642349    
# OccupationOwnFreelan        0.035473   0.057671   0.615 0.538575    
# OccupationRetired                 NA         NA      NA       NA    
# RegionCenter                0.005936   0.041362   0.144 0.885909    
# RegionEast                  0.045375   0.040710   1.115 0.265188    
# RegionNorth                       NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6603 on 1668 degrees of freedom
# Multiple R-squared:  0.5874,	Adjusted R-squared:  0.578 
# F-statistic: 62.48 on 38 and 1668 DF,  p-value: < 2.2e-16


mdl7 <- lm(loy_avg ~ sat_avg + val_avg + qual_avg + imag_avg + expe_avg,
           data = log_csibank_no_zeros)
summary(mdl7)


# ====================================================================================


# Continue With ANOVA comparison of models. 

anova(mdl1, mdl2)

# Analysis of Variance Table
# 
# Model 1: log(loy_avg) ~ log(sat_avg)
# Model 2: log(loy_avg) ~ log(sat_avg) + log(val_avg)
# Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
# 1   1705 1047.20                                  
# 2   1704  940.74  1    106.46 192.83 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# -------------

anova(mdl2, mdl3)

# Analysis of Variance Table
# 
# Model 1: log(loy_avg) ~ log(sat_avg) + log(val_avg)
# Model 2: log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg)
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1   1704 940.74                                  
# 2   1703 903.91  1    36.824 69.378 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# -------------

anova(mdl3, mdl4)


# Analysis of Variance Table
# 
# Model 1: log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg)
# Model 2: log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#   log(imag_avg)
# Res.Df    RSS Df Sum of Sq      F  Pr(>F)    
# 1   1703 903.91                                
# 2   1702 888.53  1    15.384 29.469 6.5e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# -------------

anova(mdl4, mdl5)

# Analysis of Variance Table
# 
# Model 1: log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#   log(imag_avg)
# Model 2: log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#   log(expe_avg)
# Res.Df    RSS Df Sum of Sq F Pr(>F)
# 1   1702 888.53                      
# 2   1702 900.99  0   -12.458 


anova(mdl4, mdl6)

# Analysis of Variance Table
# 
# Model 1: log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#   log(imag_avg)
# Model 2: log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#   log(imag_avg) + log(expe_avg)
# Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
# 1   1702 888.53                                
# 2   1701 883.87  1    4.6583 8.9649 0.002792 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



anova(mdl5, mdl6)

# Analysis of Variance Table
# 
# Model 1: log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#   log(expe_avg)
# Model 2: log(loy_avg) ~ log(sat_avg) + log(val_avg) + log(qual_avg) + 
#   log(imag_avg) + log(expe_avg)
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1   1702 900.99                                  
# 2   1701 883.87  1    17.117 32.941 1.123e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Best model is model6 


# Model 7 is the same with model 6 but with transformed dataset

anova(mdl_all, mdl7)

# Analysis of Variance Table
# 
# Model 1: loy_avg ~ reputation + trustworthiness + seriousness + solidness + 
#   caring + prod_services + cust_services + solutions + expectations_qual + 
#   reliable_services + range_products + pers_advice + overall_qual + 
#   beneficial_services + valuable_investments + quality_to_price + 
#   price_to_quality + fulfilled_expectations + satisf_against_other_banks + 
#   perf_against_ideal_bank + imag_avg + expe_avg + qual_avg + 
#   val_avg + sat_avg + `Age36-45` + `Age46-55` + `Age56-65` + 
#   EducationUnfinished + EducationElementary + EducationHighschool + 
#   EducationUndergrad + EducationGraduated + OccupationManager + 
#   OccupationMediumEmplo + OccupationNotemploy + OccupationOwnFreelan + 
#   OccupationRetired + RegionCenter + RegionEast + RegionNorth
# Model 2: loy_avg ~ sat_avg + val_avg + qual_avg + imag_avg + expe_avg
# Res.Df    RSS  Df Sum of Sq      F    Pr(>F)    
# 1   1668 727.14                                   
# 2   1701 883.87 -33   -156.73 10.895 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# ====================================================================================


# Perform training of model with 
#   - Intercept
#   - All predictors
#   - Forward Selection
#   - Backward selection

InterceptOnly <- lm(loy_avg~1, data=log_csibank_no_zeros)

Forward <- step(InterceptOnly, direction="forward", scope=formula(mdl_all), trace=0)

# Step Df    Deviance Resid. Df Resid. Dev         AIC
# 1                               NA          NA      1706  1762.1981    56.32443
# 2                     + sat_avg -1 715.0018293      1705  1047.1963  -830.07618
# 3                     + val_avg -1 106.4579224      1704   940.7383 -1011.07786
# 4                + overall_qual -1  52.1636879      1703   888.5747 -1106.45602
# 5            + price_to_quality -1  40.5325599      1702   848.0421 -1184.15310
# 6         + beneficial_services -1  19.3496888      1701   828.6924 -1221.55276
# 7                      + caring -1  17.9457564      1700   810.7466 -1256.92485
# 8        + valuable_investments -1  15.6404230      1699   795.1062 -1288.17702
# 9     + perf_against_ideal_bank -1  14.8366103      1698   780.2696 -1318.33042
# 10 + satisf_against_other_banks -1  10.2847200      1697   769.9849 -1338.97996
# 11     + fulfilled_expectations -1   6.0393734      1696   763.9455 -1350.42159
# 12                   + expe_avg -1   2.7194047      1695   761.2261 -1354.50881
# 13          + expectations_qual -1   7.2463622      1694   753.9798 -1368.83614
# 14                   + imag_avg -1   6.5866725      1693   747.3931 -1381.81380
# 15                   + qual_avg -1   3.2579767      1692   744.1351 -1387.27108
# 16                + pers_advice -1   2.6195845      1691   741.5155 -1391.29085
# 17         + EducationGraduated -1   1.8650029      1690   739.6505 -1393.58957
# 18         + EducationUndergrad -1   2.2947216      1689   737.3558 -1396.89367
# 19                 + reputation -1   1.9752882      1688   735.3805 -1399.47265
# 20                  + solidness -1   1.5733485      1687   733.8072 -1401.12870
# 21              + prod_services -1   0.9859715      1686   732.8212 -1401.42383
# 22             + range_products -1   0.9398779      1685   731.8813 -1401.61454


# Backward <- step(mdl_all, direction="backward", trace=0)
# 
# Step Df    Deviance Resid. Df Resid. Dev       AIC
# 1                          NA          NA      1668   727.1402 -1378.708
# 2            - RegionNorth  0 0.000000000      1668   727.1402 -1378.708
# 3      - OccupationRetired  0 0.000000000      1668   727.1402 -1378.708
# 4     - EducationGraduated  0 0.000000000      1668   727.1402 -1378.708
# 5             - `Age36-45`  1 0.008603723      1669   727.1488 -1380.688
# 6           - RegionCenter  1 0.009084802      1670   727.1579 -1382.667
# 7             - `Age56-65`  1 0.039738386      1671   727.1977 -1384.574
# 8    - OccupationNotemploy  1 0.088188965      1672   727.2859 -1386.367
# 9  - OccupationMediumEmplo  1 0.051901803      1673   727.3378 -1388.245
# 10  - OccupationOwnFreelan  1 0.033233986      1674   727.3710 -1390.167
# 11            - `Age46-55`  1 0.158234433      1675   727.5292 -1391.795
# 12         - cust_services  1 0.232454944      1676   727.7617 -1393.250
# 13           - seriousness  1 0.283464863      1677   728.0451 -1394.585
# 14             - solutions  1 0.333334217      1678   728.3785 -1395.804
# 15   - EducationUnfinished  1 0.334067143      1679   728.7125 -1397.021
# 16      - quality_to_price  1 0.464268576      1680   729.1768 -1397.934
# 17    - EducationUndergrad  1 0.590512023      1681   729.7673 -1398.552
# 18       - trustworthiness  1 0.633099566      1682   730.4004 -1399.072
# 19               - sat_avg  1 0.585281905      1683   730.9857 -1399.705
# 20            - RegionEast  1 0.691619951      1684   731.6773 -1400.090
# 21     - OccupationManager  1 0.721227295      1685   732.3986 -1400.409
# 22     - reliable_services  1 0.753524899      1686   733.1521 -1400.653
# 23        - range_products  1 0.826434734      1687   733.9785 -1400.730

# ====================================================================================

# Approach

# From previous experiments we've seen that feature constructs have statistically 
# significant impact on both previous models and in the latest ones (model with all feature anova,
# forward selection, backward selection)


# Features to use 

# caring, expectations_qual, pers_advice, overall_qual, beneficial_services, valuable_investments,
# price_to_quality, satisf_against_other_banks, perf_against_ideal_bank, imag_avg, expe_avg, qual_avg, 
# val_avg, sat_avg, reputation, fulfilled_expectations, EducationGraduated, EducationUndergrad

# ====================================================================================

# Prediction Performance 
# install.packages("caret")
library(caret)


calculate_error_metrics <- function(model, dataset, actuals) {
  # Predictions
  predictions <- exp(predict(model, newdata = dataset))
  
  # Residuals
  residuals <- actuals - predictions
  
  # Mean Error (ME)
  ME <- mean(residuals)
  
  # Median Error (MdE)
  MdE <- median(residuals)
  
  # Mean Absolute Error (MAE)
  MAE <- mean(abs(residuals))
  
  # Median Absolute Error (MdAE)
  MdAE <- median(abs(residuals))
  
  # Mean Magnitude of Relative Error (MMRE)
  MMRE <- mean(abs(residuals) / actuals)
  
  # Median Relative Absolute Error (MdRE)
  MdRE <- median(abs(residuals) / actuals)
  
  # Mean Magnitude of Relative Error Residuals (MMER)
  MMER <- mean(abs(residuals)/ predictions)
  
  # Median Relative Absolute Error Residuals (MdMER)
  MdMER <- median(abs(residuals)/ predictions)
  
  # Store metrics in a data frame
  error_metrics <- data.frame(
    ME = ME,
    MdE = MdE,
    MAE = MAE,
    MdAE = MdAE,
    MMRE = MMRE,
    MdRE = MdRE,
    MMER = MMER,
    MdMER = MdMER
  )
  
  return(error_metrics)
}


# Perform selection of features
log_csibank_no_zeros_dataset <- subset(log_csibank_no_zeros, select=c(caring, expectations_qual, pers_advice, overall_qual, beneficial_services, valuable_investments,
                                                                      price_to_quality, satisf_against_other_banks, perf_against_ideal_bank, imag_avg, expe_avg, qual_avg, 
                                                                      val_avg, sat_avg, reputation, fulfilled_expectations, EducationGraduated, EducationUndergrad, loy_avg))

# Perform splitting 
trainIndices <- createDataPartition(log_csibank_no_zeros_dataset$loy_avg, p=0.8,
                                 list=FALSE,
                                 times=1)

# Assign train and test set
train_set <- log_csibank_no_zeros_dataset[trainIndices,]
test_set <- log_csibank_no_zeros_dataset[-trainIndices,]

# ---------------------------------------------------------------

# Linear Model training
pre_model <- lm(loy_avg ~ .,
           data = train_set)
summary(pre_model)

# Extracting actuals by reverting logarithm transform
actuals <- exp(test_set$loy_avg)

# Print Metric results
print(calculate_error_metrics(pre_model, test_set, actuals))

# ME       MdE     MAE     MdAE     MMRE      MdRE      MMER     MdMER
# 1 0.3798971 0.4857549 1.48552 1.031786 1.021104 0.1593621 0.3684743 0.1707751


# ---------------------------------------------------------------

install.packages("randomForest")
library(randomForest)

rf <- randomForest(loy_avg ~ . , data = train_set)

# Print Metric results
print(calculate_error_metrics(rf, test_set, actuals))

# ME      MdE      MAE      MdAE      MMRE      MdRE      MMER     MdMER
# 1 0.3661466 0.384315 1.123143 0.8063675 0.6786427 0.1192141 0.4128198 0.1283086



compare_models <- function(model1, model2, dataset, actuals, model_names = c("Model1", "Model2")) {
  # Predictions for both models
  predictions1 <- predict(model1, newdata = dataset)
  residuals1 <- actuals - predictions1
  
  predictions2 <- predict(model2, newdata = dataset)
  residuals2 <- actuals - predictions2
  
  # Error metrics for both models
  error_metrics1 <- calculate_error_metrics(model1, dataset, actuals)
  error_metrics2 <- calculate_error_metrics(model2, dataset, actuals)
  
  # Combine residuals for boxplot
  combined_residuals <- data.frame(
    Model1 = residuals1,
    Model2 = residuals2
  )
  
  # Combine error metrics for boxplot
  combined_error_metrics <- rbind(error_metrics1, error_metrics2)
  combined_error_metrics$model <- factor(rep(model_names, each = nrow(error_metrics1)))
  
  # Boxplot with user-specified model names
  boxplot(combined_residuals, 
          main = "Comparison of Residuals",
          ylab = "Residuals",
          col = c("skyblue", "lightcoral"),
          names = model_names)
  
  # Perform Wilcoxon signed-rank test
  wilcox_test_result <- wilcox.test(residuals1, residuals2, paired = TRUE)
  
  # Display test result
  cat("\nWilcoxon Test Result:\n")
  print(wilcox_test_result)
  
  return(combined_error_metrics)
}


# Usage
comparison_result <- compare_models(pre_model, rf, test_set, actuals, model_names = c("Pre-Model", "Random Forest"))
















# ==============================================================================================

# Testing with all data



# Perform splitting 
trainIndices <- createDataPartition(log_csibank_no_zeros$loy_avg, p=0.8,
                                    list=FALSE,
                                    times=1)

# Assign train and test set
train_set <- log_csibank_no_zeros_dataset[trainIndices,]
test_set <- log_csibank_no_zeros_dataset[-trainIndices,]

# ---------------------------------------------------------------

# Linear Model training
pre_model <- lm(loy_avg ~ .,
                data = train_set)
summary(pre_model)

# Extracting actuals by reverting logarithm transform
actuals <- exp(test_set$loy_avg)

# Print Metric results
print(calculate_error_metrics(pre_model, test_set, actuals))


rf <- randomForest(loy_avg ~ . , data = train_set)

# Print Metric results
print(calculate_error_metrics(rf, test_set, actuals))


# > # Print Metric results
#   > print(calculate_error_metrics(pre_model, test_set, actuals))
# ME       MdE      MAE     MdAE     MMRE      MdRE      MMER     MdMER
# 1 0.4300091 0.5307538 1.513298 1.102385 1.231037 0.1575461 0.4274203 0.1766523
# > rf <- randomForest(loy_avg ~ . , data = train_set)
# > 
#   > # Print Metric results
#   > print(calculate_error_metrics(rf, test_set, actuals))
# ME       MdE      MAE      MdAE     MMRE      MdRE      MMER     MdMER
# 1 0.619965 0.6217744 1.213706 0.8839178 1.004207 0.1198775 0.4214688 0.1312332



# library(MASS)
# model <- polr(Satisfaction ~ Predictor1 + Predictor2, data = your_data)
# 
# 
# library(MASS)
# model <- polr(Satisfaction ~ Predictor1 + Predictor2, data = your_data, Hess=TRUE)
# 
# # Random Forest
# library(randomForest)
# model <- randomForest(Satisfaction ~ Predictor1 + Predictor2, data = your_data)
# 
# # Gradient Boosted Trees
# library(gbm)
# model <- gbm(Satisfaction ~ Predictor1 + Predictor2, data = your_data, distribution = "ordinal")
# 
# 
# library(nnet)
# model <- multinom(Satisfaction ~ Predictor1 + Predictor2, data = your_data)
# 
# library(brms)
# model <- brm(Satisfaction ~ Predictor1 + Predictor2, data = your_data, family = cumulative())
# 
# 
# library(lavaan)
# model <- sem("Latent =~ Indicator1 + Indicator2", data = your_data)
# 
# 
# library(gamlss)
# model <- gamlss(Satisfaction ~ Predictor1 + Predictor2, data = your_data, family = "Categorical")
