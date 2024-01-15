
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
csibank_no_zeros[real_columns] <- lapply(csibank[real_columns], function(x) ifelse(x == 0, NA, x))

# Perform one hot encoding to categorical variables
csibank_no_zeros <- one_hot_encode(csibank_no_zeros)


# Approach -> Will add variables (constructs only) that most correlate with the dependent variable


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


# ====================================================================================



# Continue With ANOVA comparison of models. 




# Perform training of model with 
#   - Intercept
#   - All predictors
#   - Forward Selection
#   - Backward selection



# ====================================================================================

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
