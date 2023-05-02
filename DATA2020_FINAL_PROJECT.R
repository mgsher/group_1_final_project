


### Read in Data:

data <- read.csv("documents/sn_economicdata1981-2020.csv")


income_data <- read.csv("documents/state_per_capita_income_1980_2020.csv")

# Merge Data:

# Extract the last 4 characters and convert to integer
income_data$Year <- as.integer(substr(income_data$DATE, nchar(income_data$DATE) - 3, nchar(income_data$DATE)))

# Assuming your dataframe is named "df" and the column with year values is "Year"
income_data <- income_data[income_data$Year >= 1981,]
income_data <- subset(income_data, select = -c(DATE))

# Replace periods with spaces in all column names
colnames(income_data) <- gsub("\\.", " ", colnames(income_data))


# Pivot income_data to a long format, with State.Province as the key
income_data_long <- income_data %>%
  pivot_longer(cols = -Year, names_to = "State.Province", values_to = "IncomePerCapita")

# Merge data and income_data_long by Year and State.Province
data <- left_join(data, income_data_long, by = c("Year", "State.Province"))


# EDA:

## check for percentage of nans per column:

# Calculate the percentage of NaNs per column
percentage_of_nans <- colMeans(is.na(data)) * 100

# Print the percentage of NaNs per column
print(percentage_of_nans)

# get a list of values with 100% nans:
columns_with_100_percent_nans <- names(percentage_of_nans[percentage_of_nans == 100])

# Print the list of columns with 100% NaNs
print(columns_with_100_percent_nans)

# Point 1: From the above print statement, columns "Government.enterprises.and.investment", 
# "Top.Marginal.Income.Tax.Rate", "Top.Marginal.Income.Payroll.tax.Rate", "Hiring.regulations.and.minimum.wage",
# "Labor.Market.Freedom", "Credit.Market.Regulation", and "Business.Regulations" all have 100% nans, so we remove them.

# Columns "Economic.Freedom.Summary.Index", "Rank", "Quantile", "General.Consumption.Expenditure", 
# "Transfers.and.subsidies", "Insurance.and.Retirement.Payments", "Government.Spending", 
# "Income.and.Payroll.Tax.Revenue", "world.adjusted", "Property.Tax.and.Other.Taxes", "Sales.Tax.Revenue",
# "Taxes", "Minimum.Wage.Legislation", "Government.Employment", "Union.Density", "Labor.Market.Freedom.1",
# all had about 2% NaNs, we will use MICE imputation to fill in NaNs.

# exclude columns with 100% NaNs from data:
data <- data[ , !(colnames(data) %in% columns_with_100_percent_nans)]

# Point 2: we exclude columns called "Country", "ISO_Code", "Rank", and "Quantile", as they are not of interest.
# Country is USA for all columns, ISO_CODE is a short name for State.Province (ie Alaska becomes US-AK), 
# Rank and Quantile are related to the ranking of the state and its economic freedom index

# misc cols to drop
misc_columns_to_exclude <- c("Country", "ISO_Code", "Rank", "Quantile")
data <- data[, !(colnames(data) %in% misc_columns_to_exclude)]

# If you decide to remove some variables from your analysis, explain why you did that 
# (see Point 1 and Point 2 above)

# impute missing values with MICE:

### install.packages("mice")
library(mice)
library(ggplot2)

imputed_data <- mice(data)
data_complete <- complete(imputed_data)

# Explore the distributions and the symmetry
targets_name = c("Economic.Freedom.Summary.Index", "IncomePerCapita")
# loop over all columns in the dataset
for (col in targets_name) {
  # create a histogram of the column values
  hist(data_complete[[col]], main = col, xlab = "")
  
  # create a density plot of the column values
  ggplot(data = data_complete, aes(x = data_complete[[col]])) +
    geom_density() +
    ggtitle(col)
}

# Install the e1071 package
# install.packages("e1071")

# Load the e1071 package
library(e1071)

# get only numeric cols:
numeric_data_for_skew <- data_complete[sapply(data_complete, is.numeric)]

# Calculate skewness for each numeric column
column_skewness <- sapply(numeric_data_for_skew, skewness)

# Print the skewness of each column
print(column_skewness)

################################# CLUSTERING ###################################
# first, we will cluster to see how many clusters of similar states there are. 
# I cluster by collapsing our data set into one with 50 rows (1 for each state) and a 
# column for each year-variable combination to get variation across years for each variable.
# I divide all values by 10, just to get on a 1 point scale, as all values are already 
# normalized to 10 and comparable across states (this is min-max scaling).

# I found that 2 is the best with notable results like how both Florida and 
# Texas are both put in cluster 2 and how both New York and California are 
# together in cluster 1. 

library(cluster) 
library(factoextra)
library(tidyverse)

# Gather all variables into a long format, except State and Year columns
data_w_out_income <- subset(data_complete, select = -c(IncomePerCapita))
long_data <- data_w_out_income %>%
  gather(key = "variable", value = "value", -State.Province, -Year)

# Create a new column with the variable-year combination
long_data <- long_data %>%
  mutate(variable_year = paste(variable, Year, sep = "_")) %>%
  select(-Year, -variable)

# Spread the data to have columns for each variable-year combination
wide_data <- long_data %>%
  spread(key = variable_year, value = value)

state_data <- wide_data %>%
  rename_with(~ "State", .cols = 1)

# set row names to the states
rownames(state_data) <- state_data$State

state_data <- state_data %>%
  select(-1) # drop state var

state_data <- state_data %>%
  mutate_all(~ . / 10) # divide all values by 10 to get on 1 point scale

# Calculate the gap statistic
set.seed(123)  # for reproducibility
gap_stat <- clusGap(state_data, FUN = kmeans, K.max = 10, B = 50)

# Visualize the gap statistic
fviz_gap_stat(gap_stat)

# Determine the optimal number of clusters
optimal_clusters <- which.max(gap_stat$Tab[, "gap"])

# Apply k-means clustering with the optimal number of clusters
kmeans_result <- kmeans(state_data, centers = 2, nstart = 50)

# Visualize the clustering
fviz_cluster(kmeans_result, data = state_data, stand = FALSE,
             geom = "text", labelsize = 8, repel = TRUE)


# Find means of Economic Freedom of the 2 clusters:

# Add cluster assignments to the original data_complete data frame
data_complete$Cluster <- kmeans_result$cluster[match(data_complete$State.Province, rownames(state_data))]

# Calculate the mean of Economic.Freedom.Summary.Index for each cluster
cluster_means <- data_complete %>%
  group_by(Cluster) %>%
  summarise(Mean_Economic_Freedom = mean(Economic.Freedom.Summary.Index, na.rm = TRUE))

# Display the results
print(cluster_means)

# Calculate the mean of IncomePerCapita for each cluster
cluster_means2 <- data_complete %>%
  group_by(Cluster) %>%
  summarise(Mean_Income = mean(IncomePerCapita, na.rm = TRUE))

# Display the results
print(cluster_means2)

################################################################################


################################# QQ Plots #####################################

library(car)

# Get distributions for all Rhode Island Variables:

# Subset your data to only include Rhode Island and variables of interest
rhode_island_data <- data_complete[data_complete$State.Province == "Rhode Island", !(names(data_complete) %in% c("Year", "State.Province", "Cluster"))]

# Set up a plot layout based on the number of variables
num_vars <- ncol(rhode_island_data)
num_cols <- 4
num_rows <- ceiling(num_vars / num_cols)
par(mfrow = c(num_rows, num_cols), mar = c(4, 4, 1, 1))

# Loop through each variable and create a QQ plot
for (i in 1:num_vars) {
  qqPlot(rhode_island_data[[i]], main = names(rhode_island_data)[i])
}


# Now get Distributions for all Data in Cluster 1:

# Subset your data to only include Cluster 1 and variables of interest
cluster_data <- data_complete[data_complete$Cluster == 1, !(names(data_complete) %in% c("Year", "State.Province", "Cluster"))]

# Set up a plot layout based on the number of variables
num_vars <- ncol(cluster_data)
num_cols <- 4
num_rows <- ceiling(num_vars / num_cols)
par(mfrow = c(num_rows, num_cols), mar = c(4, 4, 1, 1))

# Loop through each variable and create a QQ plot
for (i in 1:num_vars) {
  qqPlot(cluster_data[[i]], main = names(cluster_data)[i])
}


# Now get Distributions for all Data in Cluster 2:

# Subset your data to only include Cluster 1 and variables of interest
cluster_data <- data_complete[data_complete$Cluster == 2, !(names(data_complete) %in% c("Year", "State.Province", "Cluster"))]

# Set up a plot layout based on the number of variables
num_vars <- ncol(cluster_data)
num_cols <- 4
num_rows <- ceiling(num_vars / num_cols)
par(mfrow = c(num_rows, num_cols), mar = c(4, 4, 1, 1))

# Loop through each variable and create a QQ plot
for (i in 1:num_vars) {
  qqPlot(cluster_data[[i]], main = names(cluster_data)[i])
}


############################ Linear Regression #################################

# The question I want to ask is what variables are significant in predicting 
# Economic Freedom and whether we can make suggestions to Rhode Island's Policy
# Based on its 2020 numbers for our covariates.

# Get a list of unique years sorted in ascending order
unique_years <- sort(unique(data_complete$Year))

# Split the years into training (first 70%) and testing (last 30%) sets
train_years <- unique_years[1:floor(0.7 * length(unique_years))]
test_years <- unique_years[(floor(0.7 * length(unique_years)) + 1):length(unique_years)]

# Split the data into training and testing sets based on years
train_data <- data_complete %>% filter(Year %in% train_years)
test_data <- data_complete %>% filter(Year %in% test_years)

# Identify the columns to scale (exclude the "State" variable)
cols_to_scale <- colnames(data_complete) # Get column names from the data frame
cols_to_scale <- cols_to_scale[cols_to_scale != "State.Province"]

# Calculate the min and max values for each column in train_data
min_values <- apply(train_data[, cols_to_scale], 2, min)
max_values <- apply(train_data[, cols_to_scale], 2, max)

# Apply min-max scaling to train_data (excluding the "State" variable)
train_data_scaled <- as.data.frame(scale(train_data[, cols_to_scale], center = min_values, scale = max_values - min_values))
train_data_scaled$State <- train_data$State.Province

# Apply the same min-max scaling to test_data (excluding the "State" variable)
test_data_scaled <- as.data.frame(scale(test_data[, cols_to_scale], center = min_values, scale = max_values - min_values))
test_data_scaled$State <- test_data$State.Province

############################ Baseline Model (Average) #################################
# install.packages('zoo')
# library(zoo)

# rolling_avg <- rollmean(x, k = 5, fill = NA)
# baseline_df <- df %>% 
#   mutate(rolling_avg = rollmean(y, k = 3, fill = NA))

baseline_preds = mean(train_data_scaled$IncomePerCapita)

#mse <- mean((test_data_scaled$IncomePerCapita - train_svg)^2)
cluster1_states = data_complete[data_complete$Cluster == 1, ][2]
cluster1_states = unique(cluster1_states)

cluster2_states = data_complete[data_complete$Cluster == 2, ][2]
cluster2_states = unique(cluster2_states)

mse_values_c1 <- numeric()

for (i in cluster1_states$State.Province){
  state_i_test = test_data_scaled[test_data_scaled$State == i, ]
  
  # calculate MSE and store the value
  mse_c1 <- mean((state_i_test$IncomePerCapita - baseline_preds)^2)
  mse_values_c1 <- c(mse_values_c1, mse_c1)
}

baseline_mean_mse_by_state_c1 = mean(mse_values_c1)

mse_values_c2 <- numeric()

for (i in cluster2_states$State.Province){
  state_i_test = test_data_scaled[test_data_scaled$State == i, ]
  
  # calculate MSE and store the value
  mse_c2 <- mean((state_i_test$IncomePerCapita - baseline_preds)^2)
  mse_values_c2 <- c(mse_values_c2, mse_c2)
}

baseline_mean_mse_by_state_c2 = mean(mse_values_c2)

RI_test = test_data_scaled[test_data_scaled$State == 'Rhode Island', ]
RI_mse_baseline <- mean((RI_test$IncomePerCapita - baseline_preds)^2)

years = unique(test_data_scaled$Year)

year_mse_values <- numeric()

for (i in years){
  year_i_test = test_data_scaled[test_data_scaled$Year == i, ]
  
  # calculate MSE and store the value
  mse <- mean((year_i_test$IncomePerCapita - baseline_preds)^2)
  year_mse_values <- c(year_mse_values, mse)
}

mean_mse_year = mean(year_mse_values)

############################ Baseline Model Ends (Rolling Average) #################################




# --------- Step Backward ---------
# Fit the initial model with all features
complete_lm_model <- lm(IncomePerCapita ~ 
                          Economic.Freedom.Summary.Index + 
                          General.Consumption.Expenditure + Transfers.and.subsidies + 
                          Insurance.and.Retirement.Payments + Government.Spending + 
                          Income.and.Payroll.Tax.Revenue + world.adjusted + 
                          Property.Tax.and.Other.Taxes + Sales.Tax.Revenue + Taxes + 
                          Minimum.Wage.Legislation + Government.Employment + 
                          Union.Density + Labor.Market.Freedom.1,
                        data = train_data_scaled)

# Use the step function to perform backward selection
final_lm_model <- step(complete_lm_model, direction = "backward")

# Print the final model summary
summary(final_lm_model)

# ---------------------------------

# Fit the linear model
lm_model <- lm(IncomePerCapita ~ 
                  Economic.Freedom.Summary.Index + 
                  General.Consumption.Expenditure + Transfers.and.subsidies + 
                  Insurance.and.Retirement.Payments + Government.Spending + 
                  Income.and.Payroll.Tax.Revenue + world.adjusted + 
                  Property.Tax.and.Other.Taxes + Sales.Tax.Revenue + Taxes + 
                  Minimum.Wage.Legislation + Government.Employment + 
                  Union.Density + Labor.Market.Freedom.1,
               data = train_data_scaled)

summary(lm_model)

# Linear regression with Square root Transformation

lm_model_logged <- lm(IncomePerCapita ~ 
                 Economic.Freedom.Summary.Index + 
                 log(General.Consumption.Expenditure+1) + Transfers.and.subsidies + 
                 Insurance.and.Retirement.Payments + Government.Spending + 
                 Income.and.Payroll.Tax.Revenue + world.adjusted + 
                 Property.Tax.and.Other.Taxes + Sales.Tax.Revenue + Taxes + 
                 Minimum.Wage.Legislation + Government.Employment + 
                 Union.Density + Labor.Market.Freedom.1,
               data = train_data_scaled)
summary(lm_model_logged)

library(ggplot2)

# Plot Train Preds

# Add predictions to the train data
train_data_scaled$Predicted <- predict(lm_model, newdata = train_data_scaled)

# Create a ggplot2 scatter plot with actual vs. predicted values
p <- ggplot(train_data_scaled, aes(x = IncomePerCapita, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid") +
  labs(title = "Linear Model Predictions vs. Actual Values (Train Data)",
       x = "Actual Income Per Capita",
       y = "Predicted Income Per Capita") +
  theme_minimal() + 
  theme(text = element_text(size = 15)) +
  xlim(0, 1) +
  ylim(0, 0.8)

# Display the plot
print(p)

# Plot Test Preds

# Add predictions to the test data
test_data_scaled$Predicted <- predict(lm_model, newdata = test_data_scaled)

# Create a ggplot2 scatter plot with actual vs. predicted values
p <- ggplot(test_data_scaled, aes(x = IncomePerCapita, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "solid") +
  labs(title = "Linear Model Predictions vs. Actual Values (Test Data)",
       x = "Actual Income Per Capita",
       y = "Predicted Income Per Capita") +
  theme_minimal() + 
  theme(text = element_text(size = 15)) + 
  ylim(0, 1)

# Display the plot
print(p)

# ---------- calculate evaluation metric on the test set for rhode island,
# ---------- and compare it to the average of all states in cluster 1
cluster1_states = data_complete[data_complete$Cluster == 1, ][2]
cluster1_states = unique(cluster1_states)

mse_values <- numeric()

for (i in cluster1_states$State.Province){
  state_i_test = test_data_scaled[test_data_scaled$State == i, ]

  # calculate MSE and store the value
  mse <- mean((state_i_test$IncomePerCapita - state_i_test$Predicted)^2)
  mse_values <- c(mse_values, mse)
}

mean_mse_c1 = mean(mse_values)

cluster2_states = data_complete[data_complete$Cluster == 2, ][2]
cluster2_states = unique(cluster2_states)

mse_values_c2 <- numeric()

for (i in cluster2_states$State.Province){
  state_i_test = test_data_scaled[test_data_scaled$State == i, ]
  
  # calculate MSE and store the value
  mse <- mean((state_i_test$IncomePerCapita - state_i_test$Predicted)^2)
  mse_values_c2 <- c(mse_values_c2, mse)
}

mse_values_c2 = mean(mse_values_c2)

RI_test = test_data_scaled[test_data_scaled$State == 'Rhode Island', ]
RI_mse <- mean((RI_test$IncomePerCapita - RI_test$Predicted)^2)


# measure regression power for different years
# ---------- calculate evaluation metric on the test set for rhode island,
# ---------- and compare it to the average of all states in cluster 1
years = unique(test_data_scaled$Year)

year_mse_values <- numeric()

for (i in years){
  year_i_test = test_data_scaled[test_data_scaled$Year == i, ]
  
  # calculate MSE and store the value
  mse <- mean((year_i_test$IncomePerCapita - year_i_test$Predicted)^2)
  year_mse_values <- c(year_mse_values, mse)
}

mean_mse_year = mean(year_mse_values)


# Loop through each variable and create a QQ plot


## Rhode Island



for (i in 1:num_vars) {
  qqPlot(cluster_data[[i]], main = names(cluster_data)[i])
}



# check for normality of residuals:

library(ggplot2)

# Get the residuals from the linear model
lm_residuals <- resid(final_lm_model)

# Create a data frame with the residuals
plot_data <- data.frame(residuals = lm_residuals)

# Create a histogram of the residuals with 20 bins
ggplot(plot_data, aes(x = residuals)) +
  geom_histogram(bins = 150, color = "black", fill = "white") +
  labs(x = "Residuals", y = "Count", 
       title = "Distribution of Residuals")

# Create a data frame with the residuals
plot_data <- data.frame(residuals = lm_residuals)

# Create a QQ plot of the residuals
ggplot(plot_data, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot of Residuals")


# Check residuals not correlated to covariates:

library(ggplot2)
library(gridExtra)

# Get the fitted values and residuals from the linear model
lm_fitted <- fitted(lm_model_logged)
lm_residuals <- resid(lm_model_logged)

# Create a data frame with fitted values, residuals, and covariates
plot_data <- data.frame(lm_fitted, lm_residuals, train_data[, c("Economic.Freedom.Summary.Index", "General.Consumption.Expenditure", "Transfers.and.subsidies", "Insurance.and.Retirement.Payments", "Income.and.Payroll.Tax.Revenue", "world.adjusted", "Property.Tax.and.Other.Taxes", "Sales.Tax.Revenue",  "Government.Employment", "Union.Density", "Labor.Market.Freedom.1")])

# Create a list of plots for each covariate
plot_list <- list()
for (col in colnames(plot_data[, 3:ncol(plot_data)])) {
  p <- ggplot(plot_data, aes_string(x = col, y = lm_residuals)) +
    geom_point() +
    labs(x = col, y = "Residuals", 
         title = paste("Residuals vs", col))+ 
      theme(text = element_text(size = 6))
  plot_list[[col]] <- p
}

# Combine the plots into a grid
grid.arrange(grobs = plot_list, ncol = 3)

# Check Homoskedasticity:

# Get the fitted values and residuals from the linear model
lm_fitted <- fitted(final_lm_model)
lm_residuals <- resid(final_lm_model)

# Create a data frame with fitted values and residuals
plot_data <- data.frame(lm_fitted, lm_residuals)

# Create the plot using ggplot2
ggplot(plot_data, aes(x = lm_fitted, y = lm_residuals)) +
  geom_point() +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Residuals vs Fitted Values Plot (Checking for Homoskedasticity)")

###################### FIXED AND RANDOM EFFECTS MODEL ##########################

library(lme4)

# Create a model formula
# Add fixed effects for each variable, and random effects for State and Year
# model_formula <- Economic.Freedom.Summary.Index ~ 
#  General.Consumption.Expenditure + Transfers.and.subsidies + 
#  Insurance.and.Retirement.Payments + Government.Spending + 
#  Income.and.Payroll.Tax.Revenue + world.adjusted + 
#  Property.Tax.and.Other.Taxes + Sales.Tax.Revenue + Taxes + 
#  Minimum.Wage.Legislation + Government.Employment + Union.Density + 
#  Labor.Market.Freedom.1 + (1 | State.Province) + (1 | Year)

# Fit the linear mixed-effects models:

# Random Intercept Model:
random_intercept_model <- lmer(IncomePerCapita ~ Economic.Freedom.Summary.Index +
                                 General.Consumption.Expenditure + Taxes +
                                 Minimum.Wage.Legislation + Government.Employment +
                                 Union.Density + Labor.Market.Freedom.1 +
                                 (1 | State.Province), data = data_complete)

# pint summary
summary(random_intercept_model)

# Random Intercept and Variable Model:
random_intercept_and_var_model <- lmer(IncomePerCapita ~ Economic.Freedom.Summary.Index +
                                         General.Consumption.Expenditure + Taxes +
                                         Minimum.Wage.Legislation + Government.Employment +
                                         Union.Density + Labor.Market.Freedom.1 +
                                         (1 + Economic.Freedom.Summary.Index +
                                            General.Consumption.Expenditure + Taxes +
                                            Minimum.Wage.Legislation + Government.Employment +
                                            Union.Density +
                                            Labor.Market.Freedom.1 | State.Province),
                                       data = data_complete)

summary(random_intercept_and_var_model)







# Explain why you will use the methods in the next section.

# Describe the most important/interesting aspects of the preprocessing and the EDA briefly.


# ---------- Evaluating models ------------


