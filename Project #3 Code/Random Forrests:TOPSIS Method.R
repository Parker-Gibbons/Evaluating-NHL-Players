library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting
library(doParallel)  # for parallel backend to foreach
library(foreach)     # for parallel processing with for loops
library(caret)       # for general model fitting
library(rpart)       # for fitting decision trees
library(ipred)       # for fitting bagged decision trees
library(ranger)
library(tidyverse)
library(Metrics)
library(ranger)
library(randomForest)
library(cluster)
library(rpart)
library(rpart.plot)



RF_model <- randomForest(gameScore ~ games_played + season + team + position + icetime + I_F_xGoals + I_F_xRebounds + Assists + I_F_points + I_F_goals + I_F_rebounds + I_F_hits + I_F_takeaways + I_F_giveaways + lowDangerGoaAbvExp + medDangerGoaAbvExp + highDangerGoaAbvExp + faceoffsWon + penalityMinutes + penalityMinutesDrawn + shotsBlockedByPlayer + subsetclust1, data = CLUSTEREDfor_3_, ntree = 100, mtry = 4, seed = 123)
print(RF_model) 
library(randomForest)
library(dplyr)






library(randomForest)
library(dplyr)
library(ggplot2)

# Define the hyperparameter grid
hyper_grid <- expand.grid(
  mtry = seq(4, 10, by = 2),
  ntree = c(100, 200, 300)
)

# Create an empty dataframe to store results
results <- data.frame()

# Loop through the hyperparameter grid
for (i in seq_len(nrow(hyper_grid))) {
  # Fit model for ith hyperparameter combination
  RF_model <- randomForest(
    gameScore ~ games_played + season + team + position + icetime + I_F_xGoals + I_F_xRebounds + Assists + I_F_points + I_F_goals + I_F_rebounds + I_F_hits + I_F_takeaways + I_F_giveaways + lowDangerGoaAbvExp + medDangerGoaAbvExp + highDangerGoaAbvExp + faceoffsWon + penalityMinutes + penalityMinutesDrawn + shotsBlockedByPlayer + subsetclust1,
    data = CLUSTEREDfor_3_,
    ntree = hyper_grid$ntree[i],
    mtry = hyper_grid$mtry[i],
    importance = TRUE,
    na.action = na.omit
  )
  # Calculate RMSE
  oob_error <- sqrt(RF_model$mse)
  
  # Store results
  result <- data.frame(
    mtry = hyper_grid$mtry[i],
    ntree = hyper_grid$ntree[i],
    oob_error = oob_error
  )
  results <- bind_rows(results, result)
}

# Find the best model
best_model <- results %>% 
  filter(oob_error == min(oob_error))

# Print summary of the best model
print(best_model)
summary(best_model)

# Plot variable importance of the best model
var_importance <- data.frame(
  Variable = row.names(importance(RF_model)),
  Importance = importance(RF_model)$MeanDecreaseGini
)

# Sort variable importance by Importance
var_importance <- var_importance[order(-var_importance$Importance), ]

# Plot variable importance
ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Variable", y = "Importance", title = "Variable Importance Plot")

# Check the structure of importance(RF_model)
str(importance(RF_model))

# Plot variable importance of the best model
var_importance <- data.frame(
  Variable = rownames(importance(RF_model)),
  Importance = importance(RF_model)[, "IncNodePurity"] # Using IncNodePurity for importance
)

# Sort variable importance by Importance
var_importance <- var_importance[order(-var_importance$Importance), ]

# Plot variable importance
ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Variable", y = "Importance", title = "Variable Importance Plot")

# Calculate percent of variance explained by the model
percent_variance_explained <- RF_model$rsq * 100

# Print the percent of variance explained
print(percent_variance_explained)















library(randomForest)
library(dplyr)
library(ggplot2)

# Load the dataset CLUSTEREDdef_1_with
data(CLUSTEREDdef_1_)

# Define the hyperparameter grid
hyper_grid <- expand.grid(
  mtry = seq(4, 10, by = 2),
  ntree = c(100, 200, 300)
)

# Create an empty dataframe to store results
results <- data.frame()

# Loop through the hyperparameter grid
for (i in seq_len(nrow(hyper_grid))) {
  # Fit model for ith hyperparameter combination
  RF_model <- randomForest(
    gameScore ~ games_played + season + team  + icetime + I_F_xGoals + I_F_xRebounds + Assists + I_F_points + I_F_goals + I_F_rebounds + I_F_hits + I_F_takeaways + I_F_giveaways + lowDangerGoaAbvExp + medDangerGoaAbvExp + highDangerGoaAbvExp + faceoffsWon + penalityMinutes + penalityMinutesDrawn + shotsBlockedByPlayer + subsetclust2,
    data = CLUSTEREDdef_1_,
    ntree = hyper_grid$ntree[i],
    mtry = hyper_grid$mtry[i],
    importance = TRUE,
    na.action = na.omit
  )
  # Calculate RMSE
  oob_error <- sqrt(RF_model$mse)
  
  # Store results
  result <- data.frame(
    mtry = hyper_grid$mtry[i],
    ntree = hyper_grid$ntree[i],
    oob_error = oob_error
  )
  results <- bind_rows(results, result)
}

# Find the best model
best_model <- results %>% 
  filter(oob_error == min(oob_error))

# Print summary of the best model
print(summary(best_model))

# Plot variable importance of the best model
var_importance <- data.frame(
  Variable = names(best_model$importance),
  Importance = best_model$importance[, "%IncMSE"]
)

# Sort variable importance by Importance
var_importance <- var_importance[order(-var_importance$Importance), ]

# Plot variable importance
ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Variable", y = "Importance", title = "Variable Importance Plot")


# Load the dplyr package for the arrange() function
library(dplyr)

# Sort variable importance by Importance
var_importance <- var_importance %>% 
  arrange(desc(Importance))

# Plot variable importance
ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Variable", y = "Importance", title = "Variable Importance Plot")


# Print the structure of var_importance dataframe
str(var_importance)


# Plot variable importance of the best model
var_importance <- data.frame(
  Variable = colnames(importance(RF_model)),
  Importance = importance(RF_model)[, "%IncMSE"]
)

# Sort variable importance by Importance
var_importance <- var_importance[order(-var_importance$Importance), ]

# Plot variable importance
ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Variable", y = "Importance", title = "Variable Importance Plot")


# Plot variable importance of the best model
var_importance <- data.frame(
  Variable = rownames(importance(RF_model)),
  Importance = importance(RF_model)[, "%IncMSE"]
)

# Sort variable importance by Importance
var_importance <- var_importance[order(-var_importance$Importance), ]

# Plot variable importance
ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Variable", y = "Importance", title = "Variable Importance Plot")

print(RF_model)
summary(RF_model)
print(RF_model$mse)


































RF_modelG <- randomForest(GoalsSavedAbvExp ~ games_played + season + team + icetime + xGoals + goals + ReboundsAbvExp + XPlayConAbvExp + subsetclust3 + lowDangerGoalAbvExp + medDangerGoalAbvExp + highDangerGoalAbvExp + penalityMinutes, data = CLUSTEREDgoa_1_, ntree = 100, mtry = 4, seed = 123)
# Load required libraries
library(randomForest)
library(ggplot2)

# Build the random forest model
RF_modelG <- randomForest(
  GoalsSavedAbvExp ~ games_played + season + team + icetime + xGoals + goals + ReboundsAbvExp + XPlayConAbvExp + subsetclust3 + lowDangerGoalAbvExp + medDangerGoalAbvExp + highDangerGoalAbvExp + penalityMinutes, 
  data = CLUSTEREDgoa_1_, 
  ntree = 100, 
  mtry = 4, 
  seed = 123
)

# Plot variable importance of the model
var_importance <- data.frame(
  Variable = rownames(importance(RF_modelG)),
  Importance = importance(RF_modelG)[, "IncNodePurity"] # Using IncNodePurity for importance
)

# Sort variable importance by Importance
var_importance <- var_importance[order(-var_importance$Importance), ]

# Plot variable importance
ggplot(var_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Variable", y = "Importance", title = "Variable Importance Plot")

# Calculate percent variance explained by the model
percent_variance_explained <- RF_modelG$rsq * 100


# Get mtry
mtry <- RF_modelG$mtry

# Get number of trees
num_trees <- ntree(RF_modelG)

# Get out-of-bag error
oob_error <- sqrt(RF_modelG$mse)

# Output the results
print(paste("Percent Variance Explained:", percent_variance_explained))
print(paste("mtry:", mtry))
print(paste("Number of Trees:", num_trees))
print(paste("Out-of-Bag Error:", oob_error))

# Get number of trees
num_trees <- RF_modelG$ntree

# Output the number of trees
print(paste("Number of Trees:", num_trees))























# Function to perform TOPSIS
topsis <- function(data) {
  # 1. Normalize the data
  normalized_data <- apply(data, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  
  # 2. Determine the weights (you can adjust these weights based on importance)
  weights <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
  
  # 3. Determine the ideal and anti-ideal solutions
  ideal_solution <- apply(normalized_data, 2, max)
  anti_ideal_solution <- apply(normalized_data, 2, min)
  
  # 4. Calculate the distance from the ideal and anti-ideal solutions
  distance_to_ideal <- sqrt(rowSums((normalized_data - ideal_solution)^2))
  distance_to_anti_ideal <- sqrt(rowSums((normalized_data - anti_ideal_solution)^2))
  
  # 5. Calculate the relative closeness to the ideal solution
  relative_closeness <- distance_to_anti_ideal / (distance_to_ideal + distance_to_anti_ideal)
  
  # 6. Rank the alternatives
  ranked_alternatives <- order(relative_closeness, decreasing = TRUE)
  
  return(ranked_alternatives)
}

# Apply TOPSIS to each dataset
topsis_result_goa <- topsis(CLUSTEREDgoa_1_)
topsis_result_for <- topsis(CLUSTEREDfor_3_)
topsis_result_def <- topsis(CLUSTEREDdef_1_)

# Output the rankings
print("Ranking of alternatives for CLUSTEREDgoa_1_:")
print(topsis_result_goa)
print("Ranking of alternatives for CLUSTEREDfor_3_:")
print(topsis_result_for)
print("Ranking of alternatives for CLUSTEREDdef_1_:")
print(topsis_result_def)







# Function to perform TOPSIS
topsis <- function(data) {
  # Convert all columns to numeric, handling missing values
  data_numeric <- apply(data, 2, function(x) as.numeric(as.character(x)))
  
  # Check for missing values
  if (any(is.na(data_numeric))) {
    stop("Dataset contains missing or non-numeric values. Please handle missing or non-numeric values before applying TOPSIS.")
  }
  
  # Normalize the data
  normalized_data <- apply(data_numeric, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  
  # Determine the weights (you can adjust these weights based on importance)
  weights <- rep(1/ncol(data), ncol(data))
  
  # Determine the ideal and anti-ideal solutions
  ideal_solution <- apply(normalized_data, 2, max)
  anti_ideal_solution <- apply(normalized_data, 2, min)
  
  # Calculate the distance from the ideal and anti-ideal solutions
  distance_to_ideal <- sqrt(rowSums((normalized_data - ideal_solution)^2))
  distance_to_anti_ideal <- sqrt(rowSums((normalized_data - anti_ideal_solution)^2))
  
  # Calculate the relative closeness to the ideal solution
  relative_closeness <- distance_to_anti_ideal / (distance_to_ideal + distance_to_anti_ideal)
  
  # Rank the alternatives
  ranked_alternatives <- order(relative_closeness, decreasing = TRUE)
  
  return(ranked_alternatives)
}

# Apply TOPSIS to each dataset
tryCatch({
  topsis_result_goa <- topsis(CLUSTEREDgoa_1_)
  print("Ranking of alternatives for CLUSTEREDgoa_1_:")
  print(topsis_result_goa)
}, error = function(e) {
  print("Error in CLUSTEREDgoa_1_:")
  print(e$message)
})

tryCatch({
  topsis_result_for <- topsis(CLUSTEREDfor_3_)
  print("Ranking of alternatives for CLUSTEREDfor_3_:")
  print(topsis_result_for)
}, error = function(e) {
  print("Error in CLUSTEREDfor_3_:")
  print(e$message)
})

tryCatch({
  topsis_result_def <- topsis(CLUSTEREDdef_1_)
  print("Ranking of alternatives for CLUSTEREDdef_1_:")
  print(topsis_result_def)
}, error = function(e) {
  print("Error in CLUSTEREDdef_1_:")
  print(e$message)
})





# Function to handle missing or non-numeric values
handle_missing_values <- function(data) {
  # Convert non-numeric values to NA
  data_numeric <- apply(data, 2, function(x) as.numeric(as.character(x)))
  
  # Impute missing values with column means
  data_numeric[is.na(data_numeric)] <- colMeans(data_numeric, na.rm = TRUE)
  
  return(data_numeric)
}

# Apply TOPSIS to each dataset after handling missing or non-numeric values
tryCatch({
  topsis_result_goa <- topsis(handle_missing_values(CLUSTEREDgoa_1_))
  print("Ranking of alternatives for CLUSTEREDgoa_1_:")
  print(topsis_result_goa)
}, error = function(e) {
  print("Error in CLUSTEREDgoa_1_:")
  print(e$message)
})

tryCatch({
  topsis_result_for <- topsis(handle_missing_values(CLUSTEREDfor_3_))
  print("Ranking of alternatives for CLUSTEREDfor_3_:")
  print(topsis_result_for)
}, error = function(e) {
  print("Error in CLUSTEREDfor_3_:")
  print(e$message)
})

tryCatch({
  topsis_result_def <- topsis(handle_missing_values(CLUSTEREDdef_1_))
  print("Ranking of alternatives for CLUSTEREDdef_1_:")
  print(topsis_result_def)
}, error = function(e) {
  print("Error in CLUSTEREDdef_1_:")
  print(e$message)
})







# Function to handle missing or non-numeric values
handle_missing_values <- function(data) {
  # Convert non-numeric values to NA
  data_numeric <- apply(data, 2, function(x) as.numeric(as.character(x)))
  
  # Impute missing values with column medians
  data_numeric[is.na(data_numeric)] <- apply(data_numeric, 2, function(x) median(x, na.rm = TRUE))
  
  return(data_numeric)
}

# Apply TOPSIS to each dataset after handling missing or non-numeric values
tryCatch({
  topsis_result_goa <- topsis(handle_missing_values(CLUSTEREDgoa_1_))
  print("Ranking of alternatives for CLUSTEREDgoa_1_:")
  print(topsis_result_goa)
}, error = function(e) {
  print("Error in CLUSTEREDgoa_1_:")
  print(e$message)
})

tryCatch({
  topsis_result_for <- topsis(handle_missing_values(CLUSTEREDfor_3_))
  print("Ranking of alternatives for CLUSTEREDfor_3_:")
  print(topsis_result_for)
}, error = function(e) {
  print("Error in CLUSTEREDfor_3_:")
  print(e$message)
})

tryCatch({
  topsis_result_def <- topsis(handle_missing_values(CLUSTEREDdef_1_))
  print("Ranking of alternatives for CLUSTEREDdef_1_:")
  print(topsis_result_def)
}, error = function(e) {
  print("Error in CLUSTEREDdef_1_:")
  print(e$message)
})


# Function to perform TOPSIS
topsis <- function(data, weights = NULL) {
  # Convert all columns to numeric
  data_numeric <- apply(data, 2, function(x) as.numeric(as.character(x)))
  
  # Check for missing or non-numeric values
  if (any(is.na(data_numeric)) || any(!is.numeric(data_numeric))) {
    stop("Dataset contains missing or non-numeric values. Please handle missing or non-numeric values before applying TOPSIS.")
  }
  
  # Normalize the data
  normalized_data <- apply(data_numeric, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  
  # Set default weights if not provided
  if (is.null(weights)) {
    weights <- rep(1/ncol(data_numeric), ncol(data_numeric))
  }
  
  # Determine the ideal and anti-ideal solutions
  ideal_solution <- apply(normalized_data, 2, max)
  anti_ideal_solution <- apply(normalized_data, 2, min)
  
  # Calculate the distance from the ideal and anti-ideal solutions
  distance_to_ideal <- sqrt(rowSums((normalized_data - ideal_solution)^2))
  distance_to_anti_ideal <- sqrt(rowSums((normalized_data - anti_ideal_solution)^2))
  
  # Calculate the relative closeness to the ideal solution
  relative_closeness <- distance_to_anti_ideal / (distance_to_ideal + distance_to_anti_ideal)
  
  # Rank the alternatives
  ranked_alternatives <- order(relative_closeness, decreasing = TRUE)
  
  return(ranked_alternatives)
}

# Apply TOPSIS to each dataset
datasets <- list(CLUSTEREDgoa_1_, CLUSTEREDfor_3_, CLUSTEREDdef_1_)
dataset_names <- c("CLUSTEREDgoa_1_", "CLUSTEREDfor_3_", "CLUSTEREDdef_1_")

for (i in seq_along(datasets)) {
  tryCatch({
    topsis_result <- topsis(datasets[[i]])
    cat("Ranking of alternatives for", dataset_names[i], ":\n")
    print(topsis_result)
  }, error = function(e) {
    cat("Error in", dataset_names[i], ":\n")
    print(e$message)
  })
}

# Function to perform TOPSIS
topsis <- function(data, weights = NULL) {
  # Convert all columns to numeric
  data_numeric <- apply(data, 2, function(x) as.numeric(as.character(x)))
  
  # Check for missing or non-numeric values
  if (any(is.na(data_numeric)) || any(!is.numeric(data_numeric))) {
    stop("Dataset contains missing or non-numeric values. Please handle missing or non-numeric values before applying TOPSIS.")
  }
  
  # Check for infinite values
  if (any(is.infinite(data_numeric))) {
    stop("Dataset contains infinite values. Please handle infinite values before applying TOPSIS.")
  }
  
  # Normalize the data
  normalized_data <- apply(data_numeric, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  
  # Set default weights if not provided
  if (is.null(weights)) {
    weights <- rep(1/ncol(data_numeric), ncol(data_numeric))
  }
  
  # Determine the ideal and anti-ideal solutions
  ideal_solution <- apply(normalized_data, 2, max)
  anti_ideal_solution <- apply(normalized_data, 2, min)
  
  # Calculate the distance from the ideal and anti-ideal solutions
  distance_to_ideal <- sqrt(rowSums((normalized_data - ideal_solution)^2))
  distance_to_anti_ideal <- sqrt(rowSums((normalized_data - anti_ideal_solution)^2))
  
  # Calculate the relative closeness to the ideal solution
  relative_closeness <- distance_to_anti_ideal / (distance_to_ideal + distance_to_anti_ideal)
  
  # Rank the alternatives
  ranked_alternatives <- order(relative_closeness, decreasing = TRUE)
  
  return(ranked_alternatives)
}

# Apply TOPSIS to each dataset
datasets <- list(CLUSTEREDgoa_1_, CLUSTEREDfor_3_, CLUSTEREDdef_1_)
dataset_names <- c("CLUSTEREDgoa_1_", "CLUSTEREDfor_3_", "CLUSTEREDdef_1_")

for (i in seq_along(datasets)) {
  tryCatch({
    topsis_result <- topsis(datasets[[i]])
    cat("Ranking of alternatives for", dataset_names[i], ":\n")
    print(topsis_result)
  }, error = function(e) {
    cat("Error in", dataset_names[i], ":\n")
    print(e$message)
  })
}





