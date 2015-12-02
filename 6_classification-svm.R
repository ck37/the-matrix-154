# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Start timing the script.
script_timer = proc.time()

# --- End prelude.
#########################################

library(e1071)
library(dplyr)
library(doMC) # For multicore processing.

# Load the docs file if it doesn't already exist.
if (!exists("data", inherits=F)) {
  load("data/filtered-docs.Rdata")
  data = cbind(targets, docs)
  rm(docs)
  gc()
}


############################################
# Customize training parameters for slow vs. fast execution.

# Possible speed configurations.
speed_types = c("instant", "fast", "medium", "slow", "very slow", "ideal")
# Choose which option you want, based on speed vs. accuracy preference.
speed = speed_types[2]
cat("Speed configuration:", speed, "\n")

set.seed(5)

if (speed == "instant") {
  # The fastest possible settings that will yield a result, mainly for debugging purposes.
  # This should complete in a second or two.
  
  ###
  # General Parameters
  
  # Subset to a random 5% of the data to speed up execution time.
  data_subset_ratio = 0.05
  
  # Number of CV folds
  cv_folds = 2                                     
 
  ###
  # RF Parameters
  
  # Number of predictors to choose.
  mtry_seq = c(5, 10)
  
  # Number of trees to fit for RF.
  rf_ntree = 5
  
  ####
  # SVM Parameters
  
  svm_cost_seq = c(1, 10, 100)
  
} else if (speed == "fast") {
  # This configuration takes about two minutes.
  cv_folds = 3
  # Subset to a random 10% of the data.
  data_subset_ratio = 0.10
  
  mtry_seq = c(10, 20)
  rf_ntree = 25
  
  svm_cost_seq = c(1, 10)
} else if (speed == "medium") {
  # This configuration takes about 30 minutes.
  
  # We use 4 here because we have 4 cores right now.
  cv_folds = 4
  data_subset_ratio = 0.25
  
  mtry_seq = round(sqrt(ncol(data)) * c(0.5, 1, 2))
  rf_ntree = 60
  
  #svm_cost_seq = c(0.01, 0.1, 1, 5, 10)
  svm_cost_seq = c(5, 10, 100)
} else if (speed == "slow") {
  # This configuration should take about 6 hours.
  
  # We need to do 10 based on the project definition, even though 8 folds would be preferable.
  cv_folds = 10
  data_subset_ratio = 0.5
  
  mtry_seq = round(sqrt(ncol(data)) * c(1, 2, 4))
  rf_ntree = 100
  
  svm_cost_seq = c(5, 10, 100)
} else if (speed == "very slow") {
  # This configuration should take about 16 hours.
  # We need to do 10 based on the project definition, even though 8 folds would be preferable.
  cv_folds = 10
  data_subset_ratio = 0.7
  
  mtry_seq = round(sqrt(ncol(data)) * c(4, 8))
  rf_ntree = 200
  
  svm_cost_seq = c(1, 5, 10)
} else {
  # Unclear how long this would take to complete, but we would want to use Amazon EC2 to run (or Savio).
  
  cv_folds = 10
  data_subset_ratio = 0.9
  
  mtry_seq = unique(round(exp(log(ncol(data))*exp(c(-0.96, -0.71, -0.48, -0.4, -0.29, -0.2)))))
  mtry_seq
  rf_ntree = 500
  
  svm_cost_seq = c(1, 5, 10)
}


############################################
# Setup multicore processing to speed up the model training.

cat("Cores detected:", detectCores(), "\n")
if (exists("conf")) {
  registerDoMC(conf$num_cores)
} else {
  # Uses half of the available cores by default, which is a good default setting.
  registerDoMC()
}
getDoParWorkers()

############################################
# Setup cross-validation

# Randomly reorder the dataset, and also potentially down-sample.
idx = sample(nrow(data), round(nrow(data) * data_subset_ratio))

# NOTE: this does not use the full dataset size due to rounding, but the impact is minor.
samples_per_fold = floor(length(idx) / cv_folds)

# Save the levels of the target variable for use in the CV loop.
target_classes = levels(data[, 1])
table(target_classes)



############################################
# SVM training, based on the code from assignment 7, chap9_8.R

# Create a hyperparameter training grid to more easily generalize to multiple tuning parameters.
tune_grid = expand.grid(cost = svm_cost_seq)

# Matrix to record the cross-validation error results.
# Columns are: hyperparameter combination, CV fold number overall error rate, and per-class error rate.
cv_results = matrix(NA, nrow(tune_grid) * cv_folds, ncol(tune_grid) + length(target_classes) + 2)




# Loop through different num of predict selected in RF 
system.time({
  # TODO: foreach over the combination of folds and parameter combinations to better use
  # high-core count systems (e.g. EC2).
  for (j in 1:nrow(tune_grid)) {
    params = tune_grid[j, ]
    cat("Cost:", params[1], "\n")
    # Loop through k-folds using multicore processing.
    #for (test_fold in 1:cv_folds) {
    cv_data = foreach (test_fold = 1:cv_folds, .combine="rbind") %dopar% {
      # idx for validation set
      validation_rows = seq((test_fold - 1) * samples_per_fold + 1, test_fold * samples_per_fold)
      val_idx = idx[validation_rows]
      # Validation set.
      val_set = data[val_idx,]
      # Training set - we need to index within idx due to possible subsampling.
      train_set = data[idx[-validation_rows],]
      
      model = svm(train_set[, -1], train_set[, 1], kernel="radial", cost = params[1])
      
      cv_pred = predict(model, newdata = val_set[,-1])
      
      # Overall error: percentage of test observations predicted incorrectly.
      error_rate = mean(cv_pred != val_set[, 1])
      
      # Calculate the per-class error rates.
      per_class_error_rate = sapply(target_classes, FUN=function(class) {
        mean(cv_pred[ val_set[, 1] == class] != class)
      })
      names(per_class_error_rate) = paste0("error_", names(per_class_error_rate))
      
      data.frame(cbind(cost=params[1], test_fold, error_rate, t(per_class_error_rate)))
    }
    # Could re-order by the fold number, but doesn't actually matter.
    # cv_results = cv_results[order(cv_results[, 1]), ]
    
    # Save overall error rate and per-class error rates in a long data frame format.
    # Use this formula to save the k CV results in the correct rows.
    cv_results[((j-1)*cv_folds + 1):(j*cv_folds), ] = as.matrix(cv_data)
  }
})
colnames(cv_results) = colnames(cv_data)

# Convert from a matrix to a dataframe so that we can reshape the results.
cv_results = as.data.frame(cv_results)


# Calculate the mean & sd error rate for each combination of hyperparameters.
# Do.call is used so that we can group by the column names in the tuning grid.
# as.name converts the column names from strings to R variable names.
grid_results = as.data.frame(do.call(group_by, list(cv_results[, 1:(ncol(tune_grid) + 2)], as.name(colnames(tune_grid)))) %>% summarise(mean_error_rate = mean(error_rate), error_sd=sd(error_rate)))

grid_results

# Plot
plot(grid_results[, 1], grid_results$mean_error_rate, xlab = "Cost", ylab = "Cross-validated error rate",
     main = "Cross-validated SVM", type = "l")

# Find the hyperparameter combination with the minimum error rate.
best_params = grid_results[which.min(grid_results$mean_error_rate), ] 

# Best cost.
# NOTE: we don't handle the case where multiple params are the best, so if that happens we'll need to fix this part.
best_pred = best_params[, 1]
best_pred



# Refit the best parameters to the full (non-CV) dataset and save the result.
# NOTE: if using a subset of the data, it will only retrain on that subset.
model = svm(data[idx, -1], data[idx, 1], kernel="radial", cost = best_pred)

# Predict separately on holdout sample if using a subset for training and report holdout sample error.

# Define test_results in case we already used all data in cross-validation.
test_results = NA
if (data_subset_ratio != 1) {
  pred = predict(model, newdata = data[-idx, -1])
  
  # Overall error: percentage of test observations predicted incorrectly.
  error_rate = mean(pred != data[-idx, 1])
  
  # Calculate the per-class error rates.
  per_class_error_rate = sapply(target_classes, FUN=function(class) {
    mean(pred[ data[-idx, 1] == class] != class)
  })
  names(per_class_error_rate) = paste0("error_", names(per_class_error_rate))
  
  test_results = data.frame(cbind(error_rate, t(per_class_error_rate)))
  print(test_results)
}

svm = model

# Save the full model as well as the cross-validation and test-set results.
save(svm, cv_results, grid_results, test_results, file="data/models-svm.RData")


#########################################
# Review accuracy and generate ROC plots.

# TBD

############
# Cleanup

gc()

# Review script execution time.
if (exists("script_timer")) {
  cat("Script execution time:\n")
  print(proc.time() - script_timer)
  rm(script_timer)
}

# Stop logging.
sink()