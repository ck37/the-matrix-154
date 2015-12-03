# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Start timing the script.
script_timer = proc.time()

# --- End prelude.
#########################################

#library(gbm)
# Try xgboost rather than gbm for better multicore processing.
# To install -- devtools::install_github('dmlc/xgboost',subdir='R-package’)
# More info -- https://github.com/dmlc/xgboost/tree/master/R-package
library(xgboost)
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
speed = speed_types[4]
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
  # RF parameters
  
  # Number of predictors to choose.
  mtry_seq = c(5, 10)
  
  # Number of trees to fit for RF.
  rf_ntree = 5
  
  ####
  # SVM parameters
  
  svm_cost_seq = c(1, 10, 100)
  
  ###
  # GBM parameters
  gbm_ntrees = c(1, 2)
  gbm_depth = c(1, 2)
  #gbm_shrinkage = c(0.1, 0.001)
  gbm_shrinkage = c(0.2)
  #gbm_minobspernode = c(5, 10)
  gbm_minobspernode = c(10)
  
} else if (speed == "fast") {
  # This configuration takes about two minutes.
  cv_folds = 3
  # Subset to a random 10% of the data.
  data_subset_ratio = 0.10
  
  mtry_seq = c(10, 20)
  rf_ntree = 25
  
  svm_cost_seq = c(1, 10)
  
  ###
  # GBM parameters
  gbm_ntrees = c(10, 50)
  gbm_depth = c(1, 2)
  gbm_shrinkage = c(0.2, 0.1)
  gbm_minobspernode = c(10)
} else if (speed == "medium") {
  # This configuration takes about 30 minutes.
 
  # This might be 4 if you have 4 cores, or the full 10 if on EC2.
  # But it can't be less than 2.
  cv_folds = min(10, max(getDoParWorkers(), 2))
  data_subset_ratio = 0.25
  
  mtry_seq = round(sqrt(ncol(data)) * c(0.5, 1, 2))
  rf_ntree = 60
  
  #svm_cost_seq = c(0.01, 0.1, 1, 5, 10)
  svm_cost_seq = c(5, 10, 100)
  
  # GBM parameters
  gbm_ntrees = c(50, 100)
  gbm_depth = c(1, 2)
  gbm_shrinkage = c(0.2, 0.1, 0.01)
  gbm_minobspernode = c(10)
} else if (speed == "slow") {
  # This configuration should take about 6 hours per model.
  
  # We need to do 10 based on the project definition, even though 8 folds would be preferable.
  cv_folds = 10
  data_subset_ratio = 0.5
  
  mtry_seq = round(sqrt(ncol(data)) * c(1, 2, 4))
  rf_ntree = 100
  
  svm_cost_seq = c(5, 10, 100)
  
  # GBM parameters
  gbm_ntrees = c(100, 500, 1000)
  gbm_depth = c(1, 2)
  gbm_shrinkage = c(0.2, 0.1, 0.01)
  gbm_minobspernode = c(10)
} else if (speed == "very slow") {
  # This configuration should take about 16 hours.
  # We need to do 10 based on the project definition, even though 8 folds would be preferable.
  cv_folds = 10
  data_subset_ratio = 0.7
  
  mtry_seq = round(sqrt(ncol(data)) * c(4, 8))
  rf_ntree = 200
  
  svm_cost_seq = c(1, 5, 10)
  
  # GBM parameters
  gbm_ntrees = c(500, 1000)
  gbm_depth = c(1, 2, 4)
  gbm_shrinkage = c(0.1, 0.01, 0.005)
  gbm_minobspernode = c(10, 50)
} else {
  # Unclear how long this would take to complete, but we would want to use Amazon EC2 to run (or Savio).
  
  cv_folds = 10
  data_subset_ratio = 0.9
  
  mtry_seq = unique(round(exp(log(ncol(data))*exp(c(-0.96, -0.71, -0.48, -0.4, -0.29, -0.2)))))
  mtry_seq
  rf_ntree = 500
  
  svm_cost_seq = c(1, 5, 10)
  
  # GBM parameters
  gbm_ntrees = c(500, 1000, 2000)
  gbm_depth = c(1, 2, 4, 8)
  gbm_shrinkage = c(0.2, 0.1, 0.01, 0.001)
  gbm_minobspernode = c(3, 10, 50)
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
tune_grid = expand.grid(ntrees = gbm_ntrees, depth = gbm_depth, shrinkage = gbm_shrinkage, minobspernode = gbm_minobspernode)

# Matrix to record the cross-validation error results.
# Columns are: hyperparameter combination, CV fold number overall error rate, and per-class error rate.
cv_results = matrix(NA, nrow(tune_grid) * cv_folds, ncol(tune_grid) + length(target_classes) + 2)

print(tune_grid)

# Loop through different num of predict selected in RF 
system.time({
  # TODO: foreach over the combination of folds and parameter combinations to better use
  # high-core count systems (e.g. EC2).
  for (j in 1:nrow(tune_grid)) {
    params = tune_grid[j, ]
    cat("Params:\n")
    print(params)
    # Loop through k-folds using multicore processing.
    #for (test_fold in 1:cv_folds) {
   # cv_data = foreach (test_fold = 1:cv_folds, .combine="rbind") %dopar% {
    cv_data = foreach (test_fold = 1:cv_folds, .combine="rbind") %do% {
      # idx for validation set
      validation_rows = seq((test_fold - 1) * samples_per_fold + 1, test_fold * samples_per_fold)
      val_idx = idx[validation_rows]
      # Validation set.
      val_set = data[val_idx,]
      # Training set - we need to index within idx due to possible subsampling.
      train_set = data[idx[-validation_rows],]
      
      y_int = as.numeric(train_set[, 1]) - 1
      
      #model = gbm(train_set[, -1], train_set[, 1], n.trees = params$ntrees, interaction.depth = params$depth, n.minobsinnode = params$minobspernode, shrinkage = params$shrinkage)
      model = xgboost(data=data.matrix(train_set[, -1]), label=y_int, objective="multi:softmax", nround = params$ntrees, max_depth = params$depth, minchildweight = params$minobspernode, eta = params$shrinkage, num_class=length(target_classes), verbose=0)
      
      cv_pred = predict(model, newdata = data.matrix(val_set[,-1]))
      
      # Overall error: percentage of test observations predicted incorrectly.
      error_rate = mean(cv_pred != as.numeric(val_set[, 1]) - 1)
      
      # Calculate the per-class error rates.
      # TODO: fix this - doesn't work right now.
      per_class_error_rate = sapply(target_classes, FUN=function(class) {
        mean(cv_pred[ val_set[, 1] == class] != which(target_classes == class) - 1)
      })
      names(per_class_error_rate) = paste0("error_", names(per_class_error_rate))
      
      #data.frame(cbind(cost=params[1], test_fold, error_rate, t(per_class_error_rate)))
      results = data.frame(do.call(cbind, c(params, list(test_fold=test_fold, error_rate=error_rate, t(per_class_error_rate)))))
      print(results)
      results
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
#args = c(list(cv_results[, 1:(ncol(tune_grid) + 2)]), list(sapply(colnames(tune_grid), as.name)))
#args
cols = cv_results[, 1:(ncol(tune_grid) + 2)]
cols
#args = c(list(cols), lapply(colnames(tune_grid), as.name))
#args
#names(args) = c(".data", colnames(tune_grid))
#args
# Specify the group_by columns manually for now.
# TODO: fix the generalized version so that it's automatic.
grouped_data = group_by(cols, ntrees, depth, shrinkage, minobspernode)
#grouped_data = as.data.frame(do.call(group_by, args))
grid_results = as.data.frame(grouped_data %>% summarise(mean_error_rate = mean(error_rate), error_sd=sd(error_rate)))

grid_results

# Plot
plot(grid_results[, 1], grid_results$mean_error_rate, xlab = "Ntrees", ylab = "Cross-validated error rate",
     main = "Cross-validated GBM", type = "l")

# Find the hyperparameter combination with the minimum error rate.
best_params = grid_results[which.min(grid_results$mean_error_rate), ] 

params = best_params
params

# Refit the best parameters to the full (non-CV) dataset and save the result.
# NOTE: if using a subset of the data, it will only retrain on that subset.
#model = gbm(data[idx, -1], data[idx, 1], n.trees = params$ntrees, interaction.depth = params$depth, n.minobsinnode = params$minobspernode, shrinkage = params$shrinkage)
y_int = as.numeric(data[idx, 1]) - 1
model = xgboost(data=data.matrix(data[idx, -1]), label=y_int, objective="multi:softmax", nround = params$ntrees, max_depth = params$depth, minchildweight = params$minobspernode, eta = params$shrinkage, num_class=length(target_classes), verbose=0)

# Predict separately on holdout sample if using a subset for training and report holdout sample error.

# Define test_results in case we already used all data in cross-validation.
test_results = NA
if (data_subset_ratio != 1) {
  if (F) {
    # TODO: predict multicore to speed up execution on large datasets.
    predict_rows = which(!1:nrow(data) %in% idx)
    # Divide the rows to predict into one group per core.
    # groups = sample(1:getDoParWorkers(), length(predict_rows), replace=T)
    
    # TODO: fix this logic here.
    groups = rep(1:getDoParWorkers(), ceiling(length(predict_rows)/getDoParWorkers()))
    groups = groups[1:length(predict_rows)]
    groups = sort(groups)
    table(groups)
    pred = foreach(group = 1:getDoParWorkers(), .combine = c) %dopar% {
      predict(model, newdata = data[predict_rows[groups == group], -1])
    }
    # This is not working right now.
  
  } else {
    # Predict using a single core.
    pred = predict(model, newdata = data.matrix(data[-idx, -1]))
  }
  
  # Overall error: percentage of test observations predicted incorrectly.
  error_rate = mean(pred != as.numeric(data[-idx, 1]) - 1)
  
  # Calculate the per-class error rates.
  # TODO: fix this for xgboost.
  per_class_error_rate = sapply(target_classes, FUN=function(class) {
    #mean(pred[ data[-idx, 1] == class] != class)
    #cat("Class:", class, "\n")
    #cat(which(target_classes == class), "\n")
    mean(pred[ data[-idx, 1] == class] != which(target_classes == class) - 1)
  })
  names(per_class_error_rate) = paste0("error_", names(per_class_error_rate))
  
  test_results = data.frame(cbind(error_rate, t(per_class_error_rate)))
  print(test_results)
}

gbm = model

# Save the full model as well as the cross-validation and test-set results.
save(gbm, cv_results, grid_results, test_results, file="data/models-gbm.RData")


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