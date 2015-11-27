# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Load the docs file if it doesn't already exist.
if (!exists("data", inherits=F)) {
  load("data/filtered-docs.Rdata")
  data = cbind(targets, docs)
  rm(docs)
  gc()
}

library(randomForest)

############################################
# Customize training parameters for slow vs. fast execution.

# Possible speed configurations.
speed_types = c("very fast", "fast", "medium", "slow", "ideal")
# Choose which option you want, based on speed vs. accuracy preference.
speed = speed_types[3]

set.seed(5)

if (speed == "very fast") {
  # The fastest possible settings that will yield a result, mainly for debugging purposes.
  
  # Subset to a random 5% of the data to speed up execution time.
  data_subset_ratio = 0.05
  
  # Number of predictors to choose.
  mtry_seq = c(5)
  
  # Number of trees to fit for RF.
  rf_ntree = 5
  
  # Number of CV folds
  cv_folds = 2                                     
} else if (speed == "fast") {
  mtry_seq = c(5, 10)
  rf_ntree = 25
  cv_folds = 3
  # Subset to a random 30% of the data.
  data_subset_ratio = 0.3
} else if (speed == "medium") {
  mtry_seq = round(sqrt(ncol(data)) * c(0.5, 1, 2))
  rf_ntree = 100
  # We use 4 here because we have 4 cores right now.
  cv_folds = 4
  data_subset_ratio = 0.5
} else if (speed == "slow") {
  mtry_seq = round(sqrt(ncol(data)) * c(0.5, 1, 2))
  rf_ntree = 400
  # Again, 8 is a multiple of our 4 cores.
  cv_folds = 8
  data_subset_ratio = 1
} else {
  mtry_seq = unique(round(exp(log(ncol(data))*exp(c(-0.96, -0.71, -0.48, -0.4, -0.29, -0.2)))))
  mtry_seq
  rf_ntree = 2000
  cv_folds = 8
  data_subset_ratio = 1
}


############################################
# Setup multicore processing to speed of the model training.

library(doMC)
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

cv_err = matrix(NA, length(mtry_seq), cv_folds)


############################################
# RF training, based on the code from discussion section 11.

# Loop through different num of predict selected in RF 
system.time({
for (j in 1:length(mtry_seq)) {
  cat("Mtry:", mtry_seq[j], "\n")
  # Loop through k-folds
  #for (test_fold in 1:cv_folds) {
  cv_results = foreach (test_fold = 1:cv_folds, .combine="rbind") %dopar% {
    # idx for validation set
    val_idx = idx[seq((test_fold - 1) * samples_per_fold + 1, test_fold * samples_per_fold)]
    # Validation set.
    val_set = data[val_idx,]
    # Training set - we need to index within idx due to possible subsampling.
    train_set = data[idx[-val_idx],]
    rf_cv = randomForest(train_set[, -1], train_set[, 1], mtry = mtry_seq[j], ntree = rf_ntree)
    cv_pred = predict(rf_cv, newdata = val_set[,-1])
    # Percentage of test observations predicted incorrectly.
    err = mean(cv_pred != val_set[, 1])
    data.frame(cbind(test_fold, err))
  }
  # Could re-order by the fold number, but doesn't actually matter.
  # cv_results = cv_results[order(cv_results[, 1]), ]
  # The second column contains the error rates, so select it and then transpose to save as a row.
  cv_err[j, ] = t(cv_results[, 2])
}
})

# Calculate the mean of error of k-fold iterations for each value of parameter(num of predictors)
err = apply(cv_err, 1, mean)
err

# Plot
plot(mtry_seq, err, xlab = "Num of predictors", ylab = "Error rate", main = "Random Forest w/ CV", type = "l")
# Best num of pred
best_pred = mtry_seq[which.min(err)]
best_pred

# Refit the best parameters to the full dataset and save the result.
# Save importance also
rf = randomForest(data[idx, -1], data[idx, 1], mtry = best_pred, ntree = rf_ntree, importance=T)
varimp = importance(rf)
# Select the top 30 most important words.
print(round(varimp[order(varimp[, 5], decreasing=T), ], 2)[1:30, ])
save(rf, file="data/models-rf.RData")

#########################################
# Review accuracy and generate ROC plots.

# Report overall accuracy and accuracy rates per class using OOB.
# Final accuracy with the maximum number of trees:
print(rf$err.rate[nrow(rf$err.rate), ])

# Generate ROC curves

# Plot of error rate with out of bag data.
plot(rf$err.rate[,1], main="RF accuracy w/ OOB", type = "l", ylab = "Error rate", xlab = "Number of trees")
dev.copy(png, "visuals/6-rf-error-rate-overall.png")
dev.off()

library(reshape2)
errors_combined = melt(rf$err.rate[, -1], id.vars="X")
names(errors_combined) = c("ntrees", "type", "error_rate")

# Plot of error rate per class by number of trees.
library(ggplot2)
p = ggplot(errors_combined, aes(x = ntrees, y = error_rate, colour=type))
p + geom_line() + theme_bw()
dev.copy(png, "visuals/6-rf-error-rate-per-class.png")
dev.off()




#########################################
# SVM part
############

# TBD.





############
# Cleanup

gc()

# Stop logging.
sink()