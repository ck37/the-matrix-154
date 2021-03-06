# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Start timing the script.
script_timer = proc.time()

# --- End prelude.
#########################################

library(randomForest)
library(e1071)
library(xgboost)

# Load our main functions.
source("function_library.R")

verification_dir = "inbound/In-Class Competition/inClassCompetition"
verification_labels = ""

#verification_dir = "inbound/Practice"
#verification_labels = "inbound/Practice_label.csv"

# Load RF model.
#load("data/models-rf-2015-12-01-slow.RData")
#load("data/models-rf-2015-12-02-very-slow.RData")
load("data/models-rf-feature-selection-top2k-v2.RData")

# Load SVM model.
#load("data/models-svm-fast.RData")

# Load GBM model
#load("data/models-gbm-2015-12-03-slow-v1.RData")

# Define the models we want to evaluate.
models = list(rf = list(model=rf, export_name="rf_pred.csv")
              #svm = list(model=svm, export_name="svm-export.csv",
              #gbm = list(model=gbm, export_name="gbm-export.csv")
)

# Models to skip.
#models$svm = NULL
#models$rf = NULL




# Load the word feature matrix from step 3.
#load("data/filtered-docs.Rdata")
load("data/filtered-docs-low50-high80pct.Rdata")
training_docs = docs

# Load practice verification set.
imported_docs = import_text_documents(verification_dir)
results = clean_imported_documents(imported_docs, load_stopwords())
docs = results$docs

# Re-sort documents by converting filename to an ID.
docs$id = as.numeric(rownames(docs))
# Reorder documents by ascending order of filename, so that it's 0 - 101.
docs = docs[order(docs$id), ]



# Ensure that word features are the same between the two corpuses.
# Initialize all features to 0.
new_docs = as.data.frame(matrix(0, nrow=nrow(docs), ncol=ncol(training_docs)))
colnames(new_docs)  = colnames(training_docs)
# Copy the old columns into the new word feature matrix.
current_features = colnames(docs)
features_to_copy = current_features[current_features %in% colnames(training_docs)]
for (feature in features_to_copy) {
  new_docs[, feature] = docs[, feature]
}

# Restrict to features used in the model.
feature_matrix = new_docs[, features]
# Confirm that we have the correct number of columns.
stopifnot(ncol(feature_matrix) == 2000)

save(feature_matrix, file="exports/feature-matrix.RData")
#write.table(feature_matrix, file=paste0("exports/feature-matrix.csv"), row.names=F, quote=F, col.names=T, sep=",")
write.csv(feature_matrix, file="exports/feature-matrix.csv", row.names=F, quote=F, col.names=T, sep=",")

# Confirm that all training doc features exist in the new dataframe.
# If this stops with an error then we need to fix the script.
stopifnot(sum(!colnames(training_docs) %in% colnames(new_docs)) == 0)

for (model_name in names(models)) {
  cat("Processing", model_name, "\n")
  model = models[[model_name]]
  
  # Predict class using the model
  if (model_name == "gbm") {
    predictions_int = predict(model$model, data.matrix(new_docs))
  } else {
    predictions = predict(model$model, new_docs)
    # Convert the predictions to numeric codes.
    predictions_int = as.numeric(factor(predictions)) - 1
  }
  
  #print(table(predictions))

  #print(table(predictions, predictions_int))

  # Generate csv export.
  predict_export = cbind(id=docs$id, category=predictions_int)
  write.table(predict_export, file=paste0("exports/", model$export_name), row.names=F, quote=F, col.names=T, sep=",")

  # Load practice labels and check accuracy.
  if (nchar(verification_labels) > 0) {
    labels = read.csv(verification_labels)
    accuracy = mean(predictions_int == labels$category)
    cat("Accuracy on the verification set:", accuracy, "\n")
    print(table(predictions_int, labels$category))
    models[[model_name]]$accuracy = accuracy
    models[[model_name]]$predictions = predict_export
  }
}

#save(models, file="data/model-verification.RData")

#########################################
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
