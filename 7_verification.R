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
library(doMC)

#verification_dir = "inbound/Practice"
#verification_labels = "inbound/Practice_label.csv"

verification_dir = "inbound/Validation"
verification_labels = ""

# Load our main functions.
source("function_library.R")

setup_multicore()

# Load RF model.
#load("data/models-rf-2015-12-01-slow.RData")
#load("data/models-rf-2015-12-02-very-slow.RData")

# Load SVM model.
#load("data/models-svm-fast.RData")

# Load GBM model
#load("data/models-gbm-2015-12-03-slow-v1.RData")
#load("data/models-gbm-2015-12-05-slow-sentences.RData")
#load("data/models-gbm-2015-12-06-slow-v1.RData")
load("data/models-gbm.RData")

# Define the models we want to evaluate.
models = list(#rf = list(model=rf, export_name="rf-export.csv"),
              #svm = list(model=svm, export_name="svm-export.csv",
              gbm = list(model=gbm, export_name="gbm-export.csv")
)

# Models to skip.
models$svm = NULL
models$rf = NULL



# Load the word feature matrix from step 3.
load("data/filtered-docs.Rdata")
#load("data/filtered-docs-low50-high80pct.Rdata")
#training_docs = docs

load("data/power-features.RData")
#data = cbind(targets, docs)
training_docs = cbind(docs, power_features)

# Load practice verification set.
imported_docs = import_text_documents(verification_dir)
stopwords = load_stopwords()
results = clean_imported_documents(imported_docs, stopwords)
docs = results$docs

# This should be 1.
length(imported_docs)

# Merge docs into a single object to compute ngrams on the full dataset.
combined_docs = do.call(c, imported_docs)
length(combined_docs)
class(combined_docs)

# Add sentence features.
sentence_features = power_features_sentence(combined_docs)
#sentence_features = power_features_sentence(imported_docs[[1]])

# Review summmary stats on word features.
apply(sentence_features, FUN=summary, MARGIN=2)

# Check for NAs.
apply(sentence_features, FUN=function(x){sum(is.na(x))}, MARGIN=2)

# Add word features.
word_features = power_features_dtm(docs)

# Review summmary stats on word features.
apply(word_features, FUN=summary, MARGIN=2)

# Check for NAs.
apply(word_features, FUN=function(x){sum(is.na(x))}, MARGIN=2)

# Add bigrams
bigram_features = power_features_ngrams(combined_docs, stopwords, ngrams = 2)

# Add trigrams
trigram_features = power_features_ngrams(combined_docs, stopwords, ngrams = 3)

# Combine into a merged dataframe.
docs = cbind(docs, sentence_features, word_features, bigram_features, trigram_features)

# Re-sort documents by converting filename to an ID.
docs$id = as.numeric(rownames(docs))
# Reorder documents by ascending order of filename, so that it's 0 - 101.
docs = docs[order(docs$id), ]


# Ensure that features are the same between the two corpuses.
# Initialize all features to 0.
new_docs = as.data.frame(matrix(0, nrow=nrow(docs), ncol=ncol(training_docs)))
colnames(new_docs)  = colnames(training_docs)
# Copy the old columns into the new word feature matrix.
current_features = colnames(docs)
features_to_copy = current_features[current_features %in% colnames(training_docs)]
for (feature in features_to_copy) {
  new_docs[, feature] = docs[, feature]
}

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
    
    incorrect_predictions = rownames(labels[predictions_int != labels$category,])
    
    #cat("Incorrectly predicted documents:")
    #cat(paste0(incorrect_predictions, collapse=", "), "\n")
  }
}

save(models, file="data/model-verification.RData")

#########################################
# Cleanup

gc()

# Review script execution time.
if (exists("script_timer")) {
  cat("Script execution time:", round((proc.time() - script_timer)[3] / 60, 0), "minutes.\n")
  rm(script_timer)
}

# Stop logging.
sink()
