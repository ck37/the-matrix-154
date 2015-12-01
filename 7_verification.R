# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Start timing the script.
script_timer = proc.time()

# --- End prelude.
#########################################


# Load our main functions.
source("function_library.R")

# Load the word feature matrix from step 3.
load("data/filtered-docs.Rdata")
training_docs = docs

# Load practice verification set.
imported_docs = import_text_documents("inbound/Practice")
results = clean_imported_documents(imported_docs, load_stopwords())
docs = results$docs

# Re-sort documents by converting filename to an ID.
docs$id = as.numeric(rownames(docs))
# Reorder documents by ascending order of filename, so that it's 0 - 101.
docs = docs[order(docs$id), ]

# Load RF model.
load("data/models-rf.RData")

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

# Confirm that all training doc features exist in the new dataframe.
# If this stops with an error then we need to fix the script.
stopifnot(sum(!colnames(training_docs) %in% colnames(new_docs)) == 0)


# Predict class using RF matrix.
predictions = predict(rf, new_docs)
table(predictions)

# Convert the predictions to numeric codes.
predictions_int = as.numeric(factor(predictions)) - 1
table(predictions, predictions_int)

# TODO: need to be using an id field here; need to modify the import/cleaning process to create that based on filename.

# Generate csv export.
write.table(cbind(docs$id, predictions_int), file="exports/verification-export.csv", row.names=F, quote=F, col.names=F, sep=",")

# Load practice labels and check accuracy.
labels = read.csv("inbound/Practice_label.csv")
cat("Accuracy on the verification set:\n")
print(mean(predictions_int == labels$category))
table(predictions_int, labels$category)



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