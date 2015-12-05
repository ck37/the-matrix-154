# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Start timing the script.
script_timer = proc.time()

# --- End prelude.
#########################################

library(tm)
library(SnowballC)
library(stringi)
library(stringr)
library(openNLP)
library(NLP)
library(qdap)
library(dplyr)
library(foreach)

# Remember to load the .Rproj file in Rstudio so that paths are relative to the project directory.
load("data/imported-text-docs.Rdata")

source("function_library.R")

# Setup multicore processing to speed up the processing.
library(doMC)
cat("Cores detected:", detectCores(), "\n")
if (exists("conf")) {
  registerDoMC(conf$num_cores)
} else {
  # Uses half of the available cores by default, which is a good default setting.
  registerDoMC()
}
getDoParWorkers()

# Use foreach here so that it can be run on multiple cores.
# This takes about 58 minutes to execute without multicore processing.
# TODO: get multicore processing to work.
#system.time({
  #feature_list = foreach(worker = 1:getDoParWorkers(), .combine=rbind) %do% {
  sentence_features = foreach(worker = 1:length(names(docs)), .combine="rbind") %do% {
    #power_features[[type]] = power_features_sentence(docs[[type]])
    result = power_features_sentence(docs[[worker]])
    result
  }
#})

# Confirm that we created the sentence features successfully.
stopifnot(class(sentence_features) != "NULL")

dim(sentence_features)
# TODO: confirm that we get the results in the exactly correct order.

# Save this for bigram features.
imported_docs = docs

load("data/cleaned-docs.Rdata")

# Run the dtm power features on the word feature dataframe.
# This takes ~3 hours to finish on EC2.
system.time({
  word_features = power_features_dtm(docs)
})

combined_docs = do.call(c, imported_docs)
class(combined_docs)
stopwords = load_stopwords()

system.time({
  bigram_features = power_features_bigrams(combined_docs, stopwords)
})

# Combine the sentence and word power features.
# TODO: need to make sure that we are combining in the correct order.
# Otherwise we need to merge on the book name/id.
power_features = cbind(sentence_features, word_features, bigram_features)

save(power_features, file="data/power-features.RData")

combined_features = cbind(docs, power_features)
save(combined_features, file="data/combined-features.RData")

rm(docs, imported_docs, combined_docs, combined_features)

#merge(d, e, by=0, all=TRUE) 


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
