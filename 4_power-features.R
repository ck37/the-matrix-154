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
system.time({
  #feature_list = foreach(worker = 1:getDoParWorkers(), .combine=rbind) %do% {
  sentence_features = foreach(worker = 1:length(names(docs)), .combine = "rbind") %do% {
    #power_features[[type]] = power_features_sentence(docs[[type]])
    result = power_features_sentence(docs[[worker]])
    result
  }
})

# Confirm that we created the sentence features successfully.
stopifnot(class(sentence_features) != "NULL")

dim(sentence_features)
# TODO: confirm that we get the results in the exactly correct order.

# Save this for ngram features.
imported_docs = docs

load("data/cleaned-docs.Rdata")

# Run the dtm power features on the word feature dataframe.
# This takes ~3 hours to finish on EC2.
system.time({
  word_features = power_features_dtm(docs)
})

# Merge docs into a single object to compute bigrams on the full dataset.
combined_docs = do.call(c, imported_docs)
class(combined_docs)
stopwords = load_stopwords()

# This takes 29 minutes on my laptop, single core.
system.time({
  bigram_features = power_features_ngrams(combined_docs, stopwords, ngrams = 2)
})

dim(bigram_features)

# Identify how many docs use each n-gram.
bigram_usage = apply(bigram_features, MARGIN=2, FUN=function(x){ sum(!is.na(x) & x > 0) })

# Look at the top 50 bigrams
sort(bigram_usage, decreasing=T)[1:50]


# This takes 29 minutes on my laptop, single core.
# TODO: many of these trigrams are from gutenberg legal disclaimer text - should put as stopwords or otherwise remove full disclaimer.
system.time({
  trigram_features = power_features_ngrams(combined_docs, stopwords, ngrams = 3)
})

dim(trigram_features)

# Identify how many docs use each n-gram.
trigram_usage = apply(trigram_features, MARGIN=2, FUN=function(x){ sum(!is.na(x) & x > 0) })

# Look at the top 50 bigrams
sort(trigram_usage, decreasing=T)[1:50]

# Combine the sentence and word power features.
# TODO: need to make sure that we are combining in the correct order.
# Otherwise we need to merge on the book name/id
power_features = cbind(sentence_features, word_features, bigram_features, trigram_features)

save(power_features, file="data/power-features.RData")

# Now load the filtered word features to create the combined feature matrix.
load("data/filtered-docs.Rdata")

combined_features = cbind(docs, power_features)

save(combined_features, file="data/combined-features.RData")

# Cleanup objects
rm(docs, imported_docs, combined_docs, combined_features, power_features, bigram_features, trigram_features)
rm(bigram_usage, trigram_usage, word_features)


#########################################
# Epilogue -- 

gc()

# Review script execution time.
if (exists("script_timer")) {
  cat("Script execution time:\n")
  print(proc.time() - script_timer)
  rm(script_timer)
}

# Stop logging.
sink()
