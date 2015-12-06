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

# Merge docs into a single object to compute ngrams on the full dataset.
combined_docs = do.call(c, docs)
length(combined_docs)
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

# TODO: remove some of the Gutenberg-focused trigrams so that they aren't extra noise during training.

# Save these features since they take a long time to calculate and R could crash.
save(bigram_features, trigram_features, file="data/ngram-power-features.Rdata")

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

# Review summmary stats on sentence features.
apply(sentence_features, FUN=summary, MARGIN=2)

# Save these features since they take a long time to calculate and R could crash.
save(sentence_features, file="data/sentence-power-features.Rdata")

load("data/cleaned-docs.Rdata")

# Run the dtm power features on the word feature dataframe.
# This takes ~3.6 hours to finish on EC2.
system.time({
  word_features = power_features_dtm(docs)
})

# Review summmary stats on word features.
apply(word_features, FUN=summary, MARGIN=2)

# Save these features since they take a long time to calculate and R could crash.
save(word_features, file="data/word-power-features.Rdata")

# Save component so that they can be analyzed more easily later.
save(sentence_features, word_features, bigram_features, trigram_features, file="data/power-feature-components.Rdata")

# Combine the sentence and word power features.
# TODO: need to make sure that we are combining in the correct order.
# Otherwise we need to merge on the book name/id
power_features = cbind(sentence_features, word_features, bigram_features, trigram_features)

dim(power_features)

save(power_features, file="data/power-features.RData")

# Now load the filtered word features to create the combined feature matrix.
load("data/filtered-docs.Rdata")

combined_features = cbind(docs, power_features)

dim(combined_features)

save(combined_features, file="data/combined-features.RData")

# Cleanup objects
rm(docs, combined_docs, combined_features, power_features, bigram_features, trigram_features)
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
