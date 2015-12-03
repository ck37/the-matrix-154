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

# Change to true to run the sample code, or just execute the lines manually.
if (F) {
  # sample data to play with
  sample = docs$child[1:3]

  # create_power_features(step1[1])

  sample_result = as.data.frame(power_features_sentence(sample))
}

power_features = list()

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
# This takes about 53 minutes to execute without multicore processing.
# TODO: get multicore processing to work.
system.time({
  feature_list = foreach(type = names(docs)) %do% {
    cat("Processing", type, "\n")
    #power_features[[type]] = power_features_sentence(docs[[type]])
    power_features_sentence(docs[[type]])
  }
})

# Re-save the outcome names.
names(feature_list) = names(docs)

# Double-check dimensions of the result.
sapply(feature_list, FUN=dim)

# Confirm that it corresponds with dimensions of the input doc list.
sapply(docs, FUN=length)

# Convert list to a matrix that we can cbind to the word feature matrix.
sentence_features = do.call(rbind, feature_list)
dim(sentence_features)
# TODO: confirm that we get the results in the exactly correct order.

load("data/cleaned-docs.Rdata")

# Run the dtm power features on the word feature dataframe.
# This takes an incredibly long time to execute (> 4 hours) - unclear how long it actually takes to finish.
system.time({
  word_features = power_features_dtm(docs)
})

# Combine the sentence and word power features.
# TODO: need to make sure that we are combining in the correct order.
# Otherwise we need to merge on the book name/id.
power_features = cbind(sentence_features, word_features)

save(power_features, file="data/power-features.RData")

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
