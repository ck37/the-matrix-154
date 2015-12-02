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

# Remember to load the .Rproj file in Rstudio so that paths are relative to the project directory.
load("data/imported-text-docs.Rdata")

load("function_library.R")

# Change to true to run the sample code, or just execute the lines manually.
if (F) {
  # sample data to play with
  sample = docs$child[1:3]

  # create_power_features(step1[1])

  sample_result = as.data.frame(power_features_sentence(sample))
}

power_features = list()

# TODO: use foreach here so that it can be run on multiple cores.
for (type in names(docs)) {
  power_features[[type]] = power_features_sentence(docs[[type]])
}

# TODO: convert current docs list to a dataframe.

# TODO: run the dtm power features on that dataframe.

save(power_features, file="data/power-features-sentence.RData")

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
