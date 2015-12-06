sink() # Close any open log file first.
# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Start timing the script.
script_timer = proc.time()

# --- End prelude.
#########################################

library(tm)
library(SnowballC) # for stemming

load("data/imported-text-docs.Rdata")

source("function_library.R")

stopwords = load_stopwords()

# Check if it's the same list as the one used by tm()
length(stopwords)
length(stopwords("english"))
# Nope, these are not the same size.

# Only 70% of the official stopwords are in the tm list, so we'll need to also use this one.
mean(stopwords %in% stopwords("english"))

library(doMC) # For multicore processing.
# Setup multicore processing to speed up script execution.

cat("Cores detected:", detectCores(), "\n")
if (exists("conf")) {
  registerDoMC(conf$num_cores)
} else {
  # Uses half of the available cores by default, which is a good default setting.
  # On an Intel CPU that will correspond to the actual number of physical CPU cores.
  registerDoMC()
}
getDoParWorkers()

# Takes about 10 minutes; this does use multicore.
system.time({
  result = clean_imported_documents(docs, stopwords)
})

docs = result$docs
targets = result$targets

print(dim(docs))

# Double-check the results.
print(table(targets))
print(length(targets))

save(docs, targets, file="data/cleaned-docs.Rdata")

rm(result, docs, targets, stopwords)

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