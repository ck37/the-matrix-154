# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Start timing the script.
script_timer = proc.time()

# --- End prelude.
#########################################

library(tm)
library(SnowballC) # for stemming

# Load the docs file if it doesn't already exist.
if (!exists("docs")) {
  load("data/imported-text-docs.Rdata")
}

source("function_library.R")

################## Exploring docs ##################
### docs is a list of 4 groups "child", "history", "religion", "science"
### info of the first file of the group "child"
child = docs$child
length(child)
inspect(child[1])
### see content of the first file of the group "child"
as.character(child[1])

################# Feature Creation ###############

# Generating small sample
index1 = sample(length(docs$child),10)
index2 = sample(length(docs$history),10)
sample1 = docs$child[index1]
sample2 = docs$history[index2]
sample = c(sample1,sample2)


#result = clean_imported_documents(sample)
# View(result)

stopwords = load_stopwords()

# Check if it's the same list as the one used by tm()
length(stopwords)
length(stopwords("english"))
# Nope, these are not the same size.

# Only 70% of the official stopwords are in the tm list, so we'll need to also use this one.
mean(stopwords %in% stopwords("english"))

result = clean_imported_documents(docs, stopwords)

docs = result$docs
targets = result$targets

print(dim(docs))

# Double-check the results.
print(table(targets))
print(length(targets))

save(docs, targets, file="data/cleaned-docs.Rdata")

rm(result)

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