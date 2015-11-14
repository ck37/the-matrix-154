# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Load all files in the directory.
file_dir = "inbound/Training/"


file_names = list.files(path=file_dir, pattern="*\\.txt", full.names=F, recursive=T)
cat(paste0("Found ", length(file_names), " text files in \"", file_dir, "\" to import.\n"))

if (length(file_names) == 0) {
  stop(paste("did not find any files to load. \nMake sure you have unzipped the training",
        "data into the inbound directory."))
}

# tm is a nice text mining package for R.
# See helpful guide at https://github.com/rochelleterman/PS239T/blob/master/11_text-analysis/2_Pre-processing.Rmd
library(tm)

# This takes a while to run - 85-90 seconds on my computer. -CK
system.time({
  # Recursively load all text files in subdirectories of our main file directory.
  docs = list()
  doc_dirs = list(child="Child(0)", history="History(1)", religion="Religion(2)", science="Science(3)")
  for (type in names(doc_dirs)) {
    docs[[type]] = Corpus(DirSource(paste0(file_dir, "/", doc_dirs[[type]])))
    cat(paste0("Processed ", type, " in subdir \"", doc_dirs[[type]], "\" and found ", length(docs[[type]]), " documents.\n"))
  }
})
# Docs now contains the large corpus of text documents, coming in at 1.4 GB of memory.
# It is a list with 4 elements, one for each type of text document.

# Double-check how many documents we loaded.
cat("Total documents loaded:", sum(sapply(docs, length)), "\n")

# Clean up.
rm(file_names, file_dir)

# Save our results, which is also a bit slow. Should be about 224MB in file size.
save(docs, file="data/imported-text-docs.Rdata")

# Stop logging.
sink()