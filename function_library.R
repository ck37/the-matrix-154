# tm is a nice text mining package for R.
# See helpful guide at https://github.com/rochelleterman/PS239T/blob/master/11_text-analysis/2_Pre-processing.Rmd
library(tm)

# For step 1.
import_text_documents = function(directory = "", doc_dirs = list(no_type="")) {
  
  file_names = list.files(path=directory, pattern="*\\.txt", full.names=F, recursive=T)
  cat(paste0("Found ", length(file_names), " text files in \"", directory, "\" to import.\n"))
  
  if (length(file_names) == 0) {
    stop(paste("did not find any files to load. \nMake sure you have unzipped the training",
               "data into the inbound directory."))
  }
  
  # This takes a while to run -  ~75 seconds on my computer. -CK
  system.time({
    # Recursively load all text files in subdirectories of our main file directory.
    docs = list()
    for (type in names(doc_dirs)) {
      docs[[type]] = Corpus(DirSource(paste0(directory, doc_dirs[[type]])))
      cat(paste0("Processed ", type, " in subdir \"", doc_dirs[[type]], "\" and found ", length(docs[[type]]), " documents.\n"))
    }
  })
  
  # Docs now contains the large corpus of text documents, coming in at 1.4 GB of memory.
  # It is a list with 4 elements, one for each type of text document.
  
  # Double-check how many documents we loaded.
  cat("Total documents loaded:", sum(sapply(docs, length)), "\n")
  
  # Return the result.
  return(docs)
}
