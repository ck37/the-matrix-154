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

### Exclude the common word features (known as stop words) listed in
# http://www.textfixer.com/resources/common-english-words.txt

load_stopwords = function(input_file = "inbound/common-english-words.txt", output_file = "data/stopwords.Rdata",
                         reload_file=F) {
  # Just re-use the output file if it already exists.
  # Set reload_file = T if you want to force the function to reload the input file.
  if (!reload_file && length(output_file) > 0 && file.exists(output_file)) {
    load(output_file)
  } else {
    # Load the official stopword list and make sure it's the same as the one used by tm.
    file_con = file(file_name)
    # Process it as one line separated by commas, and convert it to a vector.
    stopwords = unlist(strsplit(readLines(file_con)[1], split=c(","), fixed=T))
    close(file_con)
  
    if (output_file) {
      save(stopwords, file=output_file)
    }
  }
  return(stopwords)
}

# Derive a dictionary of words and total number of their appearances through out the whole dataset.
clean_documents = function(book, stopwords = c()) {
  book = tm_map(book, content_transformer(tolower))
  book = tm_map(book, removeWords, c('project','gutenberg','ebook','title','author','release','chapter'))
  if (length(stopwords) > 0) {
    book  = tm_map(book, removeWords, stopwords)
  }
  # NOTE: we should double-check this removePunctuation code, because it may remove the punctation
  # without substituting spaces, which will mess up the words.
  dtm = DocumentTermMatrix(book,
                           control = list(tolower=T, stopwords=T, removePunctuation=T, removeNumbers=T, stemming=T)
  )
  dtm = removeSparseTerms(dtm, .99)
  # Let's hold off on this part for now.
  #dtm = as.data.frame(as.matrix(dtm))
  return(dtm)
}


clean_imported_documents = function(docs, stopwords = c()) {
  
  cleaned_docs = list()
  
  # This step is slow. 
  for (type in names(docs)) {
    if (type != "no_type") {
      cat("Cleaning", type, "\n")
    }
    cleaned_docs[[type]] = clean_documents(docs[[type]], stopwords)
  }
  
  # This creates our target/response vector as a factor with values of child, history, religion, or science.
  targets = NA
  # This will run if we have outcome labels for the data, otherwise we'll keep targets as NA.
  if (length(names(docs)) > 0 | names(docs)[1] != "no_type") {
    targets = as.factor(unlist(sapply(names(cleaned_docs), simplify="array", USE.NAMES=F,
                        FUN=function(doc_type){ rep(doc_type, length.out=nrow(as.matrix(cleaned_docs[[doc_type]])))})))
  } 
  
  # Combine docs into a single dataframe.
  system.time({
    docs = as.data.frame(as.matrix(do.call(tm:::c.DocumentTermMatrix, cleaned_docs)))
  })
  
  # Fix rownames - remove the .txt suffix from the name.
  rownames(docs) = gsub("(.*)\\.txt$", "\\1", rownames(docs))
  
  # Make sure that the # of rows in the document corpus equals the length of our target vector.
  # Otherwise stop and give an error.
  if (length(targets) > 1 || !is.na(targets)) {
    stopifnot(nrow(docs) == length(targets))
  }
  
  # Return our result.
  return(list(docs=docs, targets=targets))
}