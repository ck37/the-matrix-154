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
    file_con = file(input_file)
    # Process it as one line separated by commas, and convert it to a vector.
    stopwords = unlist(strsplit(readLines(file_con)[1], split=c(","), fixed=T))
    close(file_con)
  
    if (length(output_file) > 0) {
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


########################## power features sentences ##########################

### external function(1): convert_text_to_sentences
convert_text_to_sentences <- function(text, lang = "en") {
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  text <- as.String(text)
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  sentences <- text[sentence.boundaries]
  return(sentences)
}

### external function(2): removes punctuations
remove_punc = function(x) {
  gsub("[[:punct:]]", "", x)
}

### main function: generates sentence-based power feature matrix
### input: tm data
### output: 4 columns of power features with rownames=filename
power_features_sentence = function(doc) {
  n = length(doc)
  power = matrix(NA,nrow=n,ncol=4,dimnames=list(seq(1,n)))
  
  for(i in 1:n){
    book = doc[i]
    book2 = tm_map(book, content_transformer(tolower))
    book3 = tm_map(book2, stripWhitespace)
    book4 = tm_map(book2, stemDocument)
    
    text = as.data.frame(book4)[2]
    sents = convert_text_to_sentences(text)
    sents2 = lapply(sents,remove_punc)
    
    ### number of sentence
    power5 = length(sents2)
    
    ### average length of sentence
    power6 = sum(stri_count(sents2,regex="\\S+"))/length(sents2)
    
    ### number of 4-digit number
    power7 = length(na.omit(str_extract(sents2, "\\d{4}")))
    
    ### number of digits
    power8 = sum(grepl("[[:digit:]]", sents2))
    
    ### 
    
    title = names(book)
    power[i,] = as.matrix(cbind(power5,power6,power7,power8))
    rownames(power)[i] = title
  }
  #colnames(power) = c("power1","power2","power3","power4")
  colnames(power) = c("sentence_count","sentence_avg_length","4digit_nums","digit_count")
  return(power)
}

########################## power features from dtm ##########################
### input: dtm
### output: originial dtm combined with new power features
power_features_dtm = function(dtm) {
  ### power1: returns the vector of average word length of each txt file
  power1 = numeric()
  c = numeric()
  for(i in 1:length(dtm[ ,1])){    
    for(j in 1:length(dtm[1, ])){    
      c[j] = nchar(colnames(dtm[j])) * dtm[i,j]
    }
    power1[i] = sum(c)/ sum(dtm[i, ])
  }
  
  ### power2: returns the vector showing how many distinct words used in each file
  power2 = numeric()
  for(i in 1:length(dtm[ ,1])){    
    total = 0
    for(j in 1:length(dtm[1, ])){
      if(dtm[i,j] >= 1){
        total = total + 1
      }  
    }
    power2[i] = total
  }
  
  ### power3: total # of words
  power3 = numeric()
  for(i in 1:length(dtm[ ,1])){    
    total = sum(dtm[i, ])
    power3[i] = total  
  }
  
  ### power4: total # of characters
  power4 = numeric()
  c2 = numeric()
  for(i in 1:length(dtm[ ,1])){    
    for(j in 1:length(dtm[1, ])){    
      c2[j] = nchar(colnames(dtm[j])) * dtm[i,j]
    }
    power4[i] = sum(c2)
  }
  #dtm$power1 = power1
  #dtm$power2 = power2
  #dtm$power3 = power3
  #dtm$power4 = power4
  result = cbind(power1, power2, power3, power4)
  colnames(result) = c("words_avg_length", "words_distinct", "words_count", "chars_count")
  #return(dtm)
  return(result)
}
