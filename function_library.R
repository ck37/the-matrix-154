# tm is a nice text mining package for R.
# See helpful guide at https://github.com/rochelleterman/PS239T/blob/master/11_text-analysis/2_Pre-processing.Rmd
library(tm)
library(SnowballC) # for stemming

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
    # Since disk is the main bottleneck here, multicore processing probably wouldn't help.
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

# Derive a dictionary of words/ bigrams and total number of their appearances through out the whole dataset.
clean_documents = function(book, stopwords = c()) {
  book = tm_map(book, content_transformer(tolower))
  book = tm_map(book, removeWords, c('project','gutenberg','ebook','title','author','release','chapter','posting','editor','translator','encoding','ascii','updated'))
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

# For step 2; docs is a list of matrices loaded in step 1.
clean_imported_documents = function(docs, stopwords = c()) {
  
  cleaned_docs = list()
  
  # This step is slow. 
  #foreach (type in names(docs)) %dopar% {
  system.time({
    cleaned_docs = foreach (type = names(docs)) %dopar% {
    #if (type != "no_type") {
    #  cat("Cleaning", type, "\n")
    #}
    #cleaned_docs[[type]] = clean_documents(docs[[type]], stopwords)
      clean_documents(docs[[type]], stopwords)
    }
  })
  names(cleaned_docs) = names(docs)
  # Confirm document dimensions:
  sapply(cleaned_docs, FUN=dim)
  
  # This creates our target/response vector as a factor with values of child, history, religion, or science.
  targets = NA
  # This will run if we have outcome labels for the data, otherwise we'll keep targets as NA.
  if (length(names(docs)) > 0 | names(docs)[1] != "no_type") {
    targets = as.factor(unlist(sapply(names(cleaned_docs), simplify="array", USE.NAMES=F,
                        FUN=function(doc_type){ rep(doc_type, length.out=nrow(as.matrix(cleaned_docs[[doc_type]])))})))
  } 
  
  # Combine docs into a single dataframe - takes roughly a minute on EC2.
  system.time({
    docs = as.data.frame(as.matrix(do.call(tm:::c.DocumentTermMatrix, cleaned_docs)))
  })
  
  # Fix rownames - remove the .txt suffix from the name.
  #rownames(docs) = gsub("(.*)\\.txt$", "\\1", rownames(docs))
  rownames(docs) = gsub("\\.txt$", "\\1", rownames(docs))
  
  # Confirm that we have no duplicated rownames.
  length(unique(rownames(docs))) == nrow(docs)
  
  # Make sure that the # of rows in the document corpus equals the length of our target vector.
  # Otherwise stop and give an error.
  if (length(targets) > 1 || !is.na(targets)) {
    stopifnot(nrow(docs) == length(targets))
  }
  
  # Return our result.
  result = list(docs=docs, targets=targets)
  return(result)
}


########################## power features sentences:  ##########################

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
    book4 = tm_map(book3, stemDocument)
    
    text = as.data.frame(book4)[2]
    sents = convert_text_to_sentences(text)
    sents2 = lapply(sents,remove_punc)
    
    ### number of sentence
    power6 = length(sents2)
    
    ### average length of sentence
    power7 = sum(stri_count(sents2,regex="\\S+"))/length(sents2)
    
    ### number of 4-digit number
    power8 = length(na.omit(str_extract(sents2, "\\d{4}")))
    
    ### number of digits
    power9 = sum(grepl("[[:digit:]]", sents2))
    
    ### 
    
    title = names(book)
    power[i,] = as.matrix(cbind(power6,power7,power8,power9))
    rownames(power)[i] = title
  }
  colnames(power) = c("sentence_count","sentence_avg_length","4digit_nums","digit_count")
  return(power)
}

########################## power features from dtm: 6 features ##########################
### input: dtm
### output: new power features matrix
power_features_dtm = function(dtm) {
  
  new = matrix(NA,nrow=nrow(dtm),ncol=6)
  colnames(new) = c("words_count","chars_count","words_avg_length","words_distinct","sd_words", "word_diversity")
  words_chars = nchar(colnames(dtm))
  
  for(i in 1:nrow(dtm)){
    ### power1: total number of words
    new[i,1] = sum(as.numeric(dtm[i,]))
    
    ### power2: total number of characters
    new[i,2] = as.numeric(t(as.matrix(words_chars))%*%as.matrix(as.numeric(dtm[i,])))
    
    ### power3: returns the vector of average word length of each txt file
    # Use max so that if there are 0 distinct words we don't try to divide by 0.
    new[i,3] = new[i,2]/max(new[i,1], 1)
    
    ### power4: number of unique words
    new[i,4] = length(which(as.numeric(dtm[i,])!=0))
    
    ### power5: standard deviation of word length
    sqrdmean = sum(as.matrix(words_chars^2)*as.matrix(as.numeric(dtm[i,])))/max(new[i,1], 1)
    mean = sum(words_chars*as.matrix(as.numeric(dtm[i,])))/max(new[i,1], 1)
    new[i,5] = sqrdmean-(mean^2)
    
    ### power6: word diversity
    new[i,6] = new[i,4]/max(new[i,1], 1)
    
  }
  new = as.data.frame(new)
  return(new)
}

################ power features bigrams: most frequent 2950 features ###############
### input: tm data
### output: power feature columns
library(NLP)
BigramTokenizer = function(x){
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
options(mc.cores=1)

gutenberg = c('project','gutenberg','ebook','anyone anywhere','no cost','re-use',
              'license','online','wwwgutenbergorg','wwwgutenbergnet','language english','ascii',
              'httpwwwpgdpnet','first edition','second edition','title',
              'author','release','chapter','posting','editor','translator','encoding','updated',
              'anyone anywhere', 'at no cost and with almost', 'restrictions whatsoever','you may copy it',
              'give it away', 'reuse it', 'under the terms', 'license included',
              'distributed','proofread','proofreading team','character set','encoding','usascii')

power_features_bigrams = function(book, stopwords = c()){
  book = tm_map(book, content_transformer(tolower))
  book = tm_map(book, removePunctuation)
  book = tm_map(book, removeWords, gutenberg)  
  book = tm_map(book, removeNumbers)
  
  if (length(stopwords)>0){
    book  = tm_map(book, removeWords, stopwords)
  }
  book = tm_map(book, stripWhitespace)
  dtm = DocumentTermMatrix(book,
                           control = list(tokenize = BigramTokenizer, stopwords=T,stemming=T))
  dtm = as.data.frame(as.matrix(dtm))
  bigrams_usage = apply(dtm, MARGIN=2, FUN=function(x){ sum(!is.na(x) & x > 0) })
  filtered = bigrams_usage[which(bigrams_usage!=nrow(dtm))]
  sorted = sort(filtered,decreasing=T)[1:2950]
  bigrams_freq = names(sorted)
  dtm = dtm[,bigrams_freq]
  return(dtm)
}

########################## Power features matrix ##########################

######################### Word+Power features matrix ########################


