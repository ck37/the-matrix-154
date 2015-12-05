library(tm)
library(SnowballC)

child = docs$child
history = docs$history
religion = docs$religion
science = docs$science
docs2 = c(child,history,religion,science)

a = docs$child[1:100]
b = docs$child[1:100]
c = docs$religion[1:100]
d = docs$religion[1:100]
aa = c(a,b,c,d)

#please load packages written in example.R
load("~/Desktop/154/data/imported-text-docs.Rdata")

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
stopwords = load_stopwords()

# Derive a dictionary of words/ bigrams and total number of their appearances through out the whole dataset.
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
  dtm = as.data.frame(as.matrix(dtm))
  return(dtm)
}
dtm = clean_documents(dd,stopwords)
View(dtm)


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
new = power_features_bigrams(aa,stopwords)
View(new)
d