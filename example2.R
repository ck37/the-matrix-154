#please load packages written in example.R
load("~/Desktop/154/data/imported-text-docs.Rdata")

dd = docs$child[1:100]
bb = docs$religion[1:100]
cc = docs$history[1:100]
a = c(dd,bb,cc)

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


BigramTokenizer = function(x){
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
options(mc.cores=1)

power_features_bigrams = function(book, stopwords = c()){
  book = tm_map(book, content_transformer(tolower))
  book = tm_map(book, removePunctuation)
  book = tm_map(book, removeNumbers)
  book = tm_map(book, removeWords, c('project','gutenberg','ebook','title','author','release','chapter','posting','editor','translator','encoding','ascii','updated'))
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

new = power_features_bigrams(a,stopwords)
View(new)
