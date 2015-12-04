load("~/Desktop/154/data/imported-text-docs.Rdata")
book = docs$history[1:10]

library(tm)
library(SnowballC)
library(RWeka)
library(stringi)
library(stringr)
library(openNLP)
library(NLP)
library(qdap)
library(dplyr)
library(foreach)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
options(mc.cores=1)

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

power_features_bigrams = function(book, stopwords = c()) {
  book = tm_map(book, content_transformer(tolower))
  book = tm_map(book, stripWhitespace)
  book = tm_map(book, removeWords, c('project','gutenberg','ebook','title','author','release','chapter','posting','editor','translator','encoding','ascii','updated'))
  if (length(stopwords)>0){
    book  = tm_map(book, removeWords, stopwords)
  }
  dtm = DocumentTermMatrix(book,
                           control = list(tokenize = BigramTokenizer,tolower=T, stopwords=T, removePunctuation=T, removeNumbers=T, stemming=T))
  dtm = removeSparseTerms(dtm,.99)
  dtm = as.data.frame(as.matrix(dtm))
  return(dtm)
}
result = power_features_bigrams(book,stopwords)
View(result)

# 지금 고쳐야하는것: 단어하나짜리들 몇개가 space 때문에 bigram으로 인식되고있는데 어차피 다른파일들에서는 반복되지 않을 것 같아서
# 필터링 하고나면 없어질거같기도해요
# 어떻게 필터링해야할지 아이디어좀 ㅠㅠ
