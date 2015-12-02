library(tm)
library(SnowballC)
library(stringi)
library(stringr)
library(openNLP)
require(NLP)
library(qdap)
library(dplyr)

load("~/Desktop/154/data/imported-text-docs.Rdata")

### sample data to play with
sample = docs$child[1:3]

### external function(1): convert_text_to_sentences
convert_text_to_sentences <- function(text, lang = "en") {
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  text <- as.String(text)
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  sentences <- text[sentence.boundaries]
  return(sentences)
}

### external function(2): removes punctuations
removepunc = function(x){
  gsub("[[:punct:]]", "", x)
}

### main function: generates sentence-based power feature matrix
power = function(doc){
  n = length(doc)
  power = matrix(NA,nrow=n,ncol=4,dimnames=list(seq(1,n)))
  
  for(i in 1:n){
    book = doc[i]
    book2 = tm_map(book, content_transformer(tolower))
    book3 = tm_map(book2, stripWhitespace)
    book4 = tm_map(book2, stemDocument)

    text = as.data.frame(book4)[2]
    sents = convert_text_to_sentences(text)
    sents2 = lapply(sents,removepunc)
  
    ### number of sentence
    power1 = length(sents2)
    
    ### average length of sentence
    power2 = sum(stri_count(sents2,regex="\\S+"))/length(sents2)
    
    ### number of 4-digit number
    power3 = length(na.omit(str_extract(sents2, "\\d{4}")))

    ### number of digits
    power4 = sum(grepl("[[:digit:]]", sents2))
    
    ### 
  
    title = names(book)
    power[i,] = as.matrix(cbind(power1,power2,power3,power4))
    rownames(power)[i] = title
  }
  colnames(power) = c("power1","power2","power3","power4")
  return(power)
}

sample_result = as.data.frame(power(sample))


power_feature = list()

for (type in names(docs)) {
  power_feature[[type]] = power(docs[[type]])
}

#merge(d, e, by=0, all=TRUE) 

