library(tm)
library(SnowballC)
library(stringi)
library(stringr)
load("~/Desktop/154/data/imported-text-docs.Rdata")

sample = docs$child[1:3]
n = length(sample)

tm_map(sample[1], sent_detect)

power = function(doc){
  n = length(doc)
  power = matrix(NA,nrow=n,ncol=3,dimnames=list(seq(1,n)))
  for(i in 1:n){
    book = doc[i]
    book = tm_map(book, content_transformer(tolower))
    book = tm_map(book, stripWhitespace)
    book = tm_map(book, stemDocument)
    book = tm_map(book, removePunctuation)
    ########## how to read in actual text file
    text = as.character(book)
  
    ### 3.# of 4-digit number
    fourdigit = str_extract(text, "\\d{4}")
    fourdigit2 = na.omit(fourdigit)
    power3 = length(fourdigit2)
  
    ######################
    ### 5. # of numbers
    #power5 = sum(grepl("[[:digit:]]", substring(x,seq(1,nchar(x),1),seq(1,nchar(x),1))))
  
    ### 9. find # of '_'
    #power9 = length(gregexpr('_', test)[[1]])
  
    ### 10(1). finding number of words in each txt file
    #power10_1 = sum(stri_count(text.file,regex="\\S+"))
  
    ### 10(2). finding number of sentence in each txt file
    #power10_2 = length(gregexpr('[[:alnum:] ][.!?]', text)[[1]])
  
    title = names(book)
    power[i,] = as.matrix(cbind(power1,power2,power3))
    rownames(power)[i] = title
  }
  colnames(power) = c("power1","power2","power3")
  return(power)
}

power(sample)

power_feature = list()

for (type in names(docs)) {
  power_feature[[type]] = power(docs[[type]])
}

##############################
library(openNLP)
require(NLP)
library(tm)

convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence
  # detector employing the default model for language 'en'.
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  # Convert text to class String from package NLP
  text <- as.String(text)
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  # Extract sentences
  sentences <- text[sentence.boundaries]
  # return sentences
  return(sentences)
}

sent = convert_text_to_sentences(sample[1])

tm_map(sample[1],sent_detect)




