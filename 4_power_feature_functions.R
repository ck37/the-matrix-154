########################## power featrues sentences ##########################

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
  colnames(power) = c("power1","power2","power3","power4")
  return(power)
}

########################## power featrues from dtm ##########################

power_dtm = function(dtm){
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
  dtm$power1 = power1
  dtm$power2 = power2
  dtm$power3 = power3
  dtm$power4 = power4
  return(dtm)
}

###################################################################
merge(a, b, by="row.names", all=TRUE)