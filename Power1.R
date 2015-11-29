library(tm)
library(stringi)
library(stringr)
load("~/Desktop/154/data/imported-text-docs.Rdata")

index1 = sample(length(docs$child),10)
index2 = sample(length(docs$history),10)
sample1 = docs$child[index1]
sample2 = docs$history[index2]
sample = c(sample1,sample2)
sample

clean = function(book){
  book = tm_map(book, content_transformer(tolower))
  book = tm_map(book, stripWhitespace)
  book = tm_map(book, stemDocument)
  book = tm_map(book, removePunctuation)
  return(book)
}

a = clean(sample)
text = as.character(a[1])
split = strsplit(text,"\\s+")[1]
split ####### when you print this, you will see that there still is backslash and quotes

### 1.average word length
stri_count(text,regex="\\S+") #4151 words
len = sapply(split,nchar) #4151 words
mean(len)

### 2. # of unique words used
uniq = sapply(split,unique) #1681 unique words
length(uniq)

### 3. # of 4-digit number

str12 <- "coihr 1234 &/()= jngm 34 ljd"
fourdigit = str_extract(text, "\\d{4}")
fourdigit
fourdigit2 = na.omit(fourdigit)
fourdigit2
length(fourdigit2)
