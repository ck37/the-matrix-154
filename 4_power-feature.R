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

############## Helen
### 5.
sum(grepl("[[:digit:]]", substring(x,seq(1,nchar(x),1),seq(1,nchar(x),1))))


############## Dan
#8 length of single txt file or # of words in a single txt file
if (!exists("stringi")) {install.packages("stringi?re")}
library(stringi)

#9 find # of '_'

test = "hahaa_what??__lala"
length(gregexpr('_', test)[[1]])




#10 
#finding number of words in each txt file
if (!exists("stringi")) {install.packages("stringi?re")}
library(stringi)
text.file <- c(
  "this is a string current trey",
  "nospaces",
  "multiple    spaces",
  "   leadingspaces",
  "trailingspaces    ",
  "    leading and trailing    ",
  "just one space each")
sum(stri_count(text.file,regex="\\S+"))

#finding number of sentence in each txt file
text <- 'Hello world!! Here are two sentences for you... how does this work?'
length(gregexpr('[[:alnum:] ][.!?]', text)[[1]])



