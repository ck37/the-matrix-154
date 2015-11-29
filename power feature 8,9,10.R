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


