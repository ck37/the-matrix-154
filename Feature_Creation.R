library(tm)

################## Exploring docs ##################
### docs is a list of 4 groups "child", "history", "religion", "science"
### info of the first file of the group "child"
child = docs$child
length(child)
inspect(child[1])
### see content of the first file of the group "child"
as.character(child[1])

################# Feature Creation ###############

# Generating small sample
index1 = sample(length(docs$child),10)
index2 = sample(length(docs$history),10)
sample1 = docs$child[index1]
sample2 = docs$history[index2]
sample = c(sample1,sample2)

# Derive a dictionary of words and total number of their appearances through out the whole dataset.
myfun = function(book){
  book = tm_map(book,content_transformer(tolower))
  book = tm_map(book,removeWords,c('project','gutenberg','ebook','title','author','release','chapter'))
  dtm = DocumentTermMatrix(book,
                           control = list(tolower=T,stopwords=T,removePunctuation=T,removeNumbers=T)
                           )
  dtm = removeSparseTerms(dtm,.99)
  dtm = as.data.frame(as.matrix(dtm))
  return(dtm)
}

result = myfun(sample)
View(result)


### Exclude the common word features (known as stop words) listed in http://www.textfixer.com/resources/common-english-words.txt
### Exclude common words
### Tagging
