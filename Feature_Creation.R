library(tm)

################## Exploring docs ##################
### docs is a list of 4 groups "child", "history", "religion", "science"
### info of the first file of the group "child"
length(docs$child)
inspect(docs$child[1])
### see content of the first file of the group "child"
as.character(docs$child[1])

################# Feature Creation ###############

child = docs$child
history = docs$history
religion = docs$religion
science = docs$science

sample = child[1]

# Derive a dictionary of words and total number of their appearances through out the whole dataset.
myfun = function(book){
  count = length(book)
  dtm = DocumentTermMatrix(book,
                           # replace non-alphabet characters (“,”, “\n”, etc) in the content with a space character
                           # convert uppercase to lowercase characters
                           control = list(tolower=T,removePunctuation=T))
  return(dtm)
}

myfun(sample)

### remove common words i.e. Project, Gutenberg, eBook, Title, Author, Release, Chapter, etc.
### Exclude the common word features (known as stop words) listed in http://www.textfixer.com/resources/common-english-words.txt

