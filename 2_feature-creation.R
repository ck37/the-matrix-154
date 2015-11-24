# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

library(tm)
library(SnowballC) # for stemming

# Load the docs file if it doesn't already exist.
if (!exists("docs")) {
  load("data/imported-text-docs.Rdata")
}

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
clean_documents = function(book, stopwords = c()) {
  book = tm_map(book, content_transformer(tolower))
  book = tm_map(book, removeWords, c('project','gutenberg','ebook','title','author','release','chapter'))
  if (length(stopwords) > 0) {
    book  = tm_map(book, removeWords, stopwords)
  }
  # NOTE: we should double-check this removePunctuation code, because it may remove the punctation
  # without substituting spaces, which will mess up the words.
  dtm = DocumentTermMatrix(book,
    control = list(tolower=T, stopwords=T, removePunctuation=T, removeNumbers=T, stemming=T)
  )
  dtm = removeSparseTerms(dtm, .99)
  # Let's hold off on this part for now.
  #dtm = as.data.frame(as.matrix(dtm))
  return(dtm)
}

result = clean_documents(sample)
# View(result)


### Exclude the common word features (known as stop words) listed in
# http://www.textfixer.com/resources/common-english-words.txt

# Load the official stopword list and make sure it's the same as the one used by tm.
file_con = file("inbound/common-english-words.txt")
# Process it as one line separated by commas, and convert it to a vector.
stopwords = unlist(strsplit(readLines(file_con)[1], split=c(","), fixed=T))
save(stopwords, file="data/stopwords.Rdata")
close(file_con)
rm(file_con)
stopwords

# Check if it's the same list as the one used by tm()
length(stopwords)
length(stopwords("english"))
# Nope, these are not the same size.

# Only 70% of the official stopwords are in the tm list, so we'll need to also use this one.
mean(stopwords %in% stopwords("english"))

### Exclude common words

cleaned_docs = list()

# Time this step because it's slow.
system.time({
  for (type in names(docs)) {
    cat("Cleaning", type, "\n")
    cleaned_docs[[type]] = clean_documents(docs[[type]], stopwords)
  }
})

rm(docs, stopwords, type)
# Free up unused memory.
gc()


### Tagging - now we need to combine them into a single dataframe.

# This creates our target/response vector as a factor with values of child, history, religion, or science.
targets = as.factor(unlist(sapply(names(cleaned_docs), simplify="array", USE.NAMES=F,
                 FUN=function(doc_type){ rep(doc_type, length.out=nrow(as.matrix(cleaned_docs[[doc_type]])))})))
table(targets)
length(targets)

# Combine docs into a single dataframe.
system.time({
  docs = as.data.frame(as.matrix(do.call(tm:::c.DocumentTermMatrix, cleaned_docs)))
})
# Clean up memory.
rm(cleaned_docs)

dim(docs)
# Make sure that the # of rows in the document corpus equals the length of our target vector.
# Otherwise stop and give an error.
stopifnot(nrow(docs) == length(targets))

save(docs, targets, file="data/cleaned-docs.Rdata")

# Clean up lingering variables.
rm(index1, index2, result, sample, sample1, sample2, child)

gc()

# Stop logging.
sink()