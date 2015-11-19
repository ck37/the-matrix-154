
# Load the docs file if it doesn't already exist.
if (!exists("docs")) {
  load("data/cleaned-docs.Rdata")
}

word_usage = apply(docs, MARGIN=2, FUN=sum)
summary(word_usage)
hist(word_usage, breaks=20)
log_word_usage = log(word_usage)
summary(log_word_usage)
hist(log_word_usage)
