# Log the script's output and messages to a text file.
sink(paste0(gsub("\\.[^.]+$", "", basename(sys.frame(1)$ofile)), ".log"), append=F, split=T)
cat("Executing:", sys.frame(1)$ofile, "\nDatetime:", date(), "\n")

# Load the docs file if it doesn't already exist.
if (!exists("docs")) {
  load("data/cleaned-docs.Rdata")
}

# Here we use the sum of how many times a word is used, which can be greater than 1 in a given doc.
# This is not exactly what the final-project PDF wants.
#system.time({
#  word_usage = apply(docs, MARGIN=2, FUN=sum)
#})

# Here we check if the value is not NA and greater than 0.
# This is basically a count of how many documents use the word.
# Takes 42+ seconds to run.
system.time({
  word_usage = apply(docs, MARGIN=2, FUN=function(x){ sum(!is.na(x) & x > 0) })
})
summary(word_usage)
# TODO: save plot to a png file.
hist(word_usage, breaks=30, main="Documents using the word")
dev.copy(png, "visuals/3-feature-filtering-histogram.png")
dev.off()

# How many words do we have right now?
ncol(docs)

# Remove words that are used in at least 80% of documents - 11 words.
cutoff_high = round(nrow(docs) * 0.8)
cutoff_high
sum(word_usage > cutoff_high)
word_usage[word_usage > cutoff_high]
# Remove words that are above the cutoff.
docs = docs[, !colnames(docs) %in% names(word_usage[word_usage > cutoff_high]) ]

# Remove words that are not in at least 25 documents.
cutoff_low = 25
sum(word_usage < cutoff_low)
word_usage[word_usage < cutoff_low]
# Remove words that are above the cutoff.
docs = docs[, !colnames(docs) %in% names(word_usage[word_usage < cutoff_low]) ]

# Update word usage to reflect the revised features.
word_usage  = word_usage[word_usage >= cutoff_low & word_usage <= cutoff_high]
summary(word_usage)

# Clean up environment.
rm(cutoff_high, cutoff_low, word_usage)

# Final dimensions.
dim(docs)

# Save the result.
save(docs, targets, file="data/filtered-docs.Rdata")

gc()

# Stop logging.
sink()