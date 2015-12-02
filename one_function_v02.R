library(tm)
library(SnowballC)

feature_matrix = function(file_dir, stop_dir){
    
  docs = list()
  dtm = list()
  doc_dirs = list(child="Child_0", history="History_1", religion="Religion_2", science="Science_3")
  stopwords = unlist(strsplit(readLines(file(stop_dir))[1], split=c(","), fixed=T))
  
  for (type in names(doc_dirs)) {
    docs[[type]] = Corpus(DirSource(paste0(file_dir, doc_dirs[[type]])))
}
  rm(doc_dirs)

  for (type in names(docs))  {
    docs[[type]] = tm_map(docs[[type]], content_transformer(tolower))
    docs[[type]] = tm_map(docs[[type]], removeWords, 
                          c('project','gutenberg','ebook','title','author','release','chapter', 'posting', 'editor', 'translator', 'encoding', 'ascii', 'updated'))

    docs[[type]] = tm_map(docs[[type]], removeWords, stopwords)
    dtm[[type]] = DocumentTermMatrix(docs[[type]], control = list(tolower=T, stopwords=T, 
                                                                   removePunctuation=T, removeNumbers=T, stemming=T))
    dtm[[type]] = removeSparseTerms(dtm[[type]], .99)  
     
    targets = as.factor(unlist(sapply(names(dtm), simplify="array", USE.NAMES=F,
                                      FUN=function(doc_type){ rep(doc_type, length.out=nrow(as.matrix(dtm[[doc_type]])))})))
  }
    mat = as.data.frame(as.matrix(do.call(tm:::c.DocumentTermMatrix, dtm)))

    word_usage = apply(mat, MARGIN=2, FUN=function(x){ sum(!is.na(x) & x > 0)})
    cutoff_high = round(nrow(mat) * 0.8)
    cutoff_low = 50
    
    mat = mat[, !colnames(mat) %in% names(word_usage[word_usage > cutoff_high]) ]
    mat = mat[, !colnames(mat) %in% names(word_usage[word_usage < cutoff_low]) ]
    
    word_usage = word_usage[word_usage >= cutoff_low & word_usage <= cutoff_high]
    
    feature_matrix = mat

    return(feature_matrix)
}


file_dir = "inbound/Training/"
stop_dir = "inbound/common-english-words.txt"

f_mat = feature_matrix(file_dir, stop_dir)