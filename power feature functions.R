#######################
# average word length # 
#######################
ave.word.length = function(docs){
 
  r = numeric()
  c = numeric()
  
  for(i in 1:length(docs[ ,1])){    
    
    for(j in 1:length(docs[1, ])){    
        
      c[j] = nchar(colnames(docs[j])) * docs[i,j]
      
    }
    r[i] = mean(c)
  }
return(r)               # returns the vector of average word length of each txt file
}

############################
# number of distinct words #
############################


df