#######################
# average word length # 
#######################
ave.word.length = function(docs){
 
  r = numeric()
  c = numeric()
  
  for(i in 1:length(docs[ ,1])){     #of rows
    
    for(j in 1:length(docs[1, ])){    #of col
    
    c[j] = nchar(colnames(docs[j]))
    
    }
   
    r[i] = mean(c)
  }
   
return(r)
}

############################
# number of distinct words #
############################




