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
    r[i] = sum(c)/ sum(docs[i, ])
  }
return(r)               # returns the vector of average word length of each txt file
}

############################
# number of distinct words #
############################

distinct.words = function(docs){
  
  for(i in 1:length(docs[ ,1])){    
    
    total = 0
    for(j in 1:length(docs[1, ])){
      if(docs[i,j] >= 1){
        total = total + 1
      }  
    }
    
  r[i] = total
    
  }
  return(r)   #returns the vector showing how many distinct words used in each file
}

#####################    
# total # of words ##
#####################
total.words = function(docs){
  
  for(i in 1:length(docs[ ,1])){    
    
    total = 0
    total = total + sum(docs[i, ])
    r[i] = total  
      
  }
  return(r)  #returns the vector of length 22308 with total words used
}

#########################
# total # of characters #
#########################
total.char = function(docs){
  
  r = numeric()
  c = numeric()
  
  for(i in 1:length(docs[ ,1])){    
    
    for(j in 1:length(docs[1, ])){    
      
      c[j] = nchar(colnames(docs[j])) * docs[i,j]
      
    }
    r[i] = sum(c)
  }
  return(r)     #returns the vector of length 22308 each row showing # of characters used in each file
}



