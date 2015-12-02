power_dtm = function(dtm){
  ### power1: returns the vector of average word length of each txt file
  power1 = numeric()
  c = numeric()
  for(i in 1:length(dtm[ ,1])){    
    for(j in 1:length(dtm[1, ])){    
      c[j] = nchar(colnames(dtm[j])) * dtm[i,j]
    }
    power1[i] = sum(c)/ sum(dtm[i, ])
  }
  
  ### power2: returns the vector showing how many distinct words used in each file
  power2 = numeric()
  for(i in 1:length(dtm[ ,1])){    
    total = 0
    for(j in 1:length(dtm[1, ])){
      if(dtm[i,j] >= 1){
        total = total + 1
      }  
    }
    power2[i] = total
  }
  
  ### power3: total # of words
  power3 = numeric()
  for(i in 1:length(dtm[ ,1])){    
    total = sum(dtm[i, ])
    power3[i] = total  
  }
  
  ### power4: total # of characters
  power4 = numeric()
  c2 = numeric()
  for(i in 1:length(dtm[ ,1])){    
    for(j in 1:length(dtm[1, ])){    
      c2[j] = nchar(colnames(dtm[j])) * dtm[i,j]
    }
    power4[i] = sum(c2)
  }
  dtm$power1 = power1
  dtm$power2 = power2
  dtm$power3 = power3
  dtm$power4 = power4
  return(dtm)
}

