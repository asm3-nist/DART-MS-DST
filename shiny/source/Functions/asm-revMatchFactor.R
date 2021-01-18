asm_revMatchFactor <- function(x,y,tau_ma){
  
    y_mz = as.numeric(y[,1][[1]])
    y_ab = as.numeric(y[,2][[1]])
    y_ab = y_ab/max(y_ab) # normalize query
    
    x_mz = as.numeric(x[,1][[1]])
    x_ab = as.numeric(x[,2][[1]])
    x_ab = x_ab/max(x_ab) # normalize reference
  
    n = length(x_ab)
    vector_for_MF_ref = numeric(n)
    vector_for_MF_que = numeric(n)
 
    for(i in 1:n){
      s1 = abs(y_mz-x_mz[i])
      s2 = which(s1 <= tau_ma)
      
      if (length(s2)==0){
        #cat("sitch length(s2) == 0\n")
        vector_for_MF_ref[i] = x_ab[i];
        vector_for_MF_que[i] = 0;
      } else {
        vector_for_MF_ref[i] = x_ab[i];
        #cat(paste0(length(s2),"\n"))
        #a = y_ab[s2]-x_ab[i]
        #b = which.min(a);
        b = which.min(s1[s2])
        vector_for_MF_que[i] = y_ab[s2[b]];
      }
    }
  

  num = sum(as.numeric(vector_for_MF_ref)*as.numeric(vector_for_MF_que))
  den = sqrt(sum(as.numeric(vector_for_MF_ref)^2)) * sqrt(sum(as.numeric(vector_for_MF_que)^2))
 
   if(den!=0){
    MF = num/den  
  } else {
    MF = 0
  }
  
  results = c(MF)
  
  return(results)
}