asm_PeaksExplained <- function(ref,query,tau_ma,prot_mol){
    x = ref
    y = query
  
    n = dim(x)[1]
    totAbRef = sum(as.numeric(x[,2][[1]]));

    PeaksExplained = 0;
    mass_diff_biases = NULL;
    nMatchedPeaks = 0;
 
    for(i in 1:n){
      s1 = abs(as.numeric(y[,1][[1]])-as.numeric(x[i,1]))
      s2 = which(s1 <= tau_ma)
      if (length(s2)!=0){
        PeaksExplained = PeaksExplained + as.numeric(x[i,2]);
        mass_diff_biases = c(mass_diff_biases,min(s1));
        nMatchedPeaks = nMatchedPeaks+1;
      }
    }
  
  PE_perc = PeaksExplained/totAbRef;
  if(nMatchedPeaks > 1){
    MD_bias = sd(as.numeric(mass_diff_biases))  
  } else if (nMatchedPeaks==1){
    MD_bias = as.numeric(mass_diff_biases)
  } else {
    MD_bias = 0.99
  }
  
  
  # check if protonated molecule is in query spectrum
  if(min(abs(as.numeric(y[,1][[1]])-prot_mol)) <= tau_ma){
    flag = 1; # protonated molecule is in query spectrum
  } else {
    flag = 0; # protonated molecule is not in query spectrum
  }
  
  results = c(PE_perc,MD_bias,flag,(nMatchedPeaks/n),nMatchedPeaks,n)
  
  return(results)
}