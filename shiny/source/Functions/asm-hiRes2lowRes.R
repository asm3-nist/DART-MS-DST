asm_hiRes2lowRes <- function(x){
     
     mz = x[,1][[1]]
     ab = x[,2][[1]]
     
     mz_int = round(mz)
     mz_int_unique = unique(mz_int)
     ab_int = numeric(length(mz_int_unique))
     
     for(i in 1:length(mz_int_unique)){
          j = which(mz_int==mz_int_unique[i])
          ab_int[i] = sum(ab[j])     
     }
     
     y = as.data.table(cbind(mz_int_unique,ab_int))
     colnames(y)=c("mz","ab")
     
     return(y)
}