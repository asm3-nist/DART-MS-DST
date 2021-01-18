asm_spec2dt <- function(x){
  n = length(x[[1]])
  mz = numeric(n)
  ab = numeric(n)
  for(i in 1:n){
    mz[i] = as.numeric(strsplit(x[[1]][i],"\t")[[1]][1])
    ab[i] = as.numeric(strsplit(x[[1]][i],"\t")[[1]][2])
  }
  
  spec = as.data.table(cbind(mz,ab))
  return(spec)
}
