library(data.table)
asm_spec2dt_ref <- function(x,mz_max){
  
  mz = x[[1]][,1]
  ab = x[[1]][,2]
  
  a = which(mz>mz_max)
  if(length(a)>0){
    mz = mz[-a]
    ab = ab[-a]  
  }
  
  spec_dt = as.data.table(cbind(mz,ab));
  
  return(spec_dt)
  
}