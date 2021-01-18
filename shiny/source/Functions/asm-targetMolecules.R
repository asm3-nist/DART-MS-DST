asm_targetMolecules <- function(spec,tau){
  
  x = spec
  
  bp = -sort(-x[,2][[1]])[1]

  t1 = which(x[,2]>=(tau*bp))
  t1 = t1[order(-x[t1,2])]
  targets = x[t1,]
  
  return(targets)
  
}