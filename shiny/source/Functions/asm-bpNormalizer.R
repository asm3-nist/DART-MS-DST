asm_bpNormalizer <- function(x){
  
  bp1 = -sort(-x[,2][[1]])[1]
  bp2 = -sort(-x[,2][[1]])[2]
  
  # if((bp1/bp2)>2){
  #   bp = bp2
  # } else {
  #   bp = bp1
  # }

  bp = bp1
  x[,2] = (100/bp)*x[,2]
  return(x)
}