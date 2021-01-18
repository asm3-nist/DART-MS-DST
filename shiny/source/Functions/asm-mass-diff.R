asm_mass_diff <- function(preMZ, MCE, tMZ){
  
  # when the mass calibration error is way off in the library, this can create undesirable results.
  # example 1X3X_30V.txt searched against Library v1.
  # mass_diff = abs( as.numeric(preMZ) - as.numeric(MCE) - as.numeric(tMZ) ) 
  mass_diff = abs( as.numeric(preMZ) - as.numeric(tMZ) )
  mass_diff = 1 - mass_diff
  mass_diff = max(mass_diff,0)
  
  return(mass_diff)
}