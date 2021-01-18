asm_specImport <- function(DataPath){
  
  if(FALSE){
    data = readLines(DataPath) 
    i.PeaksStart = grep("##DATA=",data)+1
    i.PeaksEnd = grep("##END=",data)-1;
    PeakList = list(data[i.PeaksStart:i.PeaksEnd])
  }
  
  data = readLines(DataPath)
  PeakList = list(data[1:length(data)])
  
  
  
  return(PeakList)
  
}