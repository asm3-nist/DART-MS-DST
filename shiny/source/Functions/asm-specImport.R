asm_specImport <- function(DataPath){
  
  filetype = strsplit(DataPath,"\\.")[[1]][2]
  
  if(filetype=="jsp"){
    data = readLines(DataPath) 
    i.PeaksStart = grep("##DATA=",data)+1
    i.PeaksEnd = grep("##END=",data)-1
    PeakList = list(data[i.PeaksStart:i.PeaksEnd])
  } else if (filetype=="txt"){
    data = readLines(DataPath)
    PeakList = list(data[1:length(data)])
  } else if (filetype=="csv"){
    rdata = readLines(DataPath)
    data = gsub(",","\t",rdata)
    PeakList = list(data[1:length(data)])
  }
  
  return(PeakList)
  
}