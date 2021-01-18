## asm_jsp2txt file converter prototype function
## Author: Arun S. Moorthy, arun.moorthy@nist.gov
## Initial Date: June 30th, 2020
## Revision Number: 0
## Brief Description: Given a directory, this script identifies all the jsp 
## files in the subdirectories and creates txt files.
# ==============================================================================

asm_jsp2txt <- function(main_folder){

     EnergyFolders = list.dirs(main_folder,recursive = FALSE)
     for(i in 1:length(EnergyFolders)){
          InFolderSpectra = list.files(EnergyFolders[i],pattern = ".jsp")
          if(length(InFolderSpectra)>0){
               for(j in 1:length(InFolderSpectra)){
                    # cat(paste(i,"\t", j, "\n", sep=""))
                    jsp = paste(EnergyFolders[i],"/",InFolderSpectra[j],sep="")
                    file_name = strsplit(InFolderSpectra[j],".jsp")[[1]][1]
                    txt = paste(EnergyFolders[i],"/",file_name,".txt",sep="")
                    
                    if(file.exists(txt)){
                         next
                    } else {
                         data = readLines(jsp)
                         start = grep("##DATA=",data)+1
                         end = grep("##END",data)-1
               
                         peak_data = data[start:end]
                    
                         sink(txt)
                         cat(peak_data,sep="\n")
                         sink()     
                    }
               }
               # cat("\n")
          } else {
               next
          }
     }

} 