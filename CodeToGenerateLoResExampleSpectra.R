# script for constructing low res approximations of spectra in Select Library
rm(list=ls())
source("shiny/source/asm_externalPackages.R")

functions = list.files("shiny/source/Functions/")
for(i in 1:length(functions)){
     file = paste0("shiny/source/Functions/",functions[i]);
     source(file)
}


Library = paste0("shiny/Libraries/SelectLibrary-ForPaper.RDS");
RefLibrary = readRDS(Library)

epsilon_0 = 1L; #because integer resolution

nCompounds = dim(RefLibrary)[1]

for(j in 1:nCompounds){
     max_k = RefLibrary[j,NumSpectra]
     for(k in 1:max_k){
          max_mz_consider = as.numeric(RefLibrary[j,PrecursorMZ_gen][[1]]) + epsilon_0;  # do we want to include some isotopes of the protonated molecule?
          RefPeakList = asm_spec2dt_ref(RefLibrary[j,PeakLists][[1]][k],max_mz_consider)
          a = asm_hiRes2lowRes(RefPeakList)

          filename = paste0("lowres-",RefLibrary[j,Name],"_",RefLibrary[j,Energies][[1]][k],"_computerSimulated.txt")
          sink(filename)
               for(i in 1:dim(a)[1]){cat(paste0(a[i,mz],"\t",a[i,ab],"\n"))}
          sink()      
     }
}



