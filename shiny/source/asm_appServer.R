# future work: modularize this the 
# way we have with the GUI

appServer <- function(input,output,session){
 
# Set up reactive values
rv <- reactiveValues()
  
      # DATABASE INTERACTIONS
      rv$compoundSelected <- 1;
      rv$energySelected <- minEnFolder; 
      rv$RefLibrary_rv <- initial_RefLibrary
      rv$RefLibSize_rv <- initial_RefLibSize
      rv$RefLibSelect <- Libraries[1]
      
      # Identify indices for each column in the library data table (for calling in app)
      rv$LibraryCats <- LibraryCats
      rv$iCode <-iCode 
      rv$iName <- iName 
      rv$iFormula <- iFormula
      rv$iInChIKey <- iInChIKey
      rv$iPrecursorMZ <- iPrecursorMZ
      rv$iDimerProb <- iDimerProb
      rv$iSmiles <- iSmiles
      rv$iPotentialErrors <- iPotentialErrors 
      rv$iMassCaliError <- iMassCaliError
      rv$iPotentialErrorsFM1 <- iPotentialErrorsFM1
      rv$iExactMass <- iExactMass
      rv$iStructure <- iStructure
      rv$iNoiseMetric <- iNoiseMetric
      rv$iBP <- iBP

      rv$iEnergies <- iEnergies
      rv$minEnFolder <- minEnFolder

      
      observeEvent(input$RefLibSelect,{
        a = input$RefLibSelect
        #cat(paste0(a,"\n\n"))
        rv$RefLibrary_rv = readRDS(paste0("Libraries/",a))
        rv$RefLibSize_rv = dim(rv$RefLibrary_rv);
        rv$RefLibSelect = input$RefLibSelect

        # Identify indices for each column in the library data table (for calling in app)
        rv$LibraryCats = colnames(rv$RefLibrary_rv)
        rv$iCode = which(rv$LibraryCats=="Code")
        rv$iName = which(rv$LibraryCats=="Name")
        rv$iFormula = which(rv$LibraryCats=="Formula")
        rv$iInChIKey = which(rv$LibraryCats=="InChIKey_gen")
        rv$iPrecursorMZ = which(rv$LibraryCats=="PrecursorMZ_gen")
        rv$iDimerProb = which(rv$LibraryCats=="DimerProb")
        rv$iSmiles = which(rv$LibraryCats=="SMILES")
        rv$iPotentialErrors = which(rv$LibraryCats=="PotentialErrors")
        rv$iMassCaliError = which(rv$LibraryCats=="MassCaliError")
        rv$iPotentialErrorsFM1 = which(rv$LibraryCats=="PotentialErrorsFM1" )
        rv$iExactMass = which(rv$LibraryCats=="AccurateMass_gen")
        rv$iStructure = which(rv$LibraryCats=="Structure_gen")
        rv$iNoiseMetric = which(rv$LibraryCats=="NoiseMetric")
        rv$iBP = which(rv$LibraryCats=="BP")

        rv$iEnergies = which(rv$LibraryCats=="Energies")
        rv$minEnFolder = rv$RefLibrary_rv[1,rv$iEnergies,with=FALSE][[1]][[1]][1]
        
        rv$compoundSelected <- 1

      })

## =============================================================================     
## DATABASE VIEWER SOURCE CODE          
## =============================================================================    
  output$Library <- DT::renderDataTable(DT::datatable({
    RefLibrary = rv$RefLibrary_rv
    iName <- rv$iName
    iFormula <- rv$iFormula
    iPrecursorMZ <- rv$iPrecursorMZ
    iCode <- rv$iCode
    
    a = RefLibrary[,c(iName,iFormula,iPrecursorMZ,iCode),with=FALSE]
    colnames(a)[3] = "[M+H]<sup>+</sup>";
    a[,3] = round(a[,3],4)
    return(a)

  }),server = FALSE, escape=FALSE,
  selection=list(mode='single', selected=1),
  options = list(lengthMenu = c(15, 30, 60),
                 pageLength = 15, scrollX=TRUE))

  observeEvent(input$Library_rows_selected, {
    RefLibrary = rv$RefLibrary_rv
    minEnFolder = rv$minEnFolder
    iPrecursorMZ = rv$iPrecursorMZ
    k = input$Library_rows_selected
    rv$compoundSelected <- k
    rv$energySelected <- minEnFolder; #"+30 V";

    rv$maxXval <- ceiling(as.numeric(RefLibrary[k,iPrecursorMZ,with=FALSE][[1]]))

  })

  output$LibraryData <- renderTable({
    RefLibrary = rv$RefLibrary_rv
    iCode = rv$iCode
    iName = rv$iName
    iFormula = rv$iFormula
    iExactMass = rv$iExactMass
    iInChIKey = rv$iInChIKey
    iDimerProb = rv$iDimerProb
    iPotentialerrors = rv$iPotentialErrors
    iMassCaliError = rv$iMassCaliError
    iPotentialErrorsFM1 = rv$iPotentialErrorsFM1
    iNoiseMetric = rv$iNoiseMetric
    
    k = rv$compoundSelected
    j = rv$energySelected;

    i = which(unlist(RefLibrary[k,Energies])==j);
    
    
    code      = RefLibrary[k,iCode,with=FALSE][[1]]
    name      = RefLibrary[k,iName,with=FALSE][[1]]
    formula   = RefLibrary[k,iFormula,with=FALSE][[1]]
    emass     = RefLibrary[k,iExactMass,with=FALSE][[1]]
    inchi     = RefLibrary[k,iInChIKey,with=FALSE][[1]]
    dimerProb = RefLibrary[k,iDimerProb,with=FALSE][[1]]
    potError  = RefLibrary[k,iPotentialErrors,with=FALSE][[1]]
    MCerror   = RefLibrary[k,iMassCaliError,with=FALSE][[1]]
    FM1error   = RefLibrary[k,iPotentialErrorsFM1,with=FALSE][[1]]
    NoiseRatio = RefLibrary[k,iNoiseMetric,with=FALSE][[1]][[1]][i]
    
    if (dimerProb>0.90){
      commentMessage = "Possible dimer"
    } else {
      commentMessage = "None"
    }
    
    contributor_code = strsplit(code,"")[[1]][1]
    if(contributor_code=="V"){
      contributor = "Virginia Dept. of Forensics Science"
    } else if (contributor_code=="a"){
      contributor = "For Manuscript - NIST"
    } else {
      contributor = "NIST"
    }
    
      errorMessage = paste("BP (30V) mz error: ",round(-as.numeric(MCerror),4),sep="")
    
    if (FM1error > 0){
      FMerrorMessage1 = "Possible fragmentation inconsistency."
    } else {
      FMerrorMessage1 = "None"
    }
      
    if (dev_mode==1){
    
      Message = as.data.frame(array(0,dim=c(10,1)));
      rownames(Message) <- c("Code:",
                             "Name:",
                             "Formula:",
                             "Exact Mass [Da]:",
                             "InChIKey:",
                             "Contributor:",
                             "Comment:",
                             "Dev. Comment 1 (BP):",
                             "Dev. Comment 2 (FC):",
                             "Dev. Comment 3 (NR):");
      Message[1,1] = code
      Message[2,1] = name
      Message[3,1] = formula
      Message[4,1] = round(as.numeric(emass),4)
      Message[5,1] = paste("<a href='http://www.ncbi.nlm.nih.gov/sites/entrez?cmd=search&db=pccompound&term=\"",inchi,"\"[InChIKey]' target='_blank'>", inchi, "</a>")
      Message[6,1] = contributor
      Message[7,1] = commentMessage
      Message[8,1] = errorMessage
      Message[9,1] = FMerrorMessage1
      Message[10,1] = round(as.numeric(NoiseRatio),3)
      
    } else {
      
      Message = as.data.frame(array(0,dim=c(6,1)));
      rownames(Message) <- c("Name:",
                             "Formula:",
                             "Exact Mass [Da]:",
                             "InChIKey:",
                             "Contributor:",
                             "Comment:");
      Message[1,1] = name
      Message[2,1] = formula
      Message[3,1] = round(as.numeric(emass)+0.0001,3)
      Message[4,1] = paste("<a href='http://www.ncbi.nlm.nih.gov/sites/entrez?cmd=search&db=pccompound&term=\"",inchi,"\"[InChIKey]' target='_blank'>", inchi, "</a>")
      Message[5,1] = contributor
      Message[6,1] = commentMessage
    
    }

    
    
    
    
    
    
    
    return(Message)

  },rownames=TRUE,colnames=FALSE,sanitize.text.function = function(x) x)
  
  output$LibraryPlot <- renderPlotly({
     RefLibrary = rv$RefLibrary_rv
     j = rv$compoundSelected;
     k = rv$energySelected;

       i = which(unlist(RefLibrary[j,Energies])==k);
       data = as.data.table(RefLibrary[j,PeakLists][[1]][i]); #asm_spec2dt_ref(unlist(RefLibrary[j,PeakLists])[[i]],pmass)
       bpn_data = asm_bpNormalizer(data)
       
       l = length(RefLibrary[j,RefinedAnnotations][[1]][i][[1]])
       annotations = character(l)
       for(m in 1:l){
         o = length(RefLibrary[j,RefinedAnnotations][[1]][i][[1]][m][[1]])
         temp_text = NULL
         for (p in 1:o){
           temp_text = paste(temp_text,RefLibrary[j,RefinedAnnotations][[1]][i][[1]][m][[1]][p],"\n")
         }
         annotations[m] = temp_text
       }
       
       col = character(l)
       for(m in 1:l){
         if(annotations[m]=="  \n"){
           col[m] = "grey"
         } else {
           col[m] = "blue"
         }
       }
       
       bpn_data = cbind(bpn_data,annotations)
    
    # https://felixfan.github.io/ggplot2-remove-grid-background-margin/      
    p <- ggplot(bpn_data,
                aes(x=mz, ymax=ab, ymin = 0, text=paste0("m/z: ",round(mz,4),"\nrel. int.: ",round(ab,3), "\n\nPossible Structure(s):\n", annotations))) 
    p <- p + geom_linerange(color=col) 
    p <- p + labs(title="",x="m/z",y="Relative Intensity, %")
    p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"),
                   axis.title.y = element_text(size = 10),axis.title.x = element_text(size=10))  
    
    ggplotly(p,tooltip=c('text'))
  })

  output$LibraryPlotsUI <- renderUI({
          RefLibrary = rv$RefLibrary_rv
          j = rv$compoundSelected;
          NumSpectra = unlist(RefLibrary[j,NumSpectra]);

          do.call(tabsetPanel, c(id='plottab',lapply(1:NumSpectra, function(i) {
            tabPanel(title=paste0(RefLibrary[j,Energies][[1]][i])
         )
       })))

    })

  observeEvent(input$plottab, {

    k = input$plottab
    rv$energySelected <- k;

    })

  output$structurePlotter_plot <- renderPlot({
    RefLibrary = rv$RefLibrary_rv
    iStructure = rv$iStructure
    
    
    j = rv$compoundSelected;
    struc_data =  RefLibrary[j,iStructure,with=FALSE][[1]][[1]]  
    
    temp = strsplit(struc_data[4],' ')[[1]]
    temp = temp[temp!=""]
    natoms = as.numeric(temp[1])
    nconnections = as.numeric(temp[2])
    s1 = 5; 
    s2 = s1+natoms-1
    atomInfo = seq(s1,s2)
    
    s3 = s2+1
    s4 = s2+nconnections
    connInfo = seq(s3,s4)
    
    x = numeric(natoms)
    y = numeric(natoms)
    c = character(natoms)
    
    p = numeric(nconnections)
    q = numeric(nconnections)
    t = character(nconnections)
    
    for(i in 1:natoms){
      temp2 = strsplit(struc_data[atomInfo[i]]," ")[[1]]
      spaces = which(temp2=="");
      temp2 = temp2[-spaces]
      x[i] = as.numeric(temp2[1])
      y[i] = as.numeric(temp2[2])
      c[i] = temp2[4]
    }
    
    for(i in 1:nconnections){
      temp3 = strsplit(struc_data[connInfo[i]]," ")[[1]]
      spaces = which(temp3=="");
      temp3 = temp3[-spaces]
      p[i] = as.numeric(temp3[1])
      q[i] = as.numeric(temp3[2])
      t[i] = temp3[3]
    }

    
    minX = min(x); maxX = max(x); midX = 0.5*(minX+maxX)
    minY = min(y); maxY = max(y); midY = 0.5*(minY+maxY)
    
    x = x-midX;
    y = y-midY;
    
    plot(x,y,
         xlim=c(min(x),max(x)),
         ylim=c(min(y),max(y)),
         axes=FALSE,
         cex=0.1,
         pch=18,
         col = "white",
         xlab="",ylab="")
    
    offset=0.006*(maxX-minX)
    offset3 = 1.1*offset
    
    for(i in 1:nconnections){
      if(t[i]=="1"){
        segments(x[p[i]],y[p[i]],x[q[i]],y[q[i]])  
      } else if (t[i] =="2"){
        if(x[p[i]] == x[q[i]]){ # vertical
         segments((x[p[i]]-offset3),y[p[i]],(x[q[i]]-offset3),y[q[i]],col="blue")
         segments((x[p[i]]+offset3),y[p[i]],(x[q[i]]+offset3),y[q[i]],col="blue")
        } else {
         segments(x[p[i]],(y[p[i]]-offset),x[q[i]],(y[q[i]]-offset),col="blue")
         segments(x[p[i]],(y[p[i]]+offset),x[q[i]],(y[q[i]]+offset),col="blue")
        }
      } else if (t[i] == "3"){
        if(x[p[i]] == x[q[i]]){
         segments((x[p[i]]-offset3),y[p[i]],(x[q[i]]-offset3),y[q[i]],col="red")
         segments(x[p[i]],y[p[i]],x[q[i]],y[q[i]],col="red")
         segments((x[p[i]]+offset3),y[p[i]],(x[q[i]]+offset3),y[q[i]],col="red")
        } else {
         segments(x[p[i]],(y[p[i]]-offset3),x[q[i]],(y[q[i]]-offset3),col="red")
         segments(x[p[i]],y[p[i]],x[q[i]],y[q[i]],col="red")
         segments(x[p[i]],(y[p[i]]+offset3),x[q[i]],(y[q[i]]+offset3),col="red")
        }
      }
      
    }
    nonC = which(c!="C")
    matplot(x[nonC],y[nonC],col = "white",pch=15,
            cex = 1.5,add=TRUE)
    
    text(x[nonC],y[nonC],c[nonC],cex=0.7)
    
    
  })


  
## =============================================================================    
## DATABASE SEARCH SOURCE CODE  
## =============================================================================
  
   # SEARCH TYPE
   rv$search_type = "ma"
   rv$target_type = "protonated molecule"
   
   # MIXTURE ANALYSIS
   rv$mz_tol = 1.0;
   rv$target_min_ab = 0.15
   rv$numTargets <- 1;
   rv$qInputs <- c(1,0,0,0)
   rv$bpN_Query1 <- NULL
   rv$bpN_Query2 <- NULL
   rv$bpN_Query3 <- NULL
   rv$targets_mz <- NULL
   rv$targets_in <- NULL
   rv$QuerySelected <- "+30 V";

   rv$targets_all_data = NULL;  # for reporting
   rv$report = NULL;            # for reporting
  
  observeEvent(input$searchType,{
    rv$search_type = input$searchType
  }) 
   
  observeEvent(input$lowres,{
      if(input$lowres==TRUE){
        shinyjs::disable("epsilon_0")
        rv$mz_tol = 1
      } else {
        shinyjs::enable("epsilon_0")
        rv$mz_tol = input$epsilon_0
      }

  })

  observeEvent(input$epsilon_0,{
    rv$mz_tol = input$epsilon_0
  })

  observeEvent(input$target_min_ab,{
    rv$target_min_ab = input$target_min_ab
  })
  
  observeEvent(input$target_type,{
    rv$target_type = input$target_type
  })

  trigger_button <- eventReactive(input$DartSearch,{
    
    RefLibrary = rv$RefLibrary_rv
    iPrecursorMZ = rv$iPrecursorMZ
    iBP = rv$iBP
    
    # this is the key search code 
    Query1 = asm_spec2dt(asm_specImport(input$q30$datapath))
    Query2 = asm_spec2dt(asm_specImport(input$q60$datapath))
    Query3 = asm_spec2dt(asm_specImport(input$q90$datapath))
    
    
      rv$bpN_Query1 = asm_bpNormalizer(Query1)  
      rv$bpN_Query2 = asm_bpNormalizer(Query2)  
      rv$bpN_Query3 = asm_bpNormalizer(Query3)  
    
    
      if(rv$search_type=="ma"){
        rv$target_min_ab = input$target_min_ab;
      } else if (rv$search_type == "pc"){
        rv$target_min_ab = 1; # only base peak is considered in a pure compound search
      }
    
    targets = asm_targetMolecules(rv$bpN_Query1,rv$target_min_ab)
    rv$targets_mz = targets[,1]
    rv$targets_in = targets[,2]
    rv$numTargets = min(MAX_TARGETS,length(rv$targets_mz[[1]]));

    epsilon_0 = rv$mz_tol;
    targets = rv$targets_mz[[1]]
    
      withProgress(message = 'Analysis Progress', value = 0, {

       runLength = min(MAX_TARGETS,length(targets));

          for(i in 1:runLength){
             tempData = NULL;
             
             if(rv$target_type=="protonated molecule"){
                cat(paste("Possible Matches for Target ",i," with protonated molecule mz ",round(targets[i],1)," (stage 1 search):\n",sep=""))
                index_of_interest = iPrecursorMZ
             } else if (rv$target_type=="base peak"){
                cat(paste("Possible Matches for Target ",i," with base peak mz ",round(targets[i],1)," (stage 1 search):\n",sep=""))
                index_of_interest = iBP
             }
             
                s1 = as.numeric(RefLibrary[,index_of_interest,with=FALSE][[1]]);
                s2 = which(abs(s1 - as.numeric(targets[i])) < epsilon_0)
             
             tempData = "<p>"
             if(length(s2)>0){
               td = paste0("Potential matches in <strong>",strsplit(rv$RefLibSelect,"\\.")[[1]][1],"</strong> database by <em>",rv$target_type,"</em> m/z targeting:<br>")
               tempData = c(tempData,td)

               for(j in 1:length(s2)){
                 td = RefLibrary[s2[j],Name][[1]];
                 tempData = c(tempData,td)
                 td=": "
                 tempData = c(tempData,td)
                 cat(td)

                 ref_mz = as.numeric(RefLibrary[s2[j],index_of_interest,with=FALSE]); cat(ref_mz); cat("\n")
    
                 
                 if (dev_mode==1){
                    td = paste(" ", ref_mz, " ");
                    tempData = c(tempData,td)
                 }
                 
                 cat(s2[j]);cat("\n")
                 mass_diff = asm_mass_diff(RefLibrary[s2[j],index_of_interest,with=FALSE],
                                           RefLibrary[s2[j],MassCaliError],
                                           targets[i]);
                 
                 if (dev_mode==1){
                    td = paste(" [", round(mass_diff,3), "] ");
                    tempData = c(tempData,td)  
                 }
                 
                 NumRefSpec = as.numeric(RefLibrary[s2[j],NumSpectra][[1]])
                 CEs = RefLibrary[s2[j],Energies][[1]]

                 asm_MF = numeric(NumRefSpec)
                 asm_PE = array(0,dim=c(NumRefSpec,6))  # There are 6 outputs from the Peaks Explained Function
    
                 for(m in 1:NumRefSpec){
                    iCE = CEs[m]
                     if(iCE == "+30 V"){
                       Query = Query1;
                     } else if (iCE == "+60 V"){
                       Query = Query2;
                     } else if (iCE == "+90 V"){
                       Query = Query3;
                     }

                    prot_mol = as.numeric(RefLibrary[s2[j],iPrecursorMZ,with=FALSE][[1]]); 
                    max_mz_consider = prot_mol + epsilon_0; # should we look beyond the prot mol to include some isotopic peaks? (discuss with ED) 
                    RefPeakList = asm_spec2dt_ref(RefLibrary[s2[j],PeakLists][[1]][m],max_mz_consider);  # collect peaks up to a max mz value to avoid computations on noise 
                    
                    if(input$lowres==TRUE){
                      Query = asm_hiRes2lowRes(Query)
                      RefPeakList = asm_hiRes2lowRes(RefPeakList)
                      epsilon_1 = 1e-8;
                    } else {
                      epsilon_1 = epsilon_0;
                    }
                    
                    asm_MF[m] = asm_revMatchFactor(RefPeakList,Query,epsilon_1)
                    asm_PE[m,] = asm_PeaksExplained(RefPeakList,Query,epsilon_1,prot_mol)
     
                 }

            asm_PEs = round(asm_PE[,1],4)
            asm_MBs = round(asm_PE[,2],4)
            asm_NPFs = round(asm_PE[,4],4);
            asm_MFs = round(asm_MF,4)

            weights1 = c(0.3333,0.3333,0.3333)  # should be a tunable parameter fraction of reference abundance explained
            weights2 = c(0.3333,0.3333,0.3333)  # should be a tunable parameter mass bias
            weights3 = c(0.3333,0.3333,0.3333)  # should be a tunable parameter reverse match factor
            weights4 = c(0.3333,0.3333,0.3333)  # should be a tunable parameter fraction of reference number of peaks
            
              aPE = 0; # fraction of reference abundance explained by number of peaks matched
              aMB = 0; # mass bias
              aMF = 0; # reverse match factor
              aNPF = 0; # fraction of reference number of peaks matched
              
              # compute weighted values
              for(m in 1:NumRefSpec){
                aPE = aPE + weights1[m]*asm_PEs[m]
                aMB = aMB + weights2[m]*asm_MBs[m]
                aMF = aMF + weights3[m]*asm_MFs[m]
                aNPF = aNPF + weights4[m]*asm_NPFs[m]
              }
              
              aMB = 1-aMB; # mass bias should be 

              if (dev_mode==1){
                td = "( "
                tempData = c(tempData,td)
                cat(td)

                  for(m in 1:NumRefSpec){
                    td = paste0(asm_NPFs[m]," ");
                    tempData = c(tempData,td)
                    cat(td);
                  }
                  td = ")"
                  cat(td)
                  tempData = c(tempData,td)
              
                  td = paste(" [",round(aNPF,3),"]  ",sep="");
                  tempData = c(tempData,td)
                  cat(td)
                
                td = "( "
                tempData = c(tempData,td)
                cat(td)

                  for(m in 1:NumRefSpec){
                    td = paste0(asm_PEs[m]," ");
                    tempData = c(tempData,td)
                    cat(td);
                  }
                  td = ")"
                  cat(td)
                  tempData = c(tempData,td)
              
                  td = paste(" [",round(aPE,3),"]  ",sep="");
                  tempData = c(tempData,td)
                  cat(td)
                
                td = "( "
                tempData = c(tempData,td)
                cat(td)

                  for(m in 1:NumRefSpec){
                    td = paste0(asm_MFs[m]," ");
                    tempData = c(tempData,td)
                    cat(td);
                  }
                  td = ")"
                  cat(td)
                  tempData = c(tempData,td)
              
                  td = paste(" [",round(aMF,3),"]  ",sep="");
                  tempData = c(tempData,td)
                  cat(td)

                td = " ( "
                tempData = c(tempData,td)
                cat(td)

                  for(m in 1:NumRefSpec){
                    td = paste0(asm_MBs[m]," ");
                    tempData = c(tempData,td)
                    cat(td);
                  }
                  td = ") "
                  cat(td)
                  tempData = c(tempData,td)

                  td = paste(" [",round(aMB,3),"]  ",sep="");
                  tempData = c(tempData,td)
                  cat(td)
              }
              
              score1 = mass_diff*aPE*aMB; # 1-mass diff and 1-aMB accounted for in mass_diff and aMB_respectively
              score1 = round(score1,3);
              score2 = mass_diff*aMF*aMB;
              score2 = round(score2,3);
              td = paste0("FPIE-based Index: <strong>",score1,"</strong>   RevMF-based Index: <strong>",score2,"</strong>")
              tempData = c(tempData,td)
              
              if(asm_PE[1,3]==0){
                td = "<em> (warning: no protonated molecule in query) </em>"
                tempData = c(tempData,td)
              }

              td = "<br>"
              tempData = c(tempData,td)
              cat(td)
            } # for j
        
        } else {
          td = paste0("No matches in <strong>",strsplit(rv$RefLibSelect,"\\.")[[1]][1],"</strong> database by <em>",rv$target_type,"</em> m/z targeting.")
          tempData = c(tempData,td)
          cat(td)
        }

             td = "<p>"
             tempData = c(tempData,td);
             cat("\n")

             rv$targets_all_data[i]=list(tempData)

             incProgress(i/runLength)

          }
      })

    
    return(1)
})

  output$QueryPlots <- renderPlotly({
    trigger_button()
    
    if(rv$QuerySelected == "+30 V"){
      bpn_data = rv$bpN_Query1;  
    } else if (rv$QuerySelected == "+60 V"){
      bpn_data = rv$bpN_Query2;
    } else if (rv$QuerySelected == "+90 V"){
      bpn_data = rv$bpN_Query3;
    }
    
    
    targets = rv$targets_mz;
    target_min_ab = rv$target_min_ab;
    
    
    l = dim(bpn_data)[1];
    col = rep("black",l);
    annotations = rep("",l);
    
    for(i in 1:length(targets[[1]])){
      j = which.min(abs(bpn_data[,mz]-targets[[1]][i]))
      if((abs(bpn_data[j,mz]-targets[[1]][i]) <= rv$mz_tol)){
        if(rv$QuerySelected == "+30 V"){
          col[j] = "red";
          annotations[j] = paste0("Target ",rv$target_type, "\n");
        } else {
          col[j] = "orange";
          annotations[j] = paste0("Target ",rv$target_type, " within error tolerance\n");
        }
        
      }
    }
        
    bpn_data = cbind(bpn_data,annotations);
    p <- ggplot(bpn_data,
                  aes(x=mz, y=ab, ymax=ab, ymin=0, text=paste0(annotations,"m/z: ",round(mz,4),"\nrel. int.: ",round(ab,3))))
    p <- p + geom_linerange(color = col);
    p <- p + labs(title="",x="m/z",y="Relative Intensity, %")
    p <- p + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                   panel.background = element_blank(),axis.line = element_line(color="black"),
                   axis.title.y = element_text(size = 10),axis.title.x = element_text(size=10))
  
    
  if(rv$search_type=="ma"){  
    if(rv$QuerySelected == "+30 V"){
        p <- p + geom_hline(yintercept=99.9*target_min_ab,linetype="dotted",size=0.25,color="purple")
    }
  }
  ggplotly(p,tooltip=c('text'))

  })
  
  output$QueryPlotsUI <- renderUI({
    
    trigger_button();
          
    QueryType = c("+30 V", "+60 V", "+90 V")
    NumSpectra = length(QueryType);

      do.call(tabsetPanel, c(id='plottabQ',lapply(1:NumSpectra, function(i) {
          tabPanel(title=QueryType[i])
        
      })))

  })
  
  observeEvent(input$plottabQ, {

    k = input$plottabQ
    rv$QuerySelected <- k;

    })



  output$rough_out <- renderText({
    trigger_button()
    print(rv$targets_all_data[[2]])
  })

  target_mz <- function(x){
    y = as.numeric(rv$targets_mz[[1]][x])
    return(y)
  }

  target_in <- function(x){
    y = as.numeric(rv$targets_in[[1]][x])
    return(y)
  }

  gen_message <- function(x){
    h = rv$targets_all_data[[x]]
    a = sprintf(h);
    return(a)
  }

  output$targets <- renderUI({

     trigger_button();
     r = rv$numTargets;

      do.call(tabsetPanel, c(id='tab',lapply(1:r, function(i) {
         tabPanel(title=paste0('Target ', i),
                  paste0('Mass-to-charge: ',sprintf("%.3f",target_mz(i))),
                  br(),
                  paste0('Relative intensity: ',sprintf("%0.1f",target_in(i))," %."),
                  br(),
                  br(),
                  HTML(gen_message(i)),
                  br()
         )
       })))

  })

    
    
  
  ## future work: report writing feature
  if(FALSE){
  output$create_report <- renderUI({

    trigger_button()
    tagList(
      radioButtons('format','Report format', c('PDF','Word'),
                 inline=TRUE),
      downloadButton('downloadReport',label="Download Report",style='padding:10px; font-size:120%')
    )
  })
  }
  
  
  session$onSessionEnded(function() {
    stopApp()
  })  
  
}