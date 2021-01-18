## NIST DART-MS Database Search Tool
## Web Application Code Base
##
## Edward R. Sisco; edward.sisco@nist.gov
## Arun S. Moorthy; arun.moorthy@nist.gov
##
## Version 0.3 (Library Version 0.5)
## Revision Date: Sept 19, 2020
## =============================================================================
rm(list=ls())
# load external libraries and custom written functions
source("source/asm_Header.R")

# Initial Definitions for some key user or reactive variables
MAX_TARGETS = 50
# o.minXval = 50
# o.maxXval = ceiling(as.numeric(RefLibrary[1,iPrecursorMZ,with=FALSE][[1]]))

# load UI and Server code
source('source/asm_appUI.R', local = TRUE)
source('source/asm_appServer.R', local = TRUE)


shinyApp(
  ui = appUI,
  server = appServer
)