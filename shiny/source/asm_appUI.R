source('source/GUI/asm_GUI_DBV.R')   # Database Viewer
source('source/GUI/asm_GUI_DST.R')   # Pure Compound Search
 

appUI <- shinyUI({

  fixedPage(theme=shinytheme("flatly"),
          navbarPage("NIST DART-MS",
               asm_GUI_DBV,
               asm_GUI_DST
          )
  )
  
})