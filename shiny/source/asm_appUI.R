source('source/GUI/asm_GUI_DBV.R')   # Database Viewer
source('source/GUI/asm_GUI_DST.R')   # Database Search
 

appUI <- shinyUI({

  fixedPage(theme=shinytheme("flatly"),
          navbarPage("NIST DART-MS",
               asm_GUI_DBV,
               asm_GUI_DST
          )
  )
  
})