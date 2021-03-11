asm_GUI_DBV <- tabPanel(
  "Viewer",
  
   DevModeMessage,
   DisclaimerMessage,
   EmailMessage,
   
   br(),
   sidebarPanel(
       div(selectInput("RefLibSelect","Reference Library: ",choices=Libraries)),
       div(DT::dataTableOutput("Library"), style = "font-size:70%"),
       div(strong(p("References")),
           p("Sisco et. al. (accepted). Creation and Release of an Updated NIST DART-MS Forensics Database."),
              style="color:black;font-size:8pt")
   ),

   mainPanel(
     fixedRow(
       column(6,
              div(tableOutput("LibraryData"),
              style = "color:black;font-size:8pt")
              ),
       column(2,
              plotOutput("structurePlotter_plot",height="250px", width = "250px")
              )
     ),
     # div(tableOutput("LibraryData"),
     #     style = "color:black;font-size:8pt"),
     # 
     # plotOutput("structurePlotter_plot",height="250px", width = "250px"),
     # 
     uiOutput("LibraryPlotsUI"),
     plotlyOutput("LibraryPlot",height="350px")
   )
)