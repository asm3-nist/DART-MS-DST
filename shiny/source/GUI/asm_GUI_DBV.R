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
           p(HTML("Sisco et. al.<a href='https://pubs.acs.org/doi/abs/10.1021/jasms.0c00416' target='_blank'>
                   J. Am. Soc. Mass Spectrom. 2021, 32, 3, 685–689.</a>")),
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