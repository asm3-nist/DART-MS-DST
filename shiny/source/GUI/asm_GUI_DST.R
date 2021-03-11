asm_GUI_DST <- tabPanel(
  "Search Tool",

   DevModeMessage,
   DisclaimerMessage,
   EmailMessage,
   br(),
   fluidRow(
      div(
        radioButtons("searchType", "Search Mode:",
                  c("Pure Compound" = "pc","Mixture Analysis" = "ma"),
                  selected="pc",
                  inline = TRUE
                  ),
        style="color:black;fontsize:14pt",
        align="center"
      )
   ),
   br(),
   br(),
   sidebarPanel(
      shinyjs::useShinyjs(),
      id = "side-panel",
      
      #div(selectInput("RefLibSelect","Reference Library: ",choices=Libraries)),
      
    tabsetPanel(

      tabPanel("Query Spectra",
        br(),
        div(p("Select centroided mass spectrum (txt, jsp, csv) measured
              at each fragmentation level."),
              style="color:black;font-size:10pt"),

                  fileInput("q30", "",
                            multiple = FALSE,
                            placeholder = "30 V Query Spectrum",
                            accept = query_file_types
                            ),
                  div(style="margin-top:-25px"),
                  fileInput("q60", "",
                            multiple = FALSE,
                            placeholder = "60 V Query Spectrum",
                            accept = query_file_types
                            ),
                  div(style="margin-top:-25px"),
                  fileInput("q90", "",
                            multiple = FALSE,
                            placeholder = "90 V Query Spectrum",
                            accept = query_file_types
                            ),

         br(),
         br(),
         actionButton("DartSearch", "Search Database",
                       style='padding:10px; font-size:120%'),
         br(),
         br(),
         br(),
         div(strong(p("References")),
            p("Moorthy & Sisco. (In Prep.). A new library-search algorithm for mixture analysis using DART-MS."),
               style="color:black;font-size:8pt")

      ),

      tabPanel("Advanced",
                br(),
                div(p("These settings can be adjusted to address expected
                      variations in MS sensitivity and resolution."),
                    style="color:black;font-size:10pt"),
                br(),
                sliderInput("target_min_ab",
                            label ="min abundance of targets (mixture analysis):",
                            min = 0.00,
                            max = 1.00,
                            value = 0.25),
                br(),
                radioButtons(inputId="target_type",
                             label="target assumption: ",
                             choices=c("protonated molecule","base peak"),
                             selected = "protonated molecule"),
                br(),
                div(p(strong("m/z tolerance"))),
                shinyjs::useShinyjs(),
                checkboxInput("lowres",
                              label = "Integer resolution spectra.",
                              value=FALSE),

                div(style="margin-top:-25px"),
                sliderInput("epsilon_0",
                            label = "",
                            min = 0.000,
                            max = 0.100,
                            step = 0.001,
                            value = 0.005)
      )
    )
   ),

   mainPanel(
     uiOutput("QueryPlotsUI"),      
     plotlyOutput("QueryPlots",height="350px"),
     br(),br(),
     uiOutput("targets")
   )
)