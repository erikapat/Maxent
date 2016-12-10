#CODIGO UBICADO EN: /home/erika/Dropbox/4ErikaIII (continuacion)/CODIGO R definitivo MAXENT/SME_MEM

require(shiny)
require(nloptr)
require(shinyapps)
require(RJSONIO)
require(Rcpp)
require(DT)


# Define UI for slider demo application
shinyUI(bootstrapPage(
  # Add custom CSS & Javascript;
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$script(type="text/javascript", src = "busy.js")
    )
  ),
  div(class = "busy",  
      p("Calculation in progress..") , 
      img(src= "http://klear.com/images/new/loader.gif")
#"http://imageshack.us/a/img827/4092/ajaxloaderq.gif")
  ),

tags$head(
  tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
),


  #  Application title
  headerPanel("MAXENT"),


  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(

   #helpText("INSTRUCTIONS:", "select the parameters and later press RUN"),
   strong("INSTRUCTIONS:"),
   h6( "Select and Get Values"),
   #submitButton("RUN SME"),
    actionButton("get", "Get Values", class= "btn-success"),
   #options for buttons: "btn-info", 'btn-primary', "btn-warning", "btn-danger", "btn-success", "btn-default"
   #"btn-primary"
    br(),
    br(),

   selectInput("method", "Select Method:", 
                choices = c("SME", "MEM_Poisson", "MEM_Exponential", "SMEE1", "SMEE2")),

   #selectInput("dataset", "Choose a dataset:", 
   #             choices = c("Case_gamma","Case1", "Case2", "Case3", "Case4","Case4")),

    fileInput("dataset", "Choose a dataset:",
              accept=c('text', 'text/plain', '.dat')),

   checkboxInput("outliers", "Advanced Analysis", FALSE),
   
    numericInput("lambda", "Initial Values (lambda):", 1),
   
    numericInput("iterations", "Iterations:", 1000),
    
    numericInput("tol", "Tolerance:", format(1e-3, scientific = TRUE)),

     # Simple integer interval
    sliderInput("step", "Choose a step:", 
                min=1, max=4, value=2),

   
     # Simple integer interval
    #sliderInput("Correction",  "Correction?:", 
    #            min=0, max=1, value=0, step = 1),
   
   sliderInput("GLL", "GLL?:", 
                min=0, max=1, value=1, step = 1),

  
    # Specification of range within an interval
    sliderInput("sigma", "Sigma:",
                min = 0, max = 1, value = c(0.1, 0.5)),
      
    numericInput("phi_max", "Maximo Phi:", value=format(1e20, scientific = TRUE)),
    numericInput("phi_min", "Minimo Phi:", value=format(1e-20, scientific = TRUE))

    
     #submitButton("Update View")

     ),

  
mainPanel(
  
  #div(class = "span4", tableOutput("values")),
  #div(class = "span8", plotOutput("plot"))
 
 tabsetPanel( 
   #RESULTS  -------------------------------------------------------------------------------------------------------
   tabPanel("Results",
                       conditionalPanel(condition = 'input.get == 0', 
                                            div('Click the "GET VALUES" button to Start!', 
                                            dataTableOutput("values_ini"), class = 'alert alert-block') 
                           ),
                       conditionalPanel(condition = 'input.get != 0',
                                        dataTableOutput("values"),
                                 div(class = "span8", plotOutput("plot")) #,  width = "100%", height = "300px")) 
                                ) ), 
   # ADVANCED ------------------------------------------------------------------------------------------------------
   tabPanel("Advanced" , 
            conditionalPanel(condition = 'input.get == 0', 
                             div('Click the "GET VALUES" button to Start!', class = 'alert alert-block')
            ), 
            conditionalPanel(condition =  "input.get != 0 && input.outliers == false", #'input.get != 0',
                             'Note: you should select Advanced Analysis' ),
            conditionalPanel(condition =  "input.get != 0 && input.outliers == true ", #'input.get != 0',
                             'Note: you select Advanced Analysis (Forthcoming...)' #) #, #, #div(class = "span4", plotOutput("plot2"))
            #conditionalPanel(condition =  "input.get != 0 && input.outliers == true", #'input.get != 0',
            #                 '', div(class = "span4", plotOutput("plot2")) 
            ) ),
   #condition = "input.outliers == true"
   
    # INFO   -----------------------------------------------------------------------------------------------------                                  
   tabPanel("Info", div('INFORMATION', class = 'alert alert-success'),  tags$hr(),
                                                                         p('If you want a sample .dat or .txt file to upload,',
                                                                          'you can first download the sample',
                                                                          a(href = 'https://www.dropbox.com/s/yci5m9sioq7r820/sample2.txt?dl=1', 'sample2.txt'), 'or',
                                                                          a(href = 'https://www.dropbox.com/s/p09ldfxsjfs3xcc/sample2.dat?dl=1', 'sample2.dat'),
                                                                          'files, and then try uploading them.',
                                                                          '\n'),
                                                                         p('This is still work in progress...'),
                                                                         p('SMEE1 & SMEE2 does not work for now.')

   #alert alert-danger, 'alert alert-block'
   #.alert-success, .alert-info, .alert-warning r
   
        ))


 #tabsetPanel( 
   #tabPanel("Results", tableOutput("values"),plotOutput("plot")), 
   #tabPanel("Results", plotOutput("plot")) ,
   #tabPanel("Initial values", tableOutput("values_ini"))
 #)

  )

))