require(shiny)
require(nloptr)
require(shinyapps)
require(RJSONIO)
require(Rcpp)
require(DT)
options(warn=-1)

`%then%` <- shiny:::`%OR%`
source("opt_methods.R")
source("ValidationFunctions(check).R")

lamb_opt <- rep(-999,8)

# Define server logic for slider examples
shinyServer(function(input, output, session) {

    
  # Return the requested dataset
  #datasetInput <- reactive({
  #  switch(input$dataset,
  #         "Case_gamma" = unlist(read.table(file ="caso_gamma.dat", header = FALSE)),
  #         "Case1" = unlist(read.table(file ="example4.dat", header = FALSE)),
  #         "Case2" = unlist(read.table(file ="example2.dat", header = FALSE)),
  #         "Case3" = unlist(read.table(file ="example5.dat", header = FALSE)),
  #         "Case4" = unlist(read.table(file ="example12.dat", header = FALSE)),
  #         "Case5" = unlist(read.table(file ="example13.dat", header = FALSE)))
  #})

 datasetInput <- reactive({

 inFile <- input$dataset
 #print(inFile$size)

    if (is.null(inFile))
      return(NULL)
    
    f <- try(read.table(inFile$datapath, header = FALSE), TRUE)

    if (class(f) != "try-error"){
      unlist(f)
    } 
      
 })


 name_fun <- reactive({  
               switch(input$method,
                   SME             = "funSME.R",
                   MEM_Poisson     = "funMEMpoisson.R",
                   MEM_Exponential = "funMEMexp.R"
 #                  SMEE1 = "fun_mu.R",
 #                  SMEE2 = rexp
                   )
  })

  name_grad <- reactive({  
               switch(input$method,
                   SME             = "gradientSME.R",
                   MEM_Poisson     = "gradientMEMpoisson.R",
                   MEM_Exponential = "gradientMEMexp.R"
                   #SMEE1 = "fun_mu.R",
                   #SMEE2 = rexp
                   )
  })

   #############
   #VALIDATION #
   #############
    #VALIDATION PART

 verification <- reactive({ 

   #If there is not file
   validate(
     need(!is.null(input$dataset), "Error: dataset is not selected or empty. Please, Choose a valid data set")
   )
   #Variable: INITIAL VALUES (lambdas)
    validate(
      need(input$lambda , "Error: Variable 'Initial values' is empty, please enter a valid number")
    )

 #Variable: ITERATIONS
    validate(
      need(input$iterations ,      "Error: Variable 'Iterations' is empty, please enter a valid number") %then%
      need(input$iterations >=0,   "Error: Variable 'Iterations' is negative, please enter a valid number") %then%
      need(input$iterations !=0,   "Error: Variable 'Iterations' is zero, please enter a valid number") %then%
      need(input$iterations >=100, "Error: Variable 'Iterations' is small, please enter a number greater than 100") #%then%
      #need(is.numeric(input$iterations), "Iterations is a character, please enter a valid number")
    )


#Variable: TOLERANCE
    validate(
      need(input$tol ,    "Error: Variable 'Tolerance' is empty, please enter a valid number") %then%
      need(input$tol >=0, "Error: Variable 'Tolerance' is negative, please enter a valid number") %then%
      need(input$tol !=0, "Error: Variable 'Tolerance' is zero, please enter a valid number") %then%
      need(input$tol <1,  "Error: Variable 'Tolerance' is very large, please enter a valid number") #%then%
    )
    
    #Variable: OTHER METHODS
   # validate(
   #   need(input$method == SMEE1 ,    "Appproach 1: SME with errors (Forthcoming)") %then%
   #     need(input$method == SMEE2 ,  "Appproach 2: SME with errors (Forthcoming)") #%then%
   # )    
    
  })


#----------------------------------------------------------------------------------------------------------
  #TABLE WITH INITIAL VALUES

  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    options(digits = 3)
    dataset <- datasetInput()
    
    
    mp     <-  maxent_parameters(dataset, 8) #cambiar K
    mu     <- mp$mu
    alpha  <- mp$alpha
    lambda <- rep(input$lambda, 8)

     
    # Compose data frame
    dt.ini <- data.frame(
      Name = c(
               "Method: ",
              # "Dataset",
               "Initial Values (lambda): ",
               "alpha_i: ",
              # "moments: ",
               "Iterations: " , 
               "Tolerance: " , 
               "Sigma: ",
               "Step: ",
               #"Correction",
               "GLL: ",
               #"sigma",
               #"time",
               "Advance analysis"
                         ),
      Value = as.character(c(
                             input$method,
                             #input$dataset, 
                             paste(lambda, collapse=', '),
                             paste(alpha, collapse=', '),
                             #paste(mu, collapse=', '),
                             input$iterations, 
                             format(input$tol, scientific = TRUE), 
                             paste(input$sigma, collapse=', '),
                             input$step ,
                             #input$Correction,
                             input$GLL,
                             #input$sigma[1],
                             #time.taken,
                             input$outliers
                                            )), 
      stringsAsFactors = FALSE, row.names = 'Name')
    
    DT::datatable(dt.ini, filter = "none", class = 'cell-border stripe', options = list(paging = FALSE, searching = FALSE
                                                          )) #, class = 'compact'
  }) 
  
#-----------------------------------------------------------------------------------------------------------
#TABLE WITH RESULTS 

  # Reactive expression to compose a data frame containing all of the values
  sliderResults <- reactive({

    verification() 
 
    options(digits = 3)
    dataset <- datasetInput()
    mp      <- maxent_parameters(dataset, 8) #cambiar K
    mu      <- mp$mu
    alpha   <- mp$alpha # global
    lambda  <- rep(input$lambda,8) 

    start.time <- Sys.time() #starting to measuring time #***
     
     phi_max1 <- input$phi_max
     phi_min1 <- input$phi_min

     if (input$method == "MEM_Exponential")
     {
     # phi_max1=1e-1
     # phi_min1=1e-1
     }
     #-----------------------------------------------------------------------------------------------------
     # Create a Progress object
     progress <- shiny::Progress$new()
     progress$set(message = "Computing data...", value = 0)
     # Close the progress when this reactive exits (even if there's an error)
     on.exit(progress$close())
     
     # Create a callback function to update progress.
     # Each time this is called:
     # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
     #   distance. If non-NULL, it will set the progress to that value.
     # - It also accepts optional detail text.
     updateProgress <- function(value = NULL, detail = NULL) {
       if (is.null(value)) {
         value <- progress$getValue()
         value <- value + (progress$getMax() - value) / 5
       }
       progress$set(value = value, detail = detail)
     }
     
     #----------------------------------------------------------------------------------------------------
     hh <- BB_modified(lambda, alpha, mu, nameFun = name_fun(), nameGrad = name_grad(),
                     M = input$iterations,
                     tolerance = input$tol, step = input$step, correction = 0, #input$Correction,
                     input$GLL, sigma1 = input$sigma[1], sigma2 = input$sigma[2],
                     phi_max = phi_max1, phi_min = phi_min1, N = 10, updateProgress)

    err <- NULL
    err <- error_msg(hh$error)
    lamb_opt <<- hh$lambda_Opt # global

   end.time   <- Sys.time()
   time.taken <- abs(start.time - end.time)

    # Compose data frame
    dt.fin <- data.frame(
      Name = c(
               "Message:",
               "Optimal lambda:",
               "Iterations:" , 
               "Gradient:" , 
 #              "Distance between solutions:" ,
 #              "Distance between gradients:" ,
               "time:"
                         ),
      Value = as.character(c(
                             paste(err),
                             paste(lamb_opt, collapse = ', '),
                             paste(hh$iter), 
                             paste(hh$grad), 
 #                            hh$dif_F,
                             paste(format(end.time - start.time))
                                            )), 
      stringsAsFactors=FALSE, row.names = 'Name')
    
    DT::datatable(dt.fin, class = 'compact', filter = "none", options = list(paging = FALSE, searching = FALSE,
                                                          columns.orderable = FALSE))
  }) 
#######################################################################################


##############
#SHOW RESULTS
##############

 #Show the values using an HTML table
 output$values_ini <- renderDataTable({
    sliderValues()
  })


# Render text when button is clicked
output$values <- renderDataTable({ # renderUI
 
  if (input$get == 0) #the name of the button is get
    return(NULL)
  if (input$get > 0)
  {
      isolate({
      sliderResults()
    })
  }
    })


output$plot <- renderPlot({


     if (input$get == 0) #the name of the button is get
        return(NULL)
     if (input$get > 0)
     {
        isolate({
        dataset <- datasetInput()
         })
        
       if (is.null(dataset)){
          return(NULL)
        }else if (class(dataset) == "try-error"){
          #If the file is empty
          return(NULL)
        } else {
          sep   <- 15 #set 9 o 10
          d     <- hist(dataset[dataset > 0], breaks = sep, plot = FALSE)
          x     <<- seq(0.01, max(d$mids), length = 100)
          alpha <- maxent_parameters(dataset, 8)$alpha #cambiar K
        
          isolate({
          type_method <- input$method
          })

          if (type_method == "SME")
             densidad <<- SME_density(lamb_opt, alpha, x, globalZ = 1e-3)$densidadS
          if (type_method == "MEM_Poisson")
             densidad <<- densityMEMpoisson(lamb_opt, alpha, x, N = 200, ita = 2, meth = 4)
          if (type_method == "MEM_Exponential")
             densidad <<- densityMEM_exp(lamb_opt, alpha ,x, N = 190, zeta = 50, mu = NULL, meth = 6)$densityMEM
            #densidad <- densityMEM_exp(lamb_opt, alpha ,x, N = 100, zeta = 10, mu = NULL, meth = 1)$densityMEM
    

          hist(dataset[dataset > 0], breaks = sep, freq = FALSE, include.lowest = TRUE, 
          ylim = c(0, max(c(densidad)) + 0.1), xlim = c(min(c(dataset[dataset > 0])), 
                                                      max(c(dataset[dataset > 0], x))),
          xlab = "S", main = paste("")) #, col = 'lightgray', border = 'lightgray')
          #grid(col = "gray", lty = "dotted")
          lines(x, densidad, col = "black", lwd = 3, lty = 2) 
          par(ps = 12, cex = 1.5)
          legend(max(c(dataset[dataset > 0], x[densidad > 0])) - 4 , max(c(densidad)) - 0.02, c(type_method),
          lty = c(2), lwd = c(2), col = c("black")) 

          
          #dataset <- as.data.table(as.vector(dataset[dataset > 0]))
          #setnames(dataset, 'values')
          #dataset <-  dataset[, values := as.numeric(values)]
          #dt.density <- as.data.table(cbind(x, densidad))
          #require(ggplot2)
          #environment<-environment() 
          #p <- ggplot(dataset, aes(x=values)) + geom_histogram(aes(y = ..density..), stat = "bin",
          #                                                     bins = sep)
          #p <- p+scale_fill_manual("",breaks="samples", values="steelblue")
          #p <- p + geom_line(data=dt.density, aes(x=x, y=densidad, colour="theory"), size=1.5)
          #p <- p+scale_color_manual("",breaks="theory", values="red")
         #print(p)
          #input$tol
          #if (input$tol <= 1e-6){
          #  withProgress(message = 'Calculating Advanced Analysis', value = 0, {
          #    F_calc <- F_diff(dataset[dataset > 0], lamb_opt, alpha, globalZ = 0.055) #0.0055
          #    FF     <<- F_calc$F_fit   # maxent F
          #    F_e    <<- F_calc$F_emp   #EMPIRIC DISTRIBUTION FUNCTION 
          #})
          #}
        }
      }
    })

#-----------------------------
output$plot2 <- renderPlot({
  
  
  if (input$get == 0 && input$outliers == FALSE) #the name of the button is get
    return(NULL)
  if (input$get !=  0 && input$outliers == TRUE)
  {
    isolate({
      dataset <- datasetInput()
    })
    
    if (is.null(dataset)){
      return(NULL)
    }else if (class(dataset) == "try-error"){
      #If the file is empty
      return(NULL)
    } else {
      #graphics.off() 
      par(mfrow=c(2,2)) 
      #DISTRIBUTION FUNCTION PLOT
      DistributionPlot(dataset[dataset > 0], F_e, FF)
      #REALIABILITY DIAGRAM
      DrawRealiabilityDiagram(FF, F_e, dataset[dataset > 0])
      #CALIBRATION DIAGRAM
      DrawCalibrationDiagram(FF, F_e, dataset[dataset > 0])
      # DENSITY
      sep   <- 15 #set 9 o 10
      hist(dataset[dataset > 0], breaks = sep, freq = F)
      lines(x, densidad, col = "black", lwd = 3, lty = 2) #, xlab = "S", main = paste("")
    }
  }
    })
#fin
})
