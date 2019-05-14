library(shiny) #used to do err thang
library(shinydashboard) #used to make it look perdy
library(ggplot2) #used for graphing

ui <- dashboardPage(
  
  dashboardHeader(title = "Menu Options"),
  
  dashboardSidebar( 
    sidebarMenu(
      
      # Will load and save data and display a table of loaded data
      menuItem("Load/Save Data", tabName = "loadSaveData", icon = icon("download")),
      
      # Display raw data and do simple spectral analysis
      menuItem("Data Processing", tabName = "dataProcessing", icon = icon("calculator")),
      
      # Bulk of the more complex spectral analysis
      menuItem("Spectral analysis", tabName = "spectralAnalysis", icon = icon("poll"))
      
    ) #SidebarMenu
  ), #dashboardSidebar
  
  dashboardBody(
    tabItems(
      
      # First tab which is for loading and saving data will also display a table of the data
      tabItem(tabName = "loadSaveData",
              
              fluidRow(
                column(6,
                box(title = "Load Data", status = "primary", solidHeader = TRUE, height = 150, width = "auto",
                    fileInput('file1', 'Choose CSV File',
                              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
                    ) # fileInput 
                  ) # box load
                ), # column 
                
                column(6,
                box(title = "Save Data", status = "primary", solidHeader = TRUE, height = 150, width = "auto",
                    "Save function goes here"
                  ) # box save
                ) # column
                ), # fluidRow for load n save
              
              fluidRow(
                column(12,
                box(title = "Loaded Data", status = "info", solidHeader = TRUE, width = "auto",
                    tableOutput("contents")
                  ) # box table
                ) # column
              ) # fluidRow for table
              
      ), # tabItem loadSaveData
      
      # Second tab which is for Data Processing will display the graphs and simple spectral analysis
      tabItem(tabName = "dataProcessing",
              
              # make two tab boxes on the top for editing graphs and such
              fluidRow(
                tabBox( title = "Plot One Controls", id = "tabBoxControls1", height = 215,
                        tabPanel("Axes", 
                                 selectInput('ycol1', 'Y Variable', ""),
                                 selectInput('xcol1', 'X Variable', "", selected = "")
                                 ), #tabPanel Axes selection
                        tabPanel("Buttons",
                                 actionButton("exclude_reset1", "Reset points"),
                                 actionButton("calcSpec", "Spectral"),
                                 checkboxInput("cleanPointsCheckbox", label = "Remove points", value = FALSE)
                                 ) #tabPanel Buttons
                        ), # tabBox for plot1
                tabBox( title = "Plot Two Controls", id = "tabBoxControls2", height = 215,
                        tabPanel("Axes", 
                                 selectInput('ycol2', 'Y Variable', ""),
                                 selectInput('xcol2', 'X Variable', "", selected = "")
                                 ), #tabPanel Axes selection
                        tabPanel("Buttons",
                                 actionButton("exclude_reset2", "Reset points")
                                 ) #tabPanel Buttons
                        ) # tabBox for plot2
                ), # fluidRow for editing graphs
              
              #fluidPage for plotting the graphs
              fluidPage(
                fluidRow(
                  column(6,
                  plotOutput("plot1", 
                             dblclick = "plot1_dblclick", # to set the zoom or reset zoom
                             click = "plot1_rmvClick", # to remove bad points 
                             brush = brushOpts(id = "plot1_brush", resetOnNew = TRUE) # to zoom in 
                             ) # plotOutput Plot1
                  ), # column 
                  
                  column(6,
                  plotOutput("spec_plot1")
                  ) # column
                ), #fluidRow for Plot1
                
                fluidRow(
                  plotOutput("plot2", 
                             dblclick = "plot2_dblclick", # to set the zoom or reset zoom
                             click = "plot2_click", # to remove bad points 
                             brush = brushOpts(id = "plot2_brush") # to zoom in 
                  ) #plotOutput Plot2
                ) #fluidRow for Plot2
              ) #fluidPage for the graphs
              
      ), #tabItem for dataProcessing
              
      # Third tab which is Spectral analysis
      tabItem(tabName = "spectralAnalysis", 
              h2("Spectral plots go here"))
      ) #tabItem for spectralAnalysis
    
  ) #dashboardBody
) #dashboardPage

server <- function(input, output, session) {
  
  # read in Data 
  myData <- reactive({ 
    req(input$file1) #  require that the input is available
    
    inFile <- input$file1 
    

    df <- read.csv(inFile$datapath)
    
    updateSelectInput(session, inputId = 'ycol1', label = 'Y Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'xcol1', label = 'X Variable',
                      choices = names(df), selected = names(df)[2])
    
    vals$keeprows = rep(TRUE, nrow(df))
    
    

    return(df)
  })
  
  
  output$contents <- renderTable({
    myData()
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  vals <- reactiveValues(
    keeprows =  NULL
  )
  
  data.plots <- reactive({
    
    
    #dataC <-cleanFile()[,c(input$xcol1,input$ycol1)]
  
    
    #print(dataC)
    
    
    # Plot the kept and excluded points as two separate data sets
    keep    <- myData()[vals$keeprows, , drop = FALSE]
    exclude <- myData()[!vals$keeprows, , drop = FALSE]
    
    
    
    firstPlot <- ggplot(keep[,c(input$xcol1,input$ycol1)], aes_string(x = input$xcol1, y = input$ycol1)) + 
      geom_path(color = "red") + geom_point() +
      geom_point(data = exclude[,c(input$xcol1,input$ycol1)], shape = 21, fill = NA, color = "black", alpha = 0.25) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
      theme(panel.grid.major = element_line(colour = "white", size = NULL, linetype = "solid", lineend= NULL, color= NULL),
            panel.grid.minor = element_line(colour = "white", size = NULL, linetype = "dashed", lineend= NULL, color= NULL))
    
    # Making the spectral plot
      #specData1 <- spec.ar(keep[c(ar(input$xcol1),input$ycol1)], log = "no", order = 10)
    
    return(list("data1" = firstPlot))
  })
  
  # Actually plot the first ploted Data
  output$plot1 <- renderPlot({
    data.plots()$data1
  })

  # Actually plot the spectral data
  output$spec_plot1 <- renderPlot({
    data.plots()$dataSpec1
  })
  
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      #ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      #ranges$y <- NULL
    }
  })
  
  # Toggle points that are clicked
  #observeEvent(input$plot1_rmvClick, {
  #  res <- nearPoints(myData(), input$plot1_rmvClick, allRows = TRUE)
  #  
  #  print(input$plot1_rmvClick)
  #  
  #  vals$keeprows <- xor(vals$keeprows, res$selected_)
  #  
  #})
  
  observeEvent(input$plot1_rmvClick,{
    if (input$cleanPointsCheckbox) {
      res <- brushedPoints(myData(), input$plot1_brush, allRows = TRUE)
      vals$keeprows <- xor(vals$keeprows, res$selected_)
      #print(temp)
    }
    
  })
  # Reset all points
  observeEvent(input$exclude_reset1, {
    vals$keeprows <- rep(TRUE, nrow(myData()))
  })
  
} #server

shinyApp(ui = ui, server = server)