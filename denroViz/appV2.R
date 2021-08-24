################################################################################
####################################STARTERS####################################
################################################################################
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(readr)
library(shinyFiles)
library(fs)
library(lubridate)
library(dendRoAnalyst)
library(xts)

workdirectory <- getwd()
plot <- NULL
#Clearing the console


################################################################################
#####################################CLIENT#####################################
################################################################################
header <-  dashboardHeader(title = "DendroViz")
  
side <-  dashboardSidebar(
    sidebarMenu(
      menuItem("Load & Visualize", tabName = "loadData", icon = icon("dashboard")),
      menuItem("Nothing", tabName = "nothing1", icon = icon("table")),
      menuItem("Nothing", tabName = "nothing2", icon = icon("table"))
    )
  )
  
body <-  dashboardBody(
    tabItems(
      # First tab item
      tabItem(
        tabName = "loadData",
        #' Radio button to choose between a folder that is located on Rstudio server
        #' or on a local machine.
        radioButtons("radio_select"," ", c("Server" = "1", "Local" = "2"), inline=T),
       
        #' Here, we show the text field to put the input path + the button that allows to
        #' browse the folder
        fluidRow(
          column(9,style = "margin-top: 25px;",textInput("mypath", label = " ", value = '../data',width = '100%')),
          column(3, style = "margin-top: 45px;",shinyDirButton("directory", "Select Folder", "Please select a folder"))),
        
        hr(),
      
        fluidRow(
          #' Here, we select the file from the list of available files in the directory.
          #' We limit ourselves to the csv files
          #' The dropdown menu for the column selection, date start and date end   
          column(3,
                 selectInput("selectAFile", 
                             "Select File",
                             choices = list.files('../data', pattern="*.csv$"),
                             selected = list.files('../data', pattern="*.csv$")[[1]], multiple = FALSE, width = '10000px')
                 ),
                  
          
          # Space allocation for the file select dropdown
          column(3,
                 uiOutput("selectAColumn")
                 ), 
          # Space allocation for the start date picker
          column(3,
                 uiOutput("dateStart")
                 ), 
          # Space allocation for the end date picker
          column(3,
                 uiOutput("dateEnd"))
          ), 
        
        hr(),
        
        # # Outputing data head and plot
        # fluidRow(
        #   column(8,
        #           tableOutput("phase_table")),
        # column(8,
        #          plotOutput("dataplot"))
        #   
        # ),
        
        # Here, we display the options for the plot finishing
        fluidRow(
          column(3,
                 textInput("xlabel", label = "xlabel", value = "Date/Time"),
                 textInput("ylabel", label = "ylabel", value = "Amplitude"),
                 textInput("title", label = "Title", value = "Phases"),
                 textInput("legend", label = "Legend", value = "Phase"),
                 # The download button UI
                 actionButton("downloadData", "Save Data"),
                 
                 # downloadButton("downloadData", "Download dataframe"),
                 actionButton("downloadPlot", "Save Plot")),
                 
          column(9,
                 plotOutput("cycleplot")
                 )
          ),
        
        hr(),
        # Here, we display the output where the results data will be rendered
        fluidRow(
          column(3),
          tableOutput("phase_table")
        ),
        
        fluidRow(
          column(1),
          column(10,
                 
          plotOutput("tree_water_deficit")),
          column(1)
          # column(10,
          #        
          # column(1)
          
        )
        

        ), #End of Visualization tab
      
      # Nothing for now. To be filled later
      tabItem(tabName = "nothing1"
      ),
      
      
      # Nothing for now. To be filled later
      tabItem(tabName = "nothing2"
      )
      
    )
  ) #End of Dashboard Body


ui <- dashboardPage(header, side, body)

################################################################################
#####################################SERVER#####################################
################################################################################

server <- function(input, output, session) {
  
  cat("\014")  
  
  #Loading the right dataset
  mydata <- reactive({
    
    if(input$radio_select == '2'){ #In files are on a a local HD
      if(is.null(input$mypath)){
        return(NULL)
      }
      else{
        data <- read.csv(file.path(input$mypath, '/', input$selectAFile), header = TRUE, sep = ";", stringsAsFactors = FALSE)
        data$date <- as.POSIXct(strptime(data$date, format = "%Y-%m-%d %H:%M:%S"))
        data
      }
    }
    else{ # In case files are on the server
      if(is.integer(input$directory) == FALSE){
        setwd(parseDirPath(volumes, input$directory))
        data <- read.csv(file.path(parseDirPath(volumes, input$directory), input$selectAFile), header = TRUE, sep = ";", stringsAsFactors = FALSE)
        data$date <- as.POSIXct(strptime(data$date, format = "%Y-%m-%d %H:%M:%S"))
      }
      else{
        data <- read.csv(file.path('../data', input$selectAFile), header = TRUE, sep = ";", stringsAsFactors = FALSE)
        data$date <- as.POSIXct(strptime(data$date, format = "%Y-%m-%d %H:%M:%S"))
        data 
      }
    }
  })
  
  # Getting the volumes
  volumes <- c(Home = fs::path_home(), 
               "R Installation" = R.home(), 
               getVolumes()()
               )
  
  # Getting the shinyDirectory
  shinyDirChoose(input, 
                 "directory", 
                 roots = volumes, 
                 session = session, 
                 restrictions = system.file(package = "base")
                 )
  
  # # Just for debugging
  # observe({
  #   cat("\ninput$directory value:\n\n")
  #   # print(input$directory)
  # })
  # 
  
  # Populate the textinput with the right path
  observe({
    if(input$radio_select == '1'){
      if(is.integer(input$directory) == FALSE){
        updateTextInput(session,
                        'mypath',
                        label = ' ',
                        value = parseDirPath(volumes, input$directory))
      }
      else{
        return(file.path(workdirectory, '../data'))
      }
    }
    else{
      updateTextInput(session, 
                      "mypath", 
                      label = " ",
                      value = workdirectory)
    }
  })
  
 
  #Update with the right input
  observeEvent(
    input$mypath,
    
    {
      if(input$radio_select=="1"){
        if(is.integer(input$directory)){
          updateSelectInput(
            session, "selectAFile", "Select a file",
            choices  =list.files(file.path(workdirectory,'../data'),pattern="*.csv$"),
            selected = list.files(file.path(workdirectory,'../data'),pattern="*.csv$")[[1]])
        }
        else{
          updateSelectInput(
            session, 
            "selectAFile", 
            "Select a file",
            choices  =list.files(input$mypath,pattern="*.csv$"),
            selected = list.files(input$mypath, pattern="*.csv$")[[1]]
          )
        }
        
      }
      else if(input$radio_select=="2"){
        updateSelectInput(
          session, "selectAFile", "Select a file",
          choices  =list.files(workdirectory,pattern="*.csv$"),
          selected = list.files(workdirectory,pattern="*.csv$")[[1]])}
    }
  )

  
  # Create a set of disabled dates
  dates_disabled <-  seq.Date(from = as.Date("2000-01-01"), to = as.Date("2050-01-01"), by = 1) 
  
  dates_disabled <- format(dates_disabled,'%Y-%m-%d')
  
  # Rendering the start dates
  output$dateStart <- renderUI({
    req(input$selectAFile)
    dateInput(input = "dateStart",
              label = "Start date",
              value = format(mydata()[1,1],'%Y-%m-%d'),
              datesdisabled= dates_disabled[!(dates_disabled  %in% format(mydata()[,1],'%Y-%m-%d'))])
  })
  
  
  # Rendering the end dates
  output$dateEnd <- renderUI({
    dateInput(input = "dateEnd",
              label = "End date",
              value = format(mydata()[1,1],'%Y-%m-%d'),
              datesdisabled= dates_disabled[!(dates_disabled  %in% format(mydata()[,1],'%Y-%m-%d'))])
  })
  
  # Rendering the columns names
  output$selectAColumn<- renderUI({
    selectInput(inputId="selectAColumn1", 
                (("Select a column")),
                 choices  =names(mydata()),
                 selected = names(mydata())[2],multiple = FALSE,width = '10000px')
   })
  
  # Rendering data to plot
  cycle_data <- reactive({
    req(input$dateStart)
    req(input$dateEnd)
    data <-  mydata() %>% filter(yday(date)>= yday(input$dateStart)) %>% 
      filter(yday(date) <= yday(input$dateEnd))
    print(str(data))
    
    # data$date <- strftime(data$date,'%Y-%m-%d %H:%M:%S')
    
    print(str(data))
    dendro_num = which(input$selectAColumn1 == names(data))
    daily_stats_dendro <- daily.data(data, TreeNum = dendro_num)
    # print(head(data))
    sc_stat <- phase.sc(data,
             TreeNum = 1,
             smoothing = 1,
             outputplot = FALSE,
             days = c(ydad(input$dateStart), yday(input$dateEnd))
             )
    
    #Removing the Median
    daily_stats_dendro <- daily_stats_dendro %>% select(-median)
    
    #' Adding Daily Net Growth = Daily Max Day - Daily Max Day(-1)

    # Grouping the data per day
    a <- data %>% 
      group_by(yday(data$date)) %>% 
      summarise_at(vars(input$selectAColumn1), max)

    #Computing the daily net growth
    daily_stats_dendro$daily_net_growth <- NA
    
    if(length(daily_stats_dendro$daily_net_growth) > 1){
      for(i in 2:length(daily_stats_dendro$daily_net_growth)){
        daily_stats_dendro$daily_net_growth[i] <- a[i, 2] - a[i-1,2]
      }
    }
    
    #' Adding daily shrinkage = Daily Max AM - Daily min PM
    # data$date <- strptime(data$date, format = "%Y-%m-%d %H:%M:%OS")

    df <- data %>% mutate(hour = as.numeric(hour(date)))
    dailyAm <- df %>% filter(hour >=0 &hour <= 11)
    dailyPM <- df %>% filter(hour >=12 &hour <= 23)
    
    daily_max_am <- dailyAm %>% 
      group_by(yday(dailyAm$date)) %>% 
      summarise_at(vars(input$selectAColumn1), max)
    
    daily_min_pm <- dailyPM %>% 
      group_by(yday(dailyPM$date)) %>% 
      summarise_at(vars(input$selectAColumn1), min)
    
    names(daily_max_am) <- c('doy', 'max')
    names(daily_min_pm) <- c('doy', 'min')
    
    s <- merge(daily_max_am, daily_min_pm, by = 'doy', all = T)
    s <- s %>% mutate(ds = max - min)
    daily_stats_dendro$daily_shrinkage <- s$ds
    
    data$phase_sc <- sc_stat$SC_phase$Phases
    
    cycle_data <- NULL
    cycle_data$daily_stats_dendro <- daily_stats_dendro
    cycle_data$original_series <- data
    
    cycle_data
    
    })
  
  # Rendering data to plot
  series_plot <- reactive({
    data <-  mydata() %>% filter(yday(date)>= yday(input$dateStart)) %>%
      filter(yday(date) <= yday(input$dateEnd))
    data$date <- strftime(data$date,'%Y-%m-%d %H:%M:%S')
    data
    })
  
  
  # Rendering the output table
  output$phase_table <-  renderTable({
    df <- cycle_data()
    df <- df$daily_stats_dendro
    head(df)
    })
  
  # Rendering the output plot
  output$cycleplot <- renderPlot({
    
    req(input$selectAColumn1)
    data <- cycle_data()
    # print(data)
    df <- data$original_series
    df <- na.omit(df, cols = daily_net_growth)
    
    # print(head(df))
    # print(str(df))
    df$date <- as.POSIXct(df$date,'%Y-%m-%d %H:%M:%S')
    print(str(df))
    # df <- df[-1,]
    # print(length(df$date))
    # print(length(df$T1A1))
    # plot(df$date, df$T1A1)
    plot <- ggplot(df, aes(x = date, y = df[,input$selectAColumn1])) + 
      geom_point(aes(colour = factor(phase_sc))) + 
      scale_colour_manual(values = c("orangered2", "sandybrown", "moccasin")) + 
      theme_bw() + 
      labs(x = input$xlabel,
           y = input$ylabel,
           color = input$legend)
    
    plot

  })
  
  output$tree_water_deficit <- renderPlot({
    req(input$selectAColumn1)
    req(input$dateStart)
    req(input$dateEnd)
    data <-  mydata() %>% filter(yday(date)>= yday(input$dateStart)) %>%
      filter(yday(date) <= yday(input$dateEnd))
    dendro_num = which(input$selectAColumn1 == names(data))

    zg_stats <- phase.zg(data,
                         TreeNum = dendro_num,
                         outputplot = TRUE,
                         days = c(yday(input$dateStart), yday(input$dateEnd)))

  })
  
  
  observeEvent(input$downloadData, {
    file <- paste(input$mypath,"/../results/Res_",substr(input$selectAFile,1,nchar(input$selectAFile)-4),"_",input$selectAColumn,input$dateStart,"_",input$dateEnd, ".csv", sep = "")
      df <- cycle_data()
      df <- df$daily_stats_dendro
      write.csv(df, file, row.names = FALSE)
      print(paste('Results saved in ', file))
    
    })

  observeEvent(input$downloadPlot, {
    file <- paste(input$mypath,"/../results/Res_",substr(input$selectAFile,1,nchar(input$selectAFile)-4),"_",input$selectAColumn,input$dateStart,"_",input$dateEnd, ".png", sep = "")
    ggsave(file, plot = plot, device = 'png')    
  })
  
  
}

# Encapsulating the app
shinyApp(ui, server)
