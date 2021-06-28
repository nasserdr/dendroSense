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

workdirectory <- getwd()



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
        
        # Here, we display the output where the results data will be rendered
        tableOutput("mytable"), 
        
        # The download button UI
        downloadButton("downloadData", "Download dataframe"), 
        
        
        # Here, we display the options for the plot finishing
        fluidRow(
          column(3,
                 textInput("color", label = "Color", value = "#FFD700"),
                 selectInput("type","Type",c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),"solid"),
                 textInput("xlabel", label = "xlabel", value = "xlabel"),
                 textInput("ylabel", label = "ylabel", value = "ylabel"),
                 textInput("title", label = "Title", value = "Title")),
          column(9,
                 plotOutput("plot1"),
                 downloadButton('downloadPlot','Download Plot'))
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
  
 
  mydata <- reactive({
    
    if(input$radio_select == '2'){ #In files are on a a local HD
      if(is.null(input$mypath)){
        return(NULL)
      }
      else{
        data <- read.csv(file.path(input$mypath, '/', input$selectAFile), header = TRUE, sep = ";", stringsAsFactors = FALSE)
        data$date <- as.POSIXct(strptime(data$date, format = "%Y-%m-%d %H:%M:%OS"))
        data
      }
    }
    else{ # In case files are on the server
      if(is.integer(input$directory) == FALSE){
        setwd(parseDirPath(volumes, input$directory))
        data <- read.csv(file.path(parseDirPath(volumes, input$directory), input$selectAFile), header = TRUE, sep = ";", stringsAsFactors = FALSE)
        data$date <- as.POSIXct(strptime(data$date, format = "%Y-%m-%d %H:%M:%OS"))
        data 
      }
      else{
        data <- read.csv(file.path('../data', input$selectAFile), header = TRUE, sep = ";", stringsAsFactors = FALSE)
        data$date <- as.POSIXct(strptime(data$date, format = "%Y-%m-%d %H:%M:%OS"))
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
  
  # Just for debugging
  observe({
    cat("\ninput$directory value:\n\n")
    # print(input$directory)
  })
  
  
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
  dataplot <- reactive({
    data <-  mydata() %>% filter(date>=input$dateStart & date<=input$dateEnd+1)
    data$date <- strftime(data$date,'%Y-%m-%d %H:%M:%OS')
    data
    })
  
  # Rendering the output table
  output$mytable <-  renderTable({
    head(dataplot())
    })
  
  # Rendering the output plot
  output$plot1 <- renderPlot({
    
    req(input$selectAColumn1)
    ggplot(data = dataplot(), aes(x= dataplot()[,1], y = dataplot()[,input$selectAColumn1] ,group=1))+
      labs(x = input$xlabel, y= input$ylabel)+
      labs(title = input$title)+
      geom_line(color=input$color,linetype = input$type)
  })
  }

# Encapsulating the app
shinyApp(ui, server)
