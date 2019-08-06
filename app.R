library(shiny)
require(shinydashboard)
library(ggplot2)
library(scales)
# library(dplyr)
# library(formattable)
source('Dedupe.R')
mylist <- c('a','b','c')
options(shiny.maxRequestSize = 10*1024^2) #10MB



#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Guru Deduper") 

sidebar <- dashboardSidebar(
  sidebarMenu(
    #menuItem("Landing", tabName="Landing", icon = icon("home")),
    menuItem("GUI", tabName = "GUI", icon = icon("align-justify")),
    menuItem("Source", href="github.com/cshanehsaz/shinydashboard", icon=icon("github"))
  )
)

#Landing Page Elements
#------------------
welcomePage <- fluidRow(
  box(
    h1('Thanks for using the Guru Deduper.'),
    h3('I made this tool in order to make deduplifying much easier and more customizable 
       since Salesforce currently stinks at it :)'),
    h3('If you have questions or comments, please submit them to:'),
    h3('cyrussh@wharton.upenn.edu'),
    width = 12
  )
)

#Dedupe UI
#-----------------------
#input parameters for individual games
#gameparams <- fluidRow(box(
dedupeUI <- fluidRow(
  box(
    h4("Current Dedupe Params: "),
    checkboxInput("email", h4("email"),
                  value = TRUE),
    fileInput("file", label="CSV of Contacts to Dedupe", buttonLabel="Browse Files", width="600px"),
    valueBoxOutput("fileUpload"),
    title = "Duplicate Finder Parameters",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    width=12
  ),
  box(
    h4("First, select the CSV file you want to dedupe."),
    h4("Then check the boxes below if you want to use the MOST RECENT 
       contact values rather than the oldest."),
    h4("After selecting, hit the submit button to run the dedupe"),
    uiOutput("inputs"),
    actionButton(inputId="button", label="Submit"),
    verbatimTextOutput("buttonCheck"),
    title = "Game Parameters",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    width=12
  )
)

tabs <- tabItems(
  tabItem(tabName = "GUI",
          dedupeUI
  ),
  tabItem(tabName = "Landing",
          welcomePage
  )
)

# combine the two fluid rows to make the body
body <- dashboardBody(tabs)

ui <- dashboardPage(title = 'Dedupe', header, sidebar, body, skin = 'blue')



# create the server functions for the dashboard  
server <- function(input, output) { 
  output$inputs <- renderUI({
    if(is.null(input$file)) {
      return()
    }
    df <- read.csv(input$file$datapath)
    column_names <<- names(df)
    totalColumns <<- as.integer(length(df))
    lapply(1:totalColumns, function(i) {
      checkboxInput(inputId = paste0("input", i), label = paste(names(df)[i]), value = FALSE)
    })
  })

  #when the submit button is hit, returns a list of all column names
  #that are set to true
  observeEvent((input$button), {
    if(!is.null(input$file)) {
      print("it's alive!")
      recents <<- c()
      lapply(1:totalColumns, function(i) {
        recents <<- c(recents, input[[paste0("input", i)]])
      })
      recents <<- which(recents == TRUE)
      print(length(recents))
      if(length(recents)>0) {
        recents <- column_names[recents]
      }
      print(recents) 
      RunDedupe(file = input$file$datapath, oldMaster = recents)
    }
  })
  
  output$fileUpload <- renderValueBox(
    expr=div(input$file$name)
  )
 
}

shinyApp(ui, server)