library(shiny)
require(shinydashboard)
library(ggplot2)
library(scales)
# library(dplyr)
# library(formattable)
#source('Dedupe.R')
mylist <- c('a','b','c')
options(shiny.maxRequestSize = 10*1024^2) #Change MB to suit your needs



#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Guru Deduper") 

sidebar <- dashboardSidebar(
  sidebarMenu(
    #menuItem("Landing", tabName="Landing", icon = icon("home")),
    menuItem("GUI", tabName = mylist[1], icon = icon("align-justify")),
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
    uiOutput("inputs"),
    title = "Game Parameters",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE
  )
)

tabs <- tabItems(
  tabItem(tabName = mylist[1],
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
    inputs <- as.integer(length(df))
    lapply(1:inputs, function(i) {
      checkboxInput(inputId = paste0("input", i), label = paste(names(df)[i]), value = TRUE)
    })
  })
  output$fileUpload <- renderValueBox(
    expr=div(input$file[1]$name)
  )
  
}

shinyApp(ui, server)