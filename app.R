if(require(shiny) == FALSE) {
  install.packages('shiny', repos = "http://cran.us.r-project.org")
}
if(require(shinydashboard) == FALSE) {
  install.packages('shinydashboard', repos = "http://cran.us.r-project.org")
}
if(require(ggplot2) == FALSE) {
  install.packages('ggplot2', repos = "http://cran.us.r-project.org")
}
if(require(scales) == FALSE) {
  install.packages('scales', repos = "http://cran.us.r-project.org")
} else {cat('packages up to date\n')}
source('Dedupe.R')

options(shiny.maxRequestSize = 200*1024^2) #10MB

#ToDo:
# -add a progress bar to the webapp


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Guru Deduper") 

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Landing", tabName="Landing", icon = icon("home")),
    menuItem("GUI", tabName = "GUI", icon = icon("align-justify")),
    menuItem("Source", href="https://github.com/cshanehsaz/shinydashboard", icon=icon("github"))
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
    h4("1. Select the CSV file you want to dedupe."),
    h4("2. Check the boxes below for the fields where you want to use the MOST RECENT 
       contact values. The values on the oldest contact are used by default."), 
    h5("Note: If you upload a very large file (>10MB) it may take a few moments 
       for the fields to appear below."),
    h4("3. After selecting your desired fields, hit the submit button to run the dedupe"),
    h4("4. After the dedupe is complete, you will find two files in the folder: ", getwd(),
       ". They are named 'RecordsToUpdate.csv' and 'RecordsToDelete.csv'. Run these files through
       workbench to finalize the dedupe in your database."),
    uiOutput("inputs"),
    actionButton(inputId="button", label="Submit"),
    #div(verbatimTextOutput("status")),
    title = "Merge Parameters",
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

  output$status <- renderText({
    width = 100
    paste0('0% Complete')
  })
  
  #when the submit button is hit, returns a list of all column names
  #that are set to true
  observeEvent((input$button), {
    if(!is.null(input$file)) {
      recents <<- c()
      lapply(1:totalColumns, function(i) {
        recents <<- c(recents, input[[paste0("input", i)]])
      })
      recents <<- which(recents == TRUE)
      if(length(recents)>0) {
        recents <- column_names[recents]
      } else {
        recents = ''
      }
      RunDedupe(file = input$file$datapath, oldMaster = recents)
    }
  })
  
  output$fileUpload <- renderValueBox(
    expr=div(input$file$name)
  )
}

shinyApp(ui, server)