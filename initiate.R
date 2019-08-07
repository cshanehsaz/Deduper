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

shiny::runApp('~/Downloads/Deduper-master',launch.browser=TRUE)