###########################################################
#### Application Launching File for Liaison Dashboard #####
###########################################################
########### See Other files for documentation #############
###########################################################

print("=======================================================================")
print("================== WELCOME TO THE Liaison DASHBOARD ===================")
print("=======================================================================")
print("Downloading Packages...")
packages <- c(
  "cli", "shiny", "shinyWidgets", "shinydashboard", "zoo", "RSQLite",  
  "tidyverse", "writexl", "plotly", "glue", "data.table", "RODBC",  
  "shinycssloaders", "rclipboard", "shinyBS", "shinythemes", "DT", 
  "odbc", "devtools", "cowsay", "markdown", "DBI", "word2vec" 
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    #install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}


#load in dashboard elements from file 
source("Code/Function.R")
source("Code/variables.R") 
print("Creating UI...")
source("Code/ui.R")
print("Creating server...")
source("Code/server.R")

say("
Opening Dashboard...
", by = "cat")


## to run in the browser (do not use for Posit, only locally)
app <- shinyApp(ui=ui, server=server)
runApp(app,launch.browser = TRUE)

