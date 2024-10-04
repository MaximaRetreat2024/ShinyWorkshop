#
#
#   Boiler template for creating and running an R shiny app
#
#   Maxima Research retreat 2024
#   
###############################################################


rm(list=ls())
# Check for correct packages
list.of.packages <- c("vroom", "shiny","tidyverse","thematic","readxl","scales","lubridate","bslib")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
## install if not present
if(length(new.packages)) 
  install.packages(new.packages)

### User interface on webpage
ui <- fluidPage(
)


### The server code
server <- function(input, output, session) 
{
}

### Run the app
shinyApp(ui, server)