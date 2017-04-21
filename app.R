#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(DT)
library(stringr)
library(readr)

sfclicences <- read_csv("hksfclicences.csv")

ui <- fluidPage(
   titlePanel("HK SFC licences"),
   sidebarLayout(
     sidebarPanel(
       checkboxGroupInput("licencetypes", "Licences held:",
                          1:10
                          ),
       radioButtons("officertypes", "Officers:",
                          c(unique(sfclicences$lcRole), "All")
       ),
       checkboxInput("activeonly", "Active only?", FALSE)
       ),
     mainPanel(
       dataTableOutput("hksfclicences")
    )
   )
)

server <- function(input, output, session) {
  output$hksfclicences <- renderDataTable({
    if (input$activeonly == TRUE) {
      sfclicences <- sfclicences %>% filter(is.na(endDate))
    }
    if (!is.null(input$licencetypes)) {
      sfclicences <- sfclicences %>% filter_(paste0("`", paste(input$licencetypes, collapse = '` ==  "✔" & `'),'` ==  "✔"'))
    }
    if (input$officertypes != "All") {
      sfclicences <- sfclicences %>% filter(lcRole == input$officertypes)
    }
    datatable(sfclicences, rownames = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

