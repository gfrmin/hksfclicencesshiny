#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(tidyr)
library(dplyr)
library(DT)

hksfclicences <- tbl(src_postgres("hkdata"), "hksfclicences", n = Inf) %>% collect %>% unique
hksfclicences$licence <- "✔" 
hksfclicences <- hksfclicences %>% spread(acttype, licence, fill = "")

licencetypes <- c("Type 1: dealing in securities",
  "Type 2: dealing in futures contracts",
  "Type 4: advising on securities",
  "Type 5: advising on futures contracts",
  "Type 6: advising on corporate finance",
  "Type 7: providing automated trading services",
  "Type 9: asset management",
  "Type 10: providing credit rating services")

ui <- fluidPage(
   
   # Application title
   titlePanel("HK SFC licences"),
   sidebarLayout(
     sidebarPanel(
       checkboxGroupInput("licencetypes", "Licences held:",
                          licencetypes
                          )
       ), 
     mainPanel(
       dataTableOutput("hksfclicences")
    )
   )
)

server <- function(input, output, session) {
   output$hksfclicences <- renderDataTable({
     if (!is.null(input$licencetypes)) {
     hksfclicences <- hksfclicences %>% filter_(
       paste0("`",
              paste(input$licencetypes, collapse = '` ==  "✔" & `'),
              '` ==  "✔"')
       )
     }
     hksfclicences
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

