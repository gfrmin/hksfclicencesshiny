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
library(stringr)

hksfclicences <- tbl(src_postgres("hkdata"), "hksfclicences") %>% collect(n = Inf) %>% unique
hksfclicences$licence <- "✔" 

licencetypes <- sort(unique(hksfclicences$acttype)) # get licence types dynamically

hksfclicences <- hksfclicences %>% spread(acttype, licence, fill = "") %>% filter(str_detect(name, "^[A-Z]")) %>% filter(!str_detect(name, "[!?]"))



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
     datatable(hksfclicences, options = list(order = list(list(3, 'asc'))), colnames = c("Institution", "Role", "Name", "Start date", "End date", licencetypes))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

