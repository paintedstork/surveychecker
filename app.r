library(shiny)
library(DT)

source("transectCheck.R")

shinyServer <- function(input, output) {
  
  summaries <- reactive ({
    req(input$button)
    loadData()
  })
  
observeEvent(input$button, {
})  

output$data_summary <- renderDT ({summaries()[[3]]
  },options = list(
    lengthMenu = list(c(20, 50, 100, 300, 500, -1), c('20', '50', '100', '300', '500', 'All')),
    pageLength = 100))
  
  
output$transect_summary <- renderDT ({summaries()[[2]]
  },options = list(
    lengthMenu = list(c(20, 50, 100, 300, 500, -1), c('20', '50', '100', '300', '500', 'All')),
    pageLength = 100))
  
output$camp_summary <- renderDT ({summaries()[[1]]
                  },options = list(
                    lengthMenu = list(c(20, 50, 100, 300, 500, -1), c('20', '50', '100', '300', '500', 'All')),
                    pageLength = 100))

  
}

shinyUI <- fluidPage(
  titlePanel('Survey Checker - Pending Checklists'),
  fluidRow(
    column(12,
           p("Uses comments in eBird checklist to see all lists are properly tagged to transects."),
           p("Last Date of Update. Code: 06 December 2022."))
  ), 
  
  
  sidebarPanel(
    width = 3,  
    actionButton("button", "Load"),
    ),

  mainPanel(
    tabsetPanel(
        tabPanel("Camps", dataTableOutput('camp_summary')),
        tabPanel("Transects", dataTableOutput('transect_summary')),
        tabPanel("Data", dataTableOutput('data_summary')),
        tabPanel("About", 
                 br(), h1("About Survey Checker"), 
                 br(), h1("This is a custom tool built for Attappady survey"), 
        )
      )
  )
)
shinyApp(ui = shinyUI, server = shinyServer)
