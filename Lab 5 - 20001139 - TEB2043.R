library(shiny)

ui <- fluidPage(
  titlePanel("Lab 5"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                   selected = '"'),
      tags$hr(),
      radioButtons("disp", "Display",
                   choices = c(Head = "head", All = "all"),
                   selected = "head"),
      selectInput("dataset", "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      downloadButton("downloadData", "Download"),
      h2(textOutput("currentTime"))
    ),
    mainPanel(
      tableOutput("contents"),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars,
           "iris" = iris)
  })
  
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = " ")
    },
    content = function() {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  output$contents <- renderTable({
    req(input$file1)
    csvFile = read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
    if(input$disp == "head") {
      return(head(df))
    } else {
      retun(df)
    }
    head(datasetInput(), n = input$obs)
  })
  
  output$currentTime <- renderText({
    invalidateLater(1000)
    paste("The current time is: ", Sys.time())
  })
}

shinyApp(ui = ui, server = server)