library(shiny)
data(mtcars)
cols <- sort(unique(names(mtcars)[names(mtcars) != 'mpg']))
ui <- fluidPage(
  wellPanel(
    fluidRow(
      column(4,
             tags$h3('Build a Linear Model for MPG'),
             selectInput('vars',
                         'Select dependent variables',
                         choices = cols,
                         selected = cols[1:2],
                         multiple = TRUE)),
      column(4, verbatimTextOutput('lmSummary')),
      column(4, plotOutput('diagnosticPlot'))
    )
  )
)
server <- function(input, output) {
  lmModel <- reactive({lm(sprintf('mpg ~ %s', paste(input$vars, collapse = '+')),
                          data = mtcars)})
  output$lmSummary <- renderPrint({
    summary(lmModel())
  })
  
  output$diagnosticPlot <- renderPlot({
    par(mfrow = c(2,2))
    plot(lmModel())
  })
}
shinyApp(ui = ui, server = server)