# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    # draw the histogram with the specified number of bins
    hist(rnorm(n = 1000,mean = input$Mean), breaks = 100, col = 'darkgray', border = 'white')
  })
}