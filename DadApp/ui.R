library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("DadApp: WebApp for Assigning Paternity"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("fathers",
                  "Potential Fathers",
                  min = 1,
                  max = 200,
                  value = 100),
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    title <- "Histrogram of output_lod"
    # generate bins based on input$bins from ui.R
    # draw the histogram with the specified number of bins
    hist(rnorm(1000,mean = input$fathers), breaks = 100, col = 'darkgray', border = 'white')
  main = title
    })
}

shinyApp(ui = ui, server = server)