library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("DadApp: WebApp for Assigning Paternity "),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("fathers",
                  "Potential Fathers",
                  min = 1,
                  max = 200,
                  value = 100),
      sliderInput ("loci",
                   "Number of loci",
                   min = 1,
                   max = 30,
                   value = 10),
      numericInput ("allele",
                    "Allele Frequency",
                    min =0.1,
                    max = 1.0,
                    value = 0.5),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("lodScoreplot")
    )
  )
)

