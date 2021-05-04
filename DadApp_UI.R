library(shiny)
library(shinytheme)
library(markdown)

shinyUI(
  fluidPage(
    title = "DadApp: A web server that perform paternity analysis utilizing pairwise likelihood-based method ",
    theme = shinytheme("united"),
    navbarPage(
      strong("DadApp"),
      collapsible = TRUE,
      titleContent <-
        HTML(
          "<b>DadApp</b>: A web server that perform paternity analysis utilizing pairwise likelihood-based method"
        ),
      tabPanel(
        "Submit Job",
        titlePanel(titleContent),
        sidebarLayout(
          wellPanel(
            tags$label(
              "Enter your Offspring input data structure(s) in 0, 1, 2 format",
              style = "float: none; width: 100%;"
            ),
            actionLink("addlink", "Insert example data"),
            tags$textarea(
              id = "Data structure",
              rows = 5,
              cols = 100,
              style = "float: none; width:100%;",
              ""
            ),
            #actionLink("addlink", "Insert example data"),
            #tags$label("or",style="float: none; width: 100%;"),
            fileInput('file1', 'or upload file', accept = c('text', '.txt')),
            # tags$label("Step 2 - Submit your job",style="float: none; width: 100%;"),
            actionButton("submitbutton", "Submit", class = "btn btn-primary"),
            
            selectInput(
              "Potential Father(s)",
              "Choose the number of potential fathers",
              c("Single", "Multiple"),
              selected = NULL,
              multiple = FALSE,
              selectize = TRUE,
              width = NULL,
              size = NULL
            ),
            tags$label(
              "Enter your Potential Father(s) input data structure(s) in 0, 1, 2 format",
              style = "float: none; width: 100%;"
            ),
            actionLink("addlink", "Insert example data"),
            tags$textarea(
              id = "Data structure",
              rows = 5,
              cols = 100,
              style = "float: none; width:100%;",
              ""
            ),
            #actionLink("addlink", "Insert example data"),
            #tags$label("or",style="float: none; width: 100%;"),
            fileInput('file1', 'or upload file', accept = c('text', '.txt')),
            # tags$label("Step 2 - Submit your job",style="float: none; width: 100%;"),
            actionButton("submitbutton", "Submit", class = "btn btn-primary"),
            
            selectInput(
              "Mother's Input",
              "Choose presense or absence of mother's input file",
              c("Absent", "Present"),
              selected = NULL,
              multiple = FALSE,
              selectize = TRUE,
              width = NULL,
              size = NULL
            ),
            tags$label(
              "Enter your Mother's input data structure(s) in 0, 1, 2 format",
              style = "float: none; width: 100%;"
            ),
            actionLink("addlink", "Insert example data"),
            tags$textarea(
              id = "Data structure",
              rows = 5,
              cols = 100,
              style = "float: none; width:100%;",
              ""
            ),
            #actionLink("addlink", "Insert example data"),
            #tags$label("or",style="float: none; width: 100%;"),
            fileInput('file1', 'or upload file', accept = c('text', '.txt')),
            # tags$label("Step 2 - Submit your job",style="float: none; width: 100%;"),
            actionButton("submitbutton", "Submit", class = "btn btn-primary"),
            
            sliderInput(
              "integer",
              "Level of Confidence for paternity assignment",
              min = 75,
              max = 100,
              pre = "Relaxed",
              post = "Strict"
            ),
            
            HTML("<a class='btn btn-default' href='/DadApp'>Clear</a>")
          ),
          #wellPanel
          
          mainPanel(
            tags$label("Status/Output", style = "float: none; width: 100%;"),
            verbatimTextOutput('contents'),
            downloadButton('downloadData', 'Download CSV')
          )
        ) #sidebarLayout
      ),
      #tabPanel Submit Job
      
      tabPanel(
        "About",
        titlePanel("About"),
        div(includeMarkdown("about.md"), align = "justify")
      ),
      tabPanel(
        "Download",
        titlePanel("Download"),
        div(includeMarkdown("download.md"), align = "justify")
      ),
      tabPanel(
        "Citing Us",
        titlePanel("Citing Us"),
        div(includeMarkdown("cite.md"), align = "justify")
      ),
      tabPanel(
        "Contact",
        titlePanel("Contact"),
        div(includeMarkdown("contact.md"), align = "justify")
      ),
      
      copyright <-
        div(
          HTML(
            "<br><table border=0 cellpadding=10 cellspacing=10 width='100%' height='50'><tr><td bgcolor='#f2f2f2' align='center'>Copyright © 2016 <a href='http://codes.bio'>codes.bio</a>. All rights reserved.</td></tr></table>"
          )
        ),
      cat(as.character(copyright))
    ) #navbarPage
  ) #fluidPage
) #shinyUI
