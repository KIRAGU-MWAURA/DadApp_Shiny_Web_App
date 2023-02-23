library(shiny)
library(shinythemes)
library(tidyr)
library(dplyr)
library(data.table)
library(gridExtra)
library(ggplot2)

ui<-fluidPage(theme = shinytheme("united"), 
              # Application title
              titlePanel("DadApp:  A web server for predicting paternity in wild animal population "),
              sidebarLayout(
                sidebarPanel(
                  fileInput("popn_infile4",
                            "Choose Whole population's genotype Data file",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")),
                  fileInput("fathers_infile1",
                            "Choose Potential Father(s) Data file",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")),
                  selectInput("dads_name_data", 
                              label = h3("Availability of Dads' Names data"), 
                              choices = c("no" = 1, 
                                          "yes" = 2), 
                              selected = "user"),
                  conditionalPanel(
                    condition = "input.dads_name_data == 2",
                    fileInput("dads_names_infile6",
                              "Choose Dads' names Data file",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
                  ),
                  
                  conditionalPanel(
                    condition = "input.dads_name_data == 1",
                    numericInput("dads_num",
                                 "Number of putative fathers",
                                 value = 1),
                  ),
                  
                  fileInput("offspring_infile2",
                            "Choose Offspring Data file",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")),
                  
                  selectInput("mothers_data", 
                              label = h3("Availability of Mother's genotype data"), 
                              choices = c("no" = 1, 
                                          "yes" = 2), 
                              selected = "user"),
                  
                  conditionalPanel(
                    condition = "input.mothers_data == 2",
                    fileInput("mother_infile3",
                              "Choose mother(s) Data file if present",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")),
                  ),
                  conditionalPanel(
                    condition = "input.mothers_data == 1",
                    checkboxInput("sure", label = ("Would you like to proceed without maternal genotype calls?"))
                  ), 
                  
                  
                  actionButton("submit", "SUBMIT"),
                ),
                # Show a plot of the generated distribution
                #Show a tabular representation with every individual potential father and their respective LOD scores 
                
                mainPanel(
                  conditionalPanel(
                    condition = "input.mothers_data == 1",
                    h3("Warning: Calculating the logarithm of odds of potential fathers without the maternal genotype calls gives low scores "),
                  ),
                  tabsetPanel(
                    tabPanel('LOD Table', tableOutput('table')),
                    tabPanel('Histogram Plot', plotOutput("plot")),
                    tabPanel('Detailed Table', tableOutput('summary_table')),
                    tabPanel('Detailed Chart', plotOutput('summary_chart'))
                  )
                )
              )
)
