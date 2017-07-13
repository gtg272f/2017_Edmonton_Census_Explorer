#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

datasetNames=c( "Population by Employment Sector",
                "Population by Employment Status",
                "Population by Household Income",
                "Population by Mode of Transportation",
                "Dwelling Unit By Ownership",
                "Population by Highest Educational Attainment",
                "Population by Age Range",
                "Population By Dwelling Unit Status",
                "Dwelling Unit By Language")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(
    column(width=12,
           h1("2017 Edmonton Census Explorer")
    )
  ),
  fluidRow(
    column(width=12,
           h3("Variable Selection")
    )
  ),
  fluidRow(
    column(width=6,
           
           selectInput("DD1", "Primary Dataset", choices=datasetNames),
           uiOutput("Var1")
           #checkboxInput("CB1", "Correlation",value = FALSE)
    ),
    column(width=6,
           conditionalPanel(
             condition = "input.CB1 == true",
             selectInput("DD3", "Secondary Dataset", choices=datasetNames),
             uiOutput("Var2")
           )
    )
  ),
  fluidRow(
    column(width=12,
           h3("Histogram")
    )
  ),
  fluidRow(
    column(width=9,
           plotlyOutput("PLOT1",width = "100%", height = "400px")
           ),
    column(width=3,
           tableOutput("TABLE1")
    )
  ),
  fluidRow(
    column(width=12,
           h3("Pareto")
    )
  ),
  fluidRow(
    column(width=8,
           plotlyOutput("PLOT2",width = "100%", height = "400px")
    ),
    column(width=2, 
           tableOutput("TABLE2")
    )
  )



))
