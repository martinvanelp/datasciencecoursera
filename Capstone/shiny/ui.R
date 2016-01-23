
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Word prediction by input"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
            h3("Your input"),
            textInput("text",
                      "Type here:",
                      "Start typing words...")
    ),

    # Show a plot of the generated distribution
    mainPanel(
            br(),
            h3("Prediction"),
            
            htmlOutput("prediction")
    )
  )
))
