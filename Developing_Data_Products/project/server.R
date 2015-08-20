library(shiny)
library(UsingR)

data(galton)

modFit <- lm(galton$child ~ galton$parent)
predictHeight <- function(parentHeight, parentGender) {
        summary(modFit)$coef[1] + summary(modFit)$coef[2] *
                parentHeight * (1 + 0.08 * ( parentGender == "Female") )
}
        
shinyServer(
        function(input, output) {
                output$inputValue <- renderPrint({input$parentHeight})
                output$prediction <- renderPrint({predictHeight(input$parentHeight, input$parentGender)})
        }
)