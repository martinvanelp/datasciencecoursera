library(shiny)

# Define UI for dataset viewer application
shinyUI(
        pageWithSidebar(
                # Application title
                headerPanel("Child height prediction"),
                
                sidebarPanel(
                        p('Enter the height and gender of the parent to get a prediction of the height of the child.'),
                        sliderInput('parentHeight', 'Parent height (inches)', 68.5, min = 62, max = 75, step = 0.5),
                        radioButtons('parentGender', 'Gender of parent', c("Male", "Female")),
                        submitButton('Submit')
                ),
                mainPanel(
                        h3('Results of prediction'),
                        h4('You entered the parent\'s height as'),
                        verbatimTextOutput("inputValue"),
                        h4('Which resulted in a prediction of the child\'s height of'),
                        verbatimTextOutput("prediction")
                )
        )
)

