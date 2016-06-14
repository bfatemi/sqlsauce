#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Add Credentials"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        sidebarPanel(
            textInput("text", label = h3("Server Name"), value = ""),
            textInput("text", label = h3("Database Name"), value = ""),
            checkboxInput("checkbox", label = "Use Windows Credentials", value = TRUE),
            br(),
            uiOutput("usr"),
            uiOutput("pwd"),
            actionButton("action", label = "Submit")
        ),

        # # Show a plot of the generated distribution
        mainPanel(
            # uiOutput("usr"),
            # uiOutput("pwd"),
            # uiOutput("submit")
        )
    )
))
