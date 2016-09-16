#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$usr <- renderUI({
        if (input$checkbox == FALSE){
            "text" = textInput("text", label = "Username", value = "")
        }})
    output$pwd <- renderUI({
        if (input$checkbox == FALSE){
            "text" = textInput("text1", label = "Password", value = "")
        }})
    output$submit <- renderUI({
        if (input$checkbox == FALSE){
            "action" = actionButton("action", label = "Submit")
        }})
})
