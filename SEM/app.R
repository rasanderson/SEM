#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyAce)
library(piecewiseSEM)
library(nlme)

rawd<-read.csv("www/FINAL_SWEDEN.csv", header=TRUE)

init <- "
                      big_sweden_mod<-psem(
                      lme(humans  ~ time   + habitat1,                              random=~1|name, rawd),
                      lme(seed    ~ humans + habitat1,                              random=~1|name, rawd),
                      lme(wolf    ~ humans + seed     + habitat1,                   random=~1|name,rawd),
                      lme(lynx    ~ humans + seed     + habitat1,                   random=~1|name,rawd),
                      lme(red_fox ~ humans + seed     + wolf     + lynx + habitat1, random=~1|name, rawd)
                      )
                      
                      
"

ui <- fluidPage(
    h1("Shiny Ace Evaluate Code"),
    fluidRow(
        column(
            6,
            h2("Source Code"),
            aceEditor("code", mode = "r", height = "200px", value = init),
            actionButton("eval", "Evaluate")
        ),
        column(
            6,
            h2("Output"),
            verbatimTextOutput("outputtext")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    myvals <- reactive({
        input$eval
        x <- isolate({
            mystuff <- paste(input$code,
                             "summary(big_sweden_mod)")
        })
    })
    
    output$outputtext <- renderPrint({
        # input$eval
        # x <- isolate({
        #   input$code
        # })
        #parse(text = x) # Returns everything in the double quotes above
        #eval(parse(text = x))
        #eval(parse(text = isolate(input$code)))
        eval(parse(text = myvals()))
        #return(myres)
    })
    
}



# server <- function(input, output) {
#     
#     big_sweden_mod <- reactive({
#         #sem_results <- psem(input$semeditor)
#         sem_results <- input$editor
#         # sem_results<-psem(
#         #     lme(humans~time+habitat1,random=~1|name,rawd),
#         #     lme(seed~humans+habitat1,random=~1|name, rawd),
#         #     lme(wolf~humans+seed+habitat1,random=~1|name,rawd),
#         #     lme(lynx~humans+seed+habitat1,random=~1|name,rawd),
#         #     lme(red_fox~humans+seed+wolf+lynx+habitat1, random=~1|name, rawd))
#         
#         
#         return(sem_results)
#     })
#     
#     # output$semplot <- renderPlot({
#     #     plot(big_sweden_mod(), show="std",node_attrs = list(
#     #         shape = "circle", color = "black",
#     #         fillcolor = "orange"))
#     # })
#     
#     output$semsummary <- renderPrint({
#         class(big_sweden_mod())
#     })
# 
# }

# Run the application 
shinyApp(ui = ui, server = server)
