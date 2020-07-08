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
    h1("Shiny SEM Interactive Code"),
    fluidRow(
        column(
            4,
            h2("SEM components"),
            checkboxGroupInput("sembox", label = h3("Select models"),
                               choices = list(
                                 "humans  ~ time + habitat1" = 1,
                                 "seed    ~ humans + habitat1" = 2,
                                 "wolf    ~ humans + seed + habitat1" = 3,
                                 "lynx    ~ humans + seed + habitat1" = 4,
                                 "red fox ~ humans + seed + wolf + lynx + habitat1" = 5),
                               selected = c(1,2,3,4,5)
                               )
        ),
        column(
            4,
            h2("Output"),
            verbatimTextOutput("outputtext")
        ),
        column(
            4,
            verbatimTextOutput("chosensem")
            # h2("Plot"),
            # renderPlot("semplot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    sem_mod <- reactive({
          big_sweden_mod<-psem(
            lme(humans  ~ time   + habitat1,                              random=~1|name, rawd),
            lme(seed    ~ humans + habitat1,                              random=~1|name, rawd),
            lme(wolf    ~ humans + seed     + habitat1,                   random=~1|name,rawd),
            lme(lynx    ~ humans + seed     + habitat1,                   random=~1|name,rawd),
            lme(red_fox ~ humans + seed     + wolf     + lynx + habitat1, random=~1|name, rawd)
          )
          tmp <- summary(big_sweden_mod, .progressBar = FALSE)
          return(tmp$coefficients)
    })


    output$outputtext <- renderPrint({
      sem_mod()
    })
    
    output$chosensem <- renderPrint({
      as.vector(input$sembox)
    })
    
    # output$semplot <- renderPlot({
    #   plot(sem_mod())
    # })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
