#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

updf <- read_csv("updf.csv")
                   
female <- updf %>%
  filter(Q5 == 2)

male <- updf %>%
  filter(Q5 == 1)

hobbyist <- updf %>%
  filter(Q7 == 1)

pursuit <- updf %>%
  filter(Q7 == 2)

professional <- updf %>%
  filter(Q7 == 3)

mixedinc <- updf %>%
  filter(Q7 == 4)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Comedy and Mental Illness"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("age",
                     "Number of bins:",
                     min = 14,
                     max = 90,
                     value = 30),
         
         selectInput("groups", "Comedian Demographics",
                     choices = c("Men", "Women", "Hobbyists", "Professionals", "Amateur Pursuing Professional Career", "Mixed Professional/Unprofessional Income"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Background", textOutput("bg")),
                    
                    tabPanel("Big 5 Personality Traits", textOutput("big5e"),
                             textOutput("big5a"), textOutput("big5c"),
                             textOutput("big5n"), textOutput("big5i"),
                             img(src = 'Big 5.png', align = "right")),
                    
                    tabPanel("Depression", plotOutput("distPlot"),      
                             img(src = '1471-244X-1-3-2.jpg', align = "right"),
                             plotOutput("joyPlot")),
                    
                    tabPanel("Hypomania", plotOutput("hPlot"),
                             textOutput("htable")),
                    
                    tabPanel("Alcoholism", 
                             textOutput("cons"), textOutput("dep"),
                             textOutput("probs"), text("AUDITtot")),
                    
                    tabPanel("Psychosis", textOutput("IntAn"),
                             textOutput("UnEx"), textOutput("CogDis"),
                             textOutput("ImpNon"), textOutput("OLIFEtot"),
                             img(src = 'O-LIFE.png', align = "right"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   datasetInput <- reactive({
     switch(input$groups,
            "Men" = male,
            "Women" = female,
            "Hobbyists" = hobbyist,
            "Professionals" = professional,
            "Amateur Pursuing Professional Career" = pursuit,
            "Mixed Professional/Unprofessional Income" = mixedinc)
   })
   

   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     ggplot(datasetInput(), aes(x = Depression)) + 
       geom_histogram(binwidth = 2, color = "black", fill = "light blue")
   })
  
   
   output$joyPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     datasetInput() %>%
       filter(Q17 < 4) %>%
     ggplot(aes(x = Depression, y = Q16_1)) + 
       geom_point()
   })
   
   output$hPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     datasetInput() %>%
       filter(Q17 < 4) %>%
       ggplot(aes(x = Hypomanic)) + 
       geom_histogram(binwidth = 1, color = "black", fill = "light blue")
   })
   
   output$htable <- renderText({
       paste0(round(mean(datasetInput()$Hypomanic, na.rm = T), digits = 2),
              " (", 
              round(sd(datasetInput()$Hypomanic, na.rm = T), digits = 2),
              ")")
   })
   
   #how to filter out NA values in the string?
   
   output$big5e <- renderText({
     paste0(round(mean(datasetInput()$Extraversion, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Extraversion, na.rm = T), digits = 2),
             ")")
   })
   
   
    output$big5a <- renderText({  
     paste0(round(mean(datasetInput()$Agreeableness, na.rm = T), digits = 2),
            " (",
            round(sd(datasetInput()$Agreeableness, na.rm = T), digits = 2),
            ")")     
   })
    
    output$big5c <- renderText({  
      paste0(round(mean(datasetInput()$Conscientiousness, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Conscientiousness, na.rm = T), digits = 2),
             ")")     
    })
    
    output$big5n <- renderText({  
      paste0(round(mean(datasetInput()$Neuroticism, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Neuroticism, na.rm = T), digits = 2),
             ")")     
    })
    
    output$big5i <- renderText({  
      paste0(round(mean(datasetInput()$Imagination, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Imagination, na.rm = T), digits = 2),
             ")")     
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

