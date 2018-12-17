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


#Below is the code to select for which comedian demographic you seek to analyze.

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
   
   # Sidebar to select for group of comedians.
   sidebarLayout(
      sidebarPanel(
         selectInput("groups", "Comedian Demographics",
                     choices = c("Men", "Women", "Hobbyists", 
                                 "Professionals", 
                                 "Amateurs Pursuing Professional Career", 
                                 "Mixed Professional/Unprofessional Income")),
         h5("The above menu allows you to select between male comics, female comics, 
           and comedians at varying professional levels."),
         p("'Hobbyists' refers to comedians who do not intend to become professionals."),
         p("'Professionals' refers to comedians who receive the entirety of their income from performing comedy."),
         p("'Amateurs Pursuing Professional Career' refers to comedians who do not currently make money from comedy but are attempting to do so."),
         p("'Mixed Professional/Unprofessional Income' refers to comedians who make some of their income from performing comedy but make the rest of their income from other sources.")
       ),
      
      
# Background and Research analysis tab.

      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Background and Major Findings", 
                             h4("Background"),
                             p("From towering figures like Robin Williams, Steve Martin, and Richard Pryor, people often assume that there is a positive relationship between comedy and mental illness.
                               Go to any comedy open-mic or Netflix special, and approximately one-quarter of comedians will tell jokes about suicide attempts, substance abuse, depression, alcoholism, manic episodes, and unresolved trauma. While the connection seems like a no-brainer to many individuals in the comedy industry, whether there is a genuine connection between comedy and mental illness, or is it manufactured as a part of performativity is somewhat controversial."), 
                             br(),
                             h5("Comedians' Stories:"),
                             a("https://www.youtube.com/watch?v=F-PyxnnzUhs&t=8s"),
                             br(), br(), 
                             p("In some respects, comedians get paid to talk about their trauma; and in the words of one participant, 'there are just as many depressed electricians as there are comedians; it's just that no one cares about electricians.'
                               Regardless, is the 'sad-clown' phenomenon just a stereotype, projected onto the industry by media headlines about the next celebrity death by suicide or rehab enrollment, or could it be a real correlation? And if there is a relationship, is it that the stress of the comedy industry is 'making' people depressed or that mentally ill people are more likely to go into it?"),
                             p("Up until this point, few studies have looked at cognitive aspects of comedians. None have looked at rates of depression, alcoholism, or bipolar disorder. And more importantly, none of these studies have explored why the correlation between comedy and mental illness could exist. This study analyses the rates of mental illness among comedians and explores potential mechanisms for why mental illness could be more prevalent among this population."),
                             br(),
                             h4("Self-Rated Enjoyability of Joke-Telling Versus Depression Levels", align = "center"), 
                             plotOutput("joyPlot"),
                             br(),
                             h4("Methods and Procedure"),
                             p("This study administered several mental health questionnaires to comedians, including the: Mini-IPIP (a Big Five Personality Measure), the Hypomanic Personality Scale - Short Form, the CES-D (validated measure of depression), the O-LIFE (to replicate the Ando study), and the AUDIT (a valid alcoholism measure)."),
                             p("Our sample included 289 comedians, with a roughly even number of males and females. The sample is also broken down into different professional levels. 52 comedians were professionals, 71 were pure hobbyists, 133 were amateurs pursuing professional careers, and 33 receiving a mix of income for comedy performance and other work."),
                             p("Participants were recruited via college comedy groups, professional comedy club comedian databases, and online comedy Facebook groups."),
                             br(),
                             h4("Conclusions and Analysis"),
                             p("It is a known scientific fact that mental illness is more prevalent among creative professions. This study sought to replicate this finding among the comedy industry. From this data, we found that comedians experienced symptoms of mental illness at much higher rates than the control population. Comedians highest levels of abnormality in depression, alcoholism, and hypomanic symptoms."),
                             p("As far as why this relationship could exist, art in many ways serves to regulate the symptoms of mental illness. While it could be the case that the comedy industry exposes comedians to high levels of stress, given that amateurs experienced mental illness at a rate very similar to that of professionals, it is unlikely that comedy 'makes' people depressed. More importantly, it appears that there is a selection bias towards mentally ill individuals in the comedy industry, given that comedy functions as a coping mechanism for mental illness. According to the self-rated 'Joy' plot, it appears that even among individuals who scored above 40 on the CES-D inventory, the rush of comedy performance is able to directly counter the effects of depressive anhedonia. Furthermore, 62% said that comedy served as a coping mechanism for life events. Therefore, it is clear that a large proportion of comedians use comedy - aware of it or not - to directly counter the symptoms of mental illness.")),
                    
# For greater ease in formatting, decided to display summary statistics for each
# sub-category as a text output as opposed to in a table. For the control data,
# just used table images from the control validation study summary statistics
# data.

                    tabPanel("Big 5 Personality Traits", 
                             
                             h5("Personality Traits"),
                             p("The 'Big 5' is a measure which reduces personality to five main metrics."),
                             p("The five metrics are: Imagination 
                             (inventive/curious vs. consistent/cautious). 
                             Conscientiousness (efficient/organized vs. easy-going/careless). 
                             Extraversion (outgoing/energetic vs. solitary/reserved). 
                             Agreeableness (friendly/compassionate vs. challenging/detached)."),
                             br(),
                             h5("Subcategories as 'mean(sd)':"),
                             textOutput("big5e"),
                             textOutput("big5a"), textOutput("big5c"),
                             textOutput("big5n"), textOutput("big5i"),
                             br(),
                             h5("Control Data for Comparison:"),
                             
  #Changed image dimensions.                             
                             img(src = 'Big 5.png', width = 380, height = 90, align = "center")),
                    
                    tabPanel("Depression", 
                             h4("Depression"),
                             p("For the purposes of this study, 'depression' is defined as chronically low mood and an inability to feel joy.
                               Depressive symptoms were measured using the Center for Epidemiological Studies Depression Scale, 
                               which inquires about depressive symptoms over the last week."),
                             h5("CES-D Histogram for Comedians", align = "center"),
                             plotOutput("distPlot"), textOutput("dnorm"), br(),     
                             img(src = '1471-244X-1-3-2.jpg', width = 600, height = 400, align = "center")),
                    
                    tabPanel("Hypomania", 
                             h4("Hypomania"),
                             p("In psychology, 'hypomania' is defined as a minor form of mania, marked by 
                                elation and hyperactivity. The hypomanic personality scale (HPS) 
                                serves as a screening tool for bipolar II disorder."), br(),
                             h5("HPS Histogram for Comedians", align = "center"), plotOutput("hPlot"),
                             textOutput("htable"),
                             h5("Analysis"),
                             p("Comedians appeared to experience hypomanic symptoms relatively more 
                               frequently than control populations. Hypomania leads to a 'flight of
                               ideas' extremely beneficial for the creative process, and hypomania is highly associated with creativity. 
                               From this data, it could be the case that hypomania improves one's capacity for comedy
                               performance.")),
                    

                    tabPanel("Alcoholism", 
                             h4("Alcoholism"),
                             p("For the purposes of this study, 'alcoholism' is defined as an addiction to the consumption of alcoholic liquor or the mental illness and compulsive behavior resulting from alcohol dependency. 
                                Consumption refers to how many drinks someone consumes in a given week.
                               Dependance refers to whether someone is inable to go without consuming
                               alocohol in a given week or else they would experience withdrawal.
                               Problems refers to alcohol-related issues such as violence or an inability to execute normal responsibilities."),
                             p(""),
                             br(),
                             h5("Subcategories as 'mean(sd)':"),
                             textOutput("cons"), textOutput("dep"),
                             textOutput("probs"), textOutput("AUDITtot"), br(),
                             h5("Control Data for Men:"), 
                             p("Consumption Score: 3.58 (2.36)"), 
                               p("Problems Score: 1.37 (2.85)"),
                               p("Total Score: 4.82 (4.78)"), br(),
                             h5("Control Data for Women:"),
                             p("Consumption Score: 2.42 (1.72)"),
                               p("Problems Score: 0.61 (1.90)"),
                               p("Total score for women: 3.18 (2.97)"),
                             br(),
                             h6("Histogram of AUDIT Total Scores for Comedians"),
                             plotOutput("atot"), 
                             p("In control populations, a cut-off score indicating high risk for alcoholism among men was an AUDIT total score
                               of 8. Among women, the cut-off score was 6. In control populations, 20% of 
                               men and women scored at or above their respective cut-offs. However, among comedians, 40%  
                               scored above theirs."),
                             h6("Histogram of Alcohol Dependance Values for Comedians", align = "center"), 
                             plotOutput("depd"), textOutput("alcad"),
                             br(), 
                             h6("Histogram of Alcohol-Related Problems for Comedians", align = "center"),
                             plotOutput("problemo"), textOutput("alcap")),
                    
# Decided to have definitions for more obscure psychology terms at top, figures below.

                    tabPanel("Schizotypy", 
                             br(),
                             h4("Schizotypy"),
                             p("In psychology, 'schizotypy' is a measure for 
                                psychosis-spectrum personality traits and serves as a screening tool for 
                                schizophrenia."), 
                             h5("Definitions of sub-categories:"),
                             p("'Unusual experiences' is defined as the disposition to have unusual perceptual and other cognitive experiences, such as hallucinations, magical, or superstitious belief and interpretation of events."),
                             p("'Cognitive disorganization' is defined as the tendency for thoughts to become derailed, disorganised or tangential."),
                             p("'Introverted anhedonia' is defined as a tendency towards introverted, emotionally flat and asocial behaviour, and is associated with a deficiency in the ability to feel pleasure from social and physical stimulation."),
                             p("'Impulsive nonconformity' is defined as a disposition towards unstable mood and behaviour particularly with regard to rules and social conventions."),
                             br(),
                             
                             h5("Subcategories as 'mean(sd)':"),
                             textOutput("UnEx"),
                             textOutput("CogDis"), textOutput("IntAn"), 
                             textOutput("ImpNon"), textOutput("OLIFEtot"),
                             br(),
                             
                             h5("Control Data for Comparison:"),
                             img(src = 'O-LIFE.png', width = 320, height = 115, align = "center"),
                             br(), br(),
                             
                             h5("Analysis"),
                             p("The only axis in which comedians appeared to significantly deviate from population
                               norms was in Impulsive Nonconformity. This is unsurprising, given that the act of performing comedy in front of a room-full of strangers is likely to select for people who are impulsive.
                               It is unlikely that comedians experience schizophrenic symptoms at a higher
                               rate than the average population."))
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
# Decided to go with 'switch' input as opposed to reactive buttons. 
   datasetInput <- reactive({
     switch(input$groups,
            "Men" = male,
            "Women" = female,
            "Hobbyists" = hobbyist,
            "Professionals" = professional,
            "Amateurs Pursuing Professional Career" = pursuit,
            "Mixed Professional/Unprofessional Income" = mixedinc)
   })
   

# This code chunk was basic model for all histograms.
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     ggplot(datasetInput(), aes(x = Depression)) + 
       geom_histogram(binwidth = 2, color = "black", fill = "light blue")
   })
  
   
   output$joyPlot <- renderPlot({

       datasetInput() %>%

# Filtered out comedians who said they did not enjoy performing, only wrote comedy.
       filter(Q17 < 4) %>%
       
     ggplot(aes(x = Depression, y = Q16_1)) + 
       geom_point()
   })
   
# Hypomanic scale.
   output$hPlot <- renderPlot({
     datasetInput() %>%
       filter(Q17 < 4) %>%
       ggplot(aes(x = Hypomanic)) + 
       geom_histogram(binwidth = 1, color = "black", fill = "light blue") 
   })
   
# Two alcohol histograms.
   
   output$atot <- renderPlot({
     # generate bins based on input$bins from ui.R
     datasetInput() %>%
       ggplot(aes(x = AUDITtot)) + 
       geom_histogram(binwidth = 1, color = "black", fill = "light blue") 
   })
   
   output$depd <- renderPlot({
     # generate bins based on input$bins from ui.R
     datasetInput() %>%
       ggplot(aes(x = Dependance)) + 
       geom_histogram(binwidth = 1, color = "black", fill = "light blue") 
   })
   
   output$problemo <- renderPlot({
     # generate bins based on input$bins from ui.R
     datasetInput() %>%
       ggplot(aes(x = Problems)) + 
       geom_histogram(binwidth = 1, color = "black", fill = "light blue") 
   })
   
# Analysis for dependance and problems.
   output$alcad <- renderText({
     paste0("A value of 4 or greater in this histogram indicates some level of alcohol dependance. 
            In this sample, there were ", round(count(subset(datasetInput(), Dependance >= 4)), digits = 2),
            " comedians greater than or equal to 4, and ", 
            round(count(subset(datasetInput(), Dependance < 4)), digits = 2),
            " comedians below this threshold.")
   })
   
   output$alcap <- renderText({
     paste0("A value greater than 0 indicates some level of problems due to alcohol consumption, such as violence, crime, missing deadlines, etc. 
            In this sample, there were ", round(count(subset(datasetInput(), Problems > 0)), digits = 2),
            " comedians over 0, and ", 
            round(count(subset(datasetInput(), Problems == 0)), digits = 2),
            " comedians below this threshold. It is extremely striking that among all demographics, a majority of comedians
            experience some alcohol-related problems. This proportion increases among professionals,
            suggesting alcohol-related problems increase with someone's professional involvement
            in the industry.")
   })
   
   output$htable <- renderText({
       paste0("The mean hypomanic inventory score for this group was ", round(mean(datasetInput()$Hypomanic, na.rm = T), digits = 2),
              " (sd = ", 
              round(sd(datasetInput()$Hypomanic, na.rm = T), digits = 2),
              "), compared to the control population mean of 8.2 (sd = 4.3).")
   })
   
   output$dnorm <- renderText({
     paste0("The mean depression inventory score for this group was ", round(mean(datasetInput()$Depression, na.rm = T), digits = 2),
            " (sd = ", 
            round(sd(datasetInput()$Depression, na.rm = T), digits = 2),
            "), compared to the control population mean of 8.39 (sd = 14.95).")
   })
   
  #Below 5 chunks score "Big 5" personality dimensions.
   output$big5e <- renderText({
     paste0("Extraversion: ", round(mean(datasetInput()$Extraversion, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Extraversion, na.rm = T), digits = 2),
             ")")
   })
   
   
    output$big5a <- renderText({  
     paste0("Agreeableness: ", round(mean(datasetInput()$Agreeableness, na.rm = T), digits = 2),
            " (",
            round(sd(datasetInput()$Agreeableness, na.rm = T), digits = 2),
            ")")     
   })
    
    output$big5c <- renderText({  
      paste0("Conscientiousness: ", round(mean(datasetInput()$Conscientiousness, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Conscientiousness, na.rm = T), digits = 2),
             ")")     
    })
    
    output$big5n <- renderText({  
      paste0("Neuroticism: ", round(mean(datasetInput()$Neuroticism, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Neuroticism, na.rm = T), digits = 2),
             ")")     
    })
    
    output$big5i <- renderText({  
      paste0("Imagination: ", round(mean(datasetInput()$Imagination, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Imagination, na.rm = T), digits = 2),
             ")")     
    })
    
# Below 4 code chunks score AUDIT sub-categories.
    
    output$cons <- renderText({  
      paste0("Consumption: ", round(mean(datasetInput()$Consumption, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Consumption, na.rm = T), digits = 2),
             ")")     
    })
    
    output$dep <- renderText({  
      paste0("Dependance: ", round(mean(datasetInput()$Dependance, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Dependance, na.rm = T), digits = 2),
             ")")     
    })
    
    output$probs <- renderText({  
      paste0("Problems: ", round(mean(datasetInput()$Problems, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$Problems, na.rm = T), digits = 2),
             ")")     
    })
    
    output$AUDITtot <- renderText({  
      paste0("Total Score: ", round(mean(datasetInput()$AUDITtot, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$AUDITtot, na.rm = T), digits = 2),
             ")")     
    })
    
  # The below 4 code chunks score O-LIFE sub-categories.
   
    output$UnEx <- renderText({  
      paste0("Unusual Experiences: ", round(mean(datasetInput()$OLA, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$OLA, na.rm = T), digits = 2),
             ")")     
    })
    
    
    output$IntAn <- renderText({  
      paste0("Introverted Anhedonia: ", round(mean(datasetInput()$IntAnh, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$IntAnh, na.rm = T), digits = 2),
             ")")     
    })
    
    
    output$CogDis <- renderText({  
      paste0("Cognitive Distortions: ", round(mean(datasetInput()$OCogDis, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$OCogDis, na.rm = T), digits = 2),
             ")")     
    })
    
    output$ImpNon <- renderText({  
      paste0("Impulsive Nonconformity: ", round(mean(datasetInput()$NonConf, na.rm = T), digits = 2),
             " (",
             round(sd(datasetInput()$NonConf, na.rm = T), digits = 2),
             ")")     
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

