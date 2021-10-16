#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Title: 'Project 3: Machine Learning and Data Science Survey'
# Author: by William Aiken, Bikram Barua, Catherine Cho, Eric Lehmphul, Nnaemeka Newman
#
library(shiny)
library(ggplot2)
library(dplyr)

library(stringr)
library(tidyverse)
library(tidyr)
library(readr)

urlfile<-"https://raw.githubusercontent.com/baruab/msdsrepo/main/Project_3_607/kaggle-survey-2018/multipleChoiceResponses.csv"
survey_raw<-read_csv(url(urlfile))

#subsetting the data
survey<-subset(survey_raw,select=c(2,5:8,12,13))

#filtering out data listed under data scientist only
survey_ds<-filter(survey,survey$Q6=="Data Scientist")
survey_ds<-subset(survey_ds,select=-c(5))

#renaming columns
colnames(survey_ds)<-c("gender","country","highest_degree","undergrad_deg","yrs_of_exp","salary")

#barplot of raw count of different levels of education
p<-ggplot(survey_ds,aes(x=factor(highest_degree)))+
    geom_bar(stat="count",width=0.7,fill="steelblue")+
    theme_minimal()
q<-p+aes(stringr::str_wrap(highest_degree,15))+xlab(NULL)
q<-q+coord_flip()


#Is there a correlation between salary and highest degree earned?
undergrad<-unique(survey_ds$undergrad_deg)
survey_ds$salary<-str_replace_all(survey_ds$salary,"(\\d|\\d+)-","") 
survey_ds$salary<-str_replace_all(survey_ds$salary,",","")
survey_ds$salary<-as.numeric(survey_ds$salary)
survey_ds$salary<-survey_ds$salary/1000

survey_ds$yrs_of_exp <-  gsub(".*-", "", survey_ds$yrs_of_exp)
survey_ds$yrs_of_exp <- as.numeric(survey_ds$yrs_of_exp)

##############################################################

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data Scientist Salary Survey Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("expInput", "Years Of Exp", 0, 40, c(2, 10), pre = ""),
            radioButtons("genderInput", "Gender",
                         choices = c("Male", "Female"),
                         selected = "Male"),
            uiOutput("countryOutput"),
            uiOutput("highestDegreeOutput")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("coolplot"),
            br(), br(),
            tableOutput("results")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$countryOutput <- renderUI({
        selectInput("countryInput", "Country",
                    sort(unique(survey_ds$country)),
                    selected = "United States of America")
    })
    
    output$highestDegreeOutput <- renderUI({
        selectInput("highestDegreeInput", "Highest Degree",
                    sort(unique(survey_ds$highest_degree)),
                    selected = "Master's degree")
    })
    
    filtered <- reactive({ 
        if (is.null(input$countryInput)) {
            return(NULL)
        }
        if (is.null(input$highestDegreeInput)) {
            return(NULL)
        }
        
        survey_ds %>%
        filter(yrs_of_exp >= input$expInput[1],
               yrs_of_exp <= input$expInput[2],
               gender == input$genderInput,
               country == input$countryInput,
               highest_degree == input$highestDegreeInput
        )
    })
      
    output$coolplot <- renderPlot({
       if (is.null(filtered())) {
           return()
       }
        
       ggplot(filtered(), aes(salary)) + geom_histogram()    
    })
    
    output$results <- renderTable({
        filtered()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
