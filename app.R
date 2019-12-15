#
#title: "Developing Data Products: Shiny App Assignment"
#author: "Becky Reimer"
#date: "12/15/2019"
#
# This is a Shiny web application. It includes an interactive plot showing the 
# survival rates of passengers on the Titanic, using the Titanic dataset that 
# is included in R. It allows us to explore the question:
#  Which types of passengers were most likely to survive the Titanic?
#
# You can run the application by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
data(Titanic)


# Define UI for application
ui <- fluidPage(
                               
    titlePanel("Survival Rates on the Titanic Based on Passenger Class"),    
    sidebarLayout(                                 
      sidebarPanel(
        h3("Select Passenger Characteristics"),
        checkboxGroupInput(inputId = "age_group",
                           label = "Select age groups(s):",
                           choices = levels(titanic$Age),
                           selected = c("Adult", "Child")),
        checkboxGroupInput(inputId = "gender",
                  label = "Select gender(s):",
                  choices = levels(titanic$Sex),
                  selected = c("Male", "Female")),
        submitButton("Submit")
      ),
      mainPanel(   
        h3("Display Survival Rates"),
        plotOutput("plot1"),
        h3("Total passengers included in plot:"),
        textOutput("sum1")
      )
    )

)

# Define server logic required to draw plot
server <- function(input, output) {

      titanic <- as.data.frame(Titanic)
      titanic <- titanic[order(titanic$Class, titanic$Age, titanic$Sex),]
      
      titanic_subset <- reactive( {  
          req(input$gender)
          titanic0 <- subset(titanic, Sex %in% input$gender & Age %in% input$age_group) 
    
          titanic1 <- group_by(titanic0, Class)
          titanic2 <- summarize(titanic1, passengers = sum(Freq, na.rm=TRUE))
  
          titanic_sub <- titanic1[(titanic1$Survived == "Yes"), ]
          titanic_sub2 <- summarize(titanic_sub, surv_pass = sum(Freq, na.rm=TRUE))
  
          titanic_rates <- arrange(join(titanic_sub2,titanic2),Class)
          titanic_rates <- mutate(titanic_rates, surv_rate = surv_pass/passengers)
      })
      
      output$plot1 = renderPlot({  
        ggplot(data=titanic_subset(), aes(x=Class, y=surv_rate, fill=passengers)) +
          geom_bar(stat = "identity") + 
          scale_y_continuous(labels = function(y) paste0(y*100, "%")) +
          expand_limits(y=1) +
          xlab("Passenger Class") + ylab("Survival Rate")
      })
  
      output$sum1 <- reactive({
        sumData <- titanic[(titanic$Sex %in% input$gender &
                            titanic$Age %in% input$age_group), ]
        sum(sumData$Freq)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

