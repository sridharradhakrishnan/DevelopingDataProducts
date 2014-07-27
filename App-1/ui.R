library(shiny)

# ui.R

shinyUI(fluidPage(
  titlePanel("Natural Disaster Information"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Natural disaster information with 
        information from U.S. National Oceanic and Atmospheric Administration’s (NOAA) storm database."),
      helpText("You will see a bar graph based on the selection(s) below"),
      h5("Initial load will take few additonal seconds...."),
      
      selectInput("disaster", 
                  label = h4("Choose a natural disaster to display"),
                  choices = c("FLOOD", "TORNADOES","HURRICANES",
                              "EXCESSIVE HEAT","WILD FIRES","HIGH WINDS"), selected = "FLOOD"),
      
      radioButtons("impact", label = h4("Choose the impact study"),
                   choices = list("Human" = 1, "Economic" = 2), 
                   selected = 1),
      
      dateRangeInput("dates", label = h4("Date range"), format="mm-dd-yyyy",start="01/01/1950",
                     end="12/12/2011",min="01/01/1950",max="01/01/2011")
    ),
    
    mainPanel(
      textOutput("text1"),
      plotOutput("plot1")
    )
  )
))