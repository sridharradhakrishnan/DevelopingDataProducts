library(shiny)
library("plyr")

# server.R

# Read the data from the data file.  This data file has been cleansed
dfsmall <- read.csv("data/datafile.csv", stringsAsFactors=FALSE,header=TRUE)

## Start of the main server program

shinyServer(
  function(input, output) {
  
    ## Get the input selections
    disaster <- reactive({input$disaster})
    impactselected <- reactive({input$impact})
    year1 <- reactive({paste(format(input$dates[1],"%m/%d/%Y"),sep="")})
    year2 <- reactive({paste(format(input$dates[2],"%m/%d/%Y"),sep="")})

    ## Subset the data based on the dates and events selected
    r4 <- reactive({dfsmall[grep(disaster(),dfsmall$EVTYPE),]})
    r5 <- reactive({r4d <- r4()
                    d1 <- as.POSIXct(r4d$BGN_DATE,format="%m/%d/%Y")
                    d2 <- as.POSIXct(input$dates[1],format="%m/%d/%Y")
                    d3 <- as.POSIXct(input$dates[2],format="%m/%d/%Y")
                    subset(r4d,((d1 >= d2) & (d1 <= d3)))
                  })
    r5rows <- reactive({nrow(r5())})
    
    
    ## Output information on the number of observations
    output$text1 <- renderText(function() {
      if (r5rows() > 0)
        paste("Total number of observations is",r5rows())
      else
        paste("Total number of observations is ZERO and hence NO graph to display")
    })
    
    ## Print the plot based on the data availability and impact study selected (radio button)
    output$plot1 <- renderPlot(function() { 
      if ((r5rows() > 0) & (impactselected() == "1")) {
        r5d <- r5()
        r6 <- ddply(r5d,.(STATE),colwise(sum, c("HARMED")))
        title <- reactive({paste("Human Impacts of ",disaster(), " (",year1()," - ",year2(),")",sep="")})
        barplot(r6$HARMED,las=2,names=r6$STATE,cex.names=0.5,cex.axis=0.5,
                horiz=FALSE,space=5.0,main=title(),
                xlab="States in USA",ylab="Fatalities + Injuries")
      }
      if ((r5rows() > 0) & (impactselected() == "2")) {
        r5d <- r5()
        r6 <- ddply(r5d,.(STATE),colwise(sum, c("TOTALCASH")))
        title <- reactive({paste("Economic Impacts of ",disaster(), " (",year1()," - ",year2(),")",sep="")})
        barplot(r6$TOTALCASH,las=2,names=r6$STATE,cex.names=0.5,cex.axis=0.5,
                horiz=FALSE,space=5.0,main=title(),
                xlab="States in USA",ylab="Crop and Property Damage Costs in Millions")
      }
    })
    
  }
)