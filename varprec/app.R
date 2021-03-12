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
fs<-read.csv("fallowsurvey.csv")

fs$hhsize<-ifelse(fs$adults!="6+",as.numeric(as.character(fs$adults)),6)+sample(0:3,replace=TRUE,prob=c(4,3,2,1),size = 1479)
fs$hhsize[is.na(fs$hhsize)]<-1



# Define UI for application that draws a histogram
ui <- fluidPage(

   sliderInput("Sample",
                        "SampleSize",
                        min = 10,
                        max = 250,
                        value = 10,step = 10),



           plotOutput("precision"),plotOutput("plot2"),

    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$precision <- renderPlot({
       tmp1<- fs[1:as.numeric(input$Sample),]
ggplot(tmp1,aes(y=hhsize,x=1,group=1))+
    geom_jitter(width=0.05,height=0)+
    stat_summary(aes(x=0.625),col="red",size=1)+
    geom_boxplot(aes(x=1.5),width=0.25,col="blue",size=2)+
    ggtitle(paste("Household Size: N =",input$Sample),
            subtitle=paste("Standard Deviation = ",round(sd(tmp1$hhsize),1),"; Standard Error =",round(sd(tmp1$hhsize)/sqrt(nrow(tmp1)),1)))+
    scale_y_continuous(limits=c(1,9.5),breaks=1:9)+
    theme_minimal()+
    xlim(0.5,1.75)+
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    xlab("")+
    geom_vline(xintercept=c(0.75,1.25))+
    ylab("Household Size")+
    annotate(x=c(0.625,1.5,1),y=c(9,9,9),label=c("Precision\nMean + Error bar","Variability\nBoxplot","Data"),geom="label",size=6)

    })


}

# Run the application
shinyApp(ui = ui, server = server)
