
########## load packages########      

#
#
#

library(rdrop2)
library(ggpubr)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
library(shiny)
library(shinymanager)
library(plotly)
library(reshape2)
library(shinyWidgets)
library(shinythemes)
library(data.table)
library(dplyr)
library(ggcyto)
library(BiocManager)
library(rsconnect)
library(shinyMobile)
options(repos = BiocManager::repositories())
rsconnect::setAccountInfo(name='bramcharitarmskcc', token='6F68B559EC7D56A5CF531E07DC066D14', secret='vYL8TqCfzH2piZ1RRZJrCXK84SZdTdblbnNQotnB')


##################### statsdata import/melt#####################
#setwd("C:/Users/RamcharB/Documents/QCapp")
#setwd("/Users/benjaminramcharitar/Downloads/QCapp")

#setwd("D:/QCapp")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
statsfort1 <- read.csv("data/Fort1stats.csv")
statsfort3 <- read.csv("data/Fort3stats.csv")
#token <- readRDS("data/token.rds")
#drop_acc(dtoken = token)
drop_auth(rdstoken = "data/token.rds")
#QC$DATE2 <-as.Date(QC$DATE)


statsfort1$date_median = as.Date(statsfort1$date_median,format='%d-%b-%Y')
statsfort1$date_rCV = as.Date(statsfort1$date_rCV,format='%d-%b-%Y')
#QC$nozzle2<-as.character(QC$nozzle2)
statsfort1$cluster_median<-as.character(statsfort1$cluster_median)
statsfort1$cluster_rCV<-as.character(statsfort1$cluster_rCV)

statsfort1median<-select(statsfort1,contains("median"))
statsfort1median = subset(statsfort1median, 
                     select = -c(X_median,pred.flexclust_median,filename_median))
statsfort1median$date_median = as.Date(statsfort1median$date_median,format='%d-%b-%Y')

medianmelt <- reshape2::melt(statsfort1median, id=c("date_median","tubename_median","cluster_median","event_median"))
medianmelt$date_median = as.Date(medianmelt$date_median,format='%d-%b-%Y')


statsfort1rCV<-select(statsfort1,contains("rCV"))
statsfort1rCV = subset(statsfort1rCV, 
                  select = -c(X_rCV,pred.flexclust_rCV,filename_rCV))
statsfort1rCV$date_rCV = as.Date(statsfort1rCV$date_rCV,format='%d-%b-%Y')
rCVmelt <- reshape2::melt(statsfort1rCV, id=c("date_rCV","tubename_rCV","cluster_rCV","event_rCV"))
rCVmelt$date_rCV = as.Date(rCVmelt$date_rCV,format='%d-%b-%Y')



statsfort3 <- read.csv("data/Fort3stats.csv")
statsfort3$date_median = as.Date(statsfort3$date_median,format='%d-%b-%Y')
statsfort3$date_rCV = as.Date(statsfort3$date_rCV,format='%d-%b-%Y')
statsfort3$cluster_median<-as.character(statsfort3$cluster_median)
statsfort3$cluster_rCV<-as.character(statsfort3$cluster_rCV)

statsfort3median<-select(statsfort3,contains("median"))
statsfort3median = subset(statsfort3median, 
                     select = -c(X_median,pred.flexclust_median,filename_median))

statsfort3median$date_median = as.Date(statsfort3median$date_median,format='%d-%b-%Y')
fort3medianmelt <- reshape2::melt(statsfort3median, id=c("date_median","tubename_median","cluster_median","event_median"))
fort3medianmelt$date_median = as.Date(statsfort3median$date_median,format='%d-%b-%Y')


statsfort3rCV<-select(statsfort3,contains("rCV"))
statsfort3rCV = subset(statsfort3rCV, 
                  select = -c(X_rCV,pred.flexclust_rCV,filename_rCV))
statsfort3rCV$date_rCV = as.Date(statsfort3rCV$date_rCV,format='%d-%b-%Y')
fort3rCVmelt <- reshape2::melt(statsfort3rCV, id=c("date_rCV","tubename_rCV","cluster_rCV","event_rCV"))
fort3rCVmelt$date_rCV = as.Date(fort3rCVmelt$date_rCV,format='%d-%b-%Y')
fort3rCVmelt <- fort3rCVmelt[fort3rCVmelt$value >= 0, ]
fort3rCVmelt<-fort3rCVmelt[!is.infinite(fort3rCVmelt$value),]



########import RAW data############
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#fort1raw <- read.csv("data/fort1RAW.csv")
fort1raw <- readRDS("data/fort1raw.rds")


#drop_download("fort1raw.rds", local_path = "fort1raw.rds",
#              overwrite = TRUE)
#fort1raw <- readRDS("fort1raw.rds")


fort1raw$Date = as.Date(fort1raw$Date,format='%d-%b-%Y')
fort1raw$cluster<-as.character(fort1raw$cluster)

datesubset<-subset(fort1raw,Date=="2021-04-30")



#########!!!!!!!!START######################
n <- 200
vars <- setdiff(names(statsfort1median), "")
varsQC3 <- setdiff(names(statsfort1rCV), "")
varsmelt<-setdiff((medianmelt$cluster_median), "")
varsrCV<-setdiff((rCVmelt$cluster_rCV), "")

varsFort3medianmelt<-setdiff((fort3medianmelt$cluster_median), "")
varsFort3rCVmelt<-setdiff((fort3rCVmelt$cluster_rCV), "")

fort1rawlist = as.list(names(fort1raw))
fort1rawlist = fort1rawlist[-c(1:3,22:26)]

#########Define the UI###########
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = TRUE, width = NULL, collapsed = TRUE),
  dashboardBody(
  fluidRow(style='padding-left:50px;',column(3,align="left",titlePanel("QC App")),
     column(7,align="right",img(src = 'Flow Cytometry - blue.png', height = '75px', width = '250px'))),
  tabsetPanel(
    tabPanel("Fortessa 1",style='padding-left:50px;',style='padding-right:50px;',
        fluidRow(splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          cellWidths = c("10%","30%", "30%"),
     selectInput("fort1rawlist",label = "Choose a Channel",choices = fort1rawlist),
     dateInput('date',label = 'Choose Date',value = Sys.Date()-35))),
     
     fluidRow(
       splitLayout(cellWidths = c("50%", "50%"), 
     
     plotOutput('dateplot'),
     plotOutput('FSCdateplot'))),
  
     radioGroupButtons('ycol4', 'Select Peak', varsrCV, selected = varsrCV[[3]],size = "lg"),
  plotlyOutput('plot4'),
  
  
  radioGroupButtons('ycol3', 'Select Peak', varsmelt, selected = varsmelt[[3]],size = "lg"),
  plotlyOutput('plot3'),
  
  
  selectInput('ycol2', 'Channel', varsQC3, selected = varsQC3[[7]]),
  plotlyOutput('plot2'),
  
  selectInput('ycol', 'Channel', vars, selected = vars[[6]]),
  plotlyOutput('plot')
    ),
  tabPanel("Fortessa 3",
    radioGroupButtons('ycol5', 'Select Peak', varsFort3medianmelt, selected = varsFort3medianmelt[[3]],size = "lg"),
       plotlyOutput('plot5'),
  selectInput('ycol6', 'cluster', varsFort3rCVmelt, selected = varsFort3rCVmelt[[3]]),
  plotlyOutput('plot6')
),

tabPanel("Aria 1"),
tabPanel("Cytoflex"),
tabPanel("Aurora")

)))


# Define the server code
server <- function(input, output) {

  
  fort1datesubset<-reactive({subset(fort1raw,Date==as.character(input$date))
  })
  
  fort3rCVmeltselected <- reactive({subset(fort3rCVmelt,cluster_rCV==input$ycol6)
  })
  
  fort3medianmeltselected <- reactive({subset(fort3medianmelt,cluster_median==input$ycol5)
  })
  
  rCVmeltselected <- reactive({subset(rCVmelt,cluster_rCV==input$ycol4)
  })
  
  medianmeltselected <- reactive({subset(medianmelt,cluster_median==input$ycol3)
  })
  
  selectedData <- reactive({
    statsfort1median[, c("date_median","cluster_median", input$ycol)]
  })
  
  peak2data <- reactive({
    statsfort1rCV[, c("date_rCV","cluster_rCV", input$ycol2)]
  })
  
  
  
  output$dateplot <- renderPlot({
    ggplot(fort1datesubset(),aes_string(x=input$fort1rawlist))+
             geom_histogram(aes(fill=fort1datesubset()$cluster),bins=1000)+
                              scale_x_flowjo_biexp(limits=c(-10,2e5))+
      guides(fill=guide_legend(title="Cluster"))+
      labs(title= as.character(input$date),x =as.character(input$fort1rawlist))+
      theme_light()
    
    
  })
  
  output$FSCdateplot <- renderPlot({
    
    ggplot(fort1datesubset(),aes(x=fort1datesubset()$FSC.A,y=fort1datesubset()$SSC.A))+
      stat_bin_2d(geom = "hex", aes(alpha = (..density..), fill = fort1datesubset()$cluster),
                  bins=50,drop=TRUE,binwidth=600)+scale_alpha_continuous(range = c(0,2.5),trans="log")+
      scale_y_continuous(limits=c(0,1e5))+scale_x_continuous(limits=c(0,2e5))+
      labs(x="FCS-A",y="SSC-A")+
      theme_classic2()+theme(text = element_text(size = 30),legend.position = "none")
    

 
    
    
  })
  
  
  
  output$plot <- renderPlotly({
    ggplotly(ggplot(selectedData(),aes_string(y = input$ycol))+
 geom_point(aes(x=statsfort1median$date_median,colour=cluster_median))+labs(title="Median",x ="Date")+
   guides(colour=guide_legend(title="Cluster"))+theme_pubr()+
   
   theme(text = element_text(size = 20),axis.text.x = element_text(angle = 45)),dynamicTicks = TRUE)
    
 
     })
  
  output$plot2 <- renderPlotly({
    ggplotly(ggplot(peak2data(),aes_string(y = input$ycol2))+
     geom_point(aes(x=statsfort1rCV$date_rCV, colour=statsfort1rCV$cluster_rCV))+labs(title="rCV",x ="Date")+
       guides(colour=guide_legend(title="Cluster"))+theme_pubr()+
      theme(axis.text.x = element_text(angle = 45)),dynamicTicks = TRUE)
    
  })
  
  
  
  
  output$plot3 <- renderPlotly({
  plot_ly(z = medianmeltselected()$value, 
          x = medianmeltselected()$date_median, 
          y = medianmeltselected()$variable, text=c(medianmeltselected()$event_median),
          type = "heatmap", showscale=TRUE,hovertemplate="Date: %{x}<br>Value: %{z}<br>Channel: %{y}<br>Event:%{text}<extra></extra>",
          reversescale =F,zauto=F,colorscale = list(c(0,'#440154FF'),
                                                    c(0.2,'#414487FF'),
                                                    c(0.3,'#2A788EFF'),
                                                    c(0.5, '#22A884FF'),
                                                    c(0.95,'#7AD151FF'),
                                                    c(1,'#FDE725FF')
          ))
  
  })
  
  output$plot4 <- renderPlotly({
    plot_ly(z = rCVmeltselected()$value, 
            x = rCVmeltselected()$date_rCV, 
            y = rCVmeltselected()$variable, 
            type = "heatmap", showscale=TRUE,
            reversescale =F,zauto=F,colorscale = list(c(0,'#440154FF'),
                                                      c(0.1,'#414487FF'),
                                                      c(0.3,'#2A788EFF'),
                                                      c(0.4, '#22A884FF'),
                                                      c(0.95,'#7AD151FF'),
                                                      c(1,'#FDE725FF')
                                                      
            ),zauto = F,
            zmax = 30,zmin=0)
    
  })
  
  
  output$plot5 <- renderPlotly({
    plot_ly(z = fort3medianmeltselected()$value, 
            x = fort3medianmeltselected()$date_median, 
            y = fort3medianmeltselected()$variable, text=c(fort3medianmeltselected()$event_median),
            type = "heatmap", showscale=TRUE,hovertemplate="Date: %{x}<br>Value: %{z}<br>Channel: %{y}<br>Event:%{text}<extra></extra>",
            reversescale =F,zauto=F,colorscale = list(c(0,'#440154FF'),
                                                      c(0.2,'#414487FF'),
                                                      c(0.3,'#2A788EFF'),
                                                      c(0.5, '#22A884FF'),
                                                      c(0.95,'#7AD151FF'),
                                                      c(1,'#FDE725FF')
            ))
    
  })
  
  
  output$plot6 <- renderPlotly({
    plot_ly(z = fort3rCVmeltselected()$value, 
            x = fort3rCVmeltselected()$date_rCV, 
            y = fort3rCVmeltselected()$variable, 
            type = "heatmap", showscale=TRUE,zauto = F,
            zmax = 30,zmin=0,
            reversescale =F,colorscale = list(c(0,'#440154FF'),
                                                      c(0.1,'#414487FF'),
                                                      c(0.3,'#2A788EFF'),
                                                      c(0.4, '#22A884FF'),
                                                      c(0.95,'#7AD151FF'),
                                                      c(1,'#FDE725FF')
            ))
    
  })
  
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)





