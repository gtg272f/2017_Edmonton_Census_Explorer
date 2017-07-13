#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)

library(mapproj)
library(ggmap)
library(rgdal)
library(plotly)
library(broom) #tidy

datasetNames=c( "Population by Employment Sector",
                "Population by Employment Status",
                "Population by Household Income",
                "Population by Mode of Transportation",
                "Dwelling Unit By Ownership",
                "Population by Highest Educational Attainment",
                "Population by Age Range",
                "Population By Dwelling Unit Status",
                "Dwelling Unit By Language")



#dataFileDirLoc="C:/Users/markk/Desktop/Coursera/Data_Science_(John_Hopkins)/Developing Data Products/Week 4/R Files/Census_explore/Data/"
dataFileDirLoc="Data/"

dataFile1="2016_Census_-_Population_by_Employment_Sector___Neighbourhood_Ward_.csv"
dataFile2="2016_Census_-_Population_by_Employment_Status__Neighbourhood_Ward_.csv"
dataFile3="2016_Census_-_Population_by_Household_Income__Neighbourhood_Ward_.csv"
dataFile4="2016_Census_-_Population_by_Mode_of_Transportation__Neighbourhood_Ward_.csv"
dataFile5="2016_Census_-_Dwelling_Unit_By_Ownership__Neighbourhood_.csv"
dataFile6="2016_Census_-_Population_by_Highest_Educational_Attainment__Neighbourhood_Ward_.csv"
dataFile7="2016_Census_-_Population_by_Age_Range__Neighbourhood_Ward_.csv"
dataFile8="2016_Census_-_Population_By_Dwelling_Unit_Status__Neighbourhood_.csv"
dataFile9="2016_Census_-_Dwelling_Unit_By_Language__Neighbourhood_Ward_.csv"



data_EmpSect <- read.csv(paste(dataFileDirLoc,dataFile1,sep="")) 
data_EmpStat <- read.csv(paste(dataFileDirLoc,dataFile2,sep=""))
data_HouseIncome <- read.csv(paste(dataFileDirLoc,dataFile3,sep=""))
data_ModeTrans <- read.csv(paste(dataFileDirLoc,dataFile4,sep=""))
data_DwellOwner <- read.csv(paste(dataFileDirLoc,dataFile5,sep=""))
data_HighestEdu <- read.csv(paste(dataFileDirLoc,dataFile6,sep=""))
data_AgeRange <- read.csv(paste(dataFileDirLoc,dataFile7,sep=""))
data_DwellStat <- read.csv(paste(dataFileDirLoc,dataFile8,sep=""))
data_DwellLang <- read.csv(paste(dataFileDirLoc,dataFile9,sep=""))

dataSet=list("Population by Employment Sector"=data_EmpSect,
             "Population by Employment Status"=data_EmpStat,
             "Population by Household Income"=data_HouseIncome,
             "Population by Mode of Transportation"=data_ModeTrans,
             "Dwelling Unit By Ownership"=data_DwellOwner,
             "Population by Highest Educational Attainment"=data_HighestEdu,
             "Population by Age Range"=data_AgeRange,
             "Population By Dwelling Unit Status"=data_DwellStat,
             "Dwelling Unit By Language"=data_DwellLang)

#KMLFile="City of Edmonton - Neighbourhood Boundaries (with Wards).kml"

#shpData <- readOGR(dsn=paste(dataFileDirLoc,KMLFile,sep=""),pointDropZ=TRUE)
#shpData <- spTransform(shpData,CRS("+proj=longlat +datum=WGS84"))
#c<-shpData@polygons

#map <- get_map(location = 'Edmonton, Alberta', zoom = 10)
#g<-ggmap(map)

#count=length(c)
#data_Map<-fortify(c[[1]])
#data_Map$Neighbourhood=rep(toupper(shpData$Name[1]),nrow(data_Map))

#for (x in 2:count){
#  data_Map_temp<-fortify(c[[x]])
#  data_Map_temp$Neighbourhood=rep(toupper(shpData$Name[x]),nrow(data_Map_temp))
#  data_Map<-rbind(data_Map,data_Map_temp)
#}

#data_EmpSect 4:20 
#data_EmpStat 4:15 11
#data_HouseIncome 4:12
#data_ModeTrans 4:10
#data_DwellOwner 4:6
#data_HighestEdu 4:15
#data_AgeRange 4:22
#data_DwellStat 4:7
#data_DwellLang 4:16
# 
# data_All<-merge(data_EmpSect,data_EmpStat,by="Neighbourhood.Number") #34
# data_All<-merge(data_All,data_HouseIncome,by="Neighbourhood.Number")
# data_All<-merge(data_All,data_ModeTrans,by="Neighbourhood.Number")
# data_All<-merge(data_All,data_DwellOwner,by="Neighbourhood.Number")
# data_All<-merge(data_All,data_HighestEdu,by="Neighbourhood.Number")
# data_All<-merge(data_All,data_AgeRange,by="Neighbourhood.Number")
# data_All<-merge(data_All,data_DwellStat,by="Neighbourhood.Number")
# data_All<-merge(data_All,data_DwellLang,by="Neighbourhood.Number")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    data_Var1 <- reactive({dataSet[[input$DD1]]})
    data_Var2 <- reactive({dataSet[[input$DD3]]})
    q<-reactive({factor(data_Var1()[,"Neighbourhood.Name"],data_Var1()[,"Neighbourhood.Name"][order(data_Var1()[,input$DD2],decreasing=TRUE)])})
    w<-reactive({data.frame(q(),data_Var1()[,input$DD2])})
    e<-reactive({
      w()[order(w()[,2],decreasing = TRUE),]
    })
    
    #data_Pareto<-reactive({})
    #data_Map_x<-reactive({merge(data_Map,data_Var1,by.x="Neighbourhood",by.y="Neighbourhood.Name")})
    
  output$Var1 <- renderUI({
    selectInput("DD2", "Primary Variable", choices=names(data_Var1()[-(1:3)]))
  })
  
  
  output$Var2 <- renderUI({
    selectInput("DD4", "Secondary Variable", choices=names(data_Var2()[-(1:3)]))
  })
  
  output$PLOT1=renderPlotly({ 
    ggplot(data_Var1(), aes(data_Var1()[input$DD2])) +geom_histogram(binwidth=20)+ xlab(input$DD2)+ggtitle("Histogram of responses")
    })
  output$TABLE1=renderTable(rownames=TRUE,bordered = TRUE,colnames = FALSE,{
    data.frame("Statistic"=c("Median"=median(data_Var1()[input$DD2][,1]),"Mean"=mean(data_Var1()[input$DD2][,1]),"Std. Dev."=sd(data_Var1()[input$DD2][,1]),"Max"=max(data_Var1()[input$DD2][,1]),"Min"=min(data_Var1()[input$DD2][,1]),"Range"=max(data_Var1()[input$DD2][,1])-min(data_Var1()[input$DD2][,1])))
    })
  
  output$PLOT2=renderPlotly({ 
    #ggplot(data=e()[1:10,])+geom_col(aes(x="Neighbourhood.Name",y=input$DD2))+theme(axis.text.x=element_text(angle=90,hjust=1))
    ggplot(data=e()[1:10,])+geom_col(aes(x=e()[1:10,1],y=e()[1:10,2]))+theme(axis.text.x=element_text(angle=90,hjust=1))+ xlab("Neighbourhood.Name")+ ylab("Count")+ggtitle("Pareto by Neightbourhood")
    })
  output$TABLE2=renderTable(rownames=FALSE,bordered = TRUE,colnames = FALSE,{
    e()[1:10,]
  })
  
  #e<-geom_polygon(aes( x=long,y=lat,group=Neighbourhood,fill =Data),data = a, color =NA,alpha = .4, size = .2,show.legend = TRUE,inherit.aes = FALSE)
  #f<-ggplotly(p=g+e,width=1000,height=1000)
  #f<-ggplot(p=g+e,width=1000,height=1000)
  #print(f)
  
  
  })
  

