
library("sf")
library("tidyverse")
library("raster")
library("stringr")
library(mapview)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("Where are our students now?"),
   mapviewOutput("PCCRC_Alum_Whereabouts"),
   p()
   )


server <- function(input, output) {
   
  output$PCCRC_Alum_Whereabouts <- renderMapview({
     con<-url("https://www.dropbox.com/s/6t1hjb5rvectfx4/Alumni.txt?raw=1")
     URLdat    <- readLines(con)
     close(con)
     dat <- str_split(URLdat,"\t")
     datMat<-as.tibble(matrix(unlist(dat),nrow=length(URLdat),ncol=length(dat[[1]]),byrow=T))[-1,]
     colnames(datMat)<-dat[[1]]
     datMat<-datMat[-1,]
     datMat$LAT<-as.numeric(datMat$LAT)
     datMat$LAT <- jitter(datMat$LAT, factor = 1.5)
     datMat$LONG<-as.numeric(datMat$LONG)
     datMat$LONG <- jitter(datMat$LONG, factor = 1.2)
     datMat$Dept<-as.character(datMat$Dept)
     datMat<-as.tibble(datMat)
     
     #color by departent
     # if(input$checkGroup=="Dept"){datMat_sub<-datMat[-which(datMat$Dept=="NA"),]}
     # if(input$checkGroup=="Degree"){datMat_sub<-datMat[-which(datMat$Degree=="NA"),]}
     # if(input$checkGroup=="EmployerCode"){datMat_sub<-datMat[-which(datMat$EmployerCode=="NA"),]}
     #
     datMat_sub<-datMat[-which(datMat$Dept=="NA"),]
     datMat_sub$Lab<-paste(unlist(datMat_sub[2]),unlist(datMat_sub[1]),sep=", ")
     
     PCCRC_alum<- st_as_sf(x=datMat_sub, 
                           coords = c("LONG", "LAT"),
                           crs = (4326))  # this is a longlat coord ref system
    mapView(PCCRC_alum,label=datMat_sub$Lab)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

