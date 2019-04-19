library(shiny)
library(ggplot2)
library(httr)
library(reshape2)
library(stringr)
library(dplyr)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Application title
   titlePanel("PCCRC Funding"),
   
   selectInput(inputId = "AwardTypeSelected",
               label = "Award Type",
               choices = list("All"="All","ResearchProjects"="ResearchProjects","Misc"="Misc","GraduateFellowship"="GraduateFellowship"),
               selected = "All"),
   
   selectInput(inputId = "Explan",
               label = "Explantory Variable",
               choices = list("Year"="Year","Subject"="Subject","Species"="Species"),
               selected = "Year"),
   
   checkboxInput(inputId = "Breakdown",
                 label = strong("Show detailed breakdown"),
                 value = FALSE),
   
   conditionalPanel(condition = "input.Breakdown==true && (input.Explan == 'Species')",
                    selectInput(inputId = "BreakdownC1",
                                  choices=c("Year","Subject"),
                                  label = strong("Show by Year/Subject"),
                                 selected = "Subject")),
                    
   conditionalPanel(condition = "input.Breakdown==true && input.Explan == 'Subject'",
                    selectInput(inputId = "BreakdownC2",
                                choices=c("Species","Year"),
                                label = strong("Show by Species/Year"),
                                selected = "Species")),
   
   conditionalPanel(condition = "input.Breakdown==true && input.Explan == 'Year'",
                    selectInput(inputId = "BreakdownC3",
                                choices=c("Species","Subject"),
                                label = strong("Show detailed"),
                                selected = "")),
   plotOutput(outputId = "main_plot", height = "500px",width="100%")
   
)

# Define server logic
server <- function(input, output) {
  x    <- read.csv(url("https://www.dropbox.com/s/22639uem7vvf0cp/PCCRCHistoryMelt.csv?raw=1"))
  CurYR<-unlist(str_split(Sys.Date(),"-"))[1]
  IndexYr<-sapply(1:nrow(x), function(i) unlist(strsplit(as.character(x$Year[i]),"X|\\."))[2])
  x<-x[which(IndexYr<=CurYR),]    
  output$main_plot<-renderPlot({
    #if award type = all
    if(input$AwardTypeSelected == "All"){ 
     if(input$Explan == "Year" && input$Breakdown==TRUE ){#
        if(input$BreakdownC3=="Species"){
          if(length(which(is.na(x$Species))) > 0){x<-x[-which(is.na(x$Species)),]}
          p <- ggplot(data=x,aes(x=Year,y=Value))+
            geom_bar(stat = "identity", position = "stack",aes(fill=x$Species))+
            theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=24))+
            scale_x_discrete(breaks=levels(x$Year),
                             labels=sapply(1:length(levels(x$Year)),
                                           function(i) unlist(str_split(string=levels(x$Year)[[i]],pattern="FY|X|\\.."))[2]))+
            scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
            guides(fill=guide_legend(title="Species"))
          
        }
        if(input$BreakdownC3=="Subject"){
          if(length(which(is.na(x$Subject))) > 0){x<-x[-which(is.na(x$Subject)),]}
          p <- ggplot(data=x,aes(x=Year,y=Value))+
            geom_bar(stat = "identity", position = "stack",aes(fill=x$SubjectArea))+
            theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
            scale_x_discrete(breaks=levels(x$Year),
                             labels=sapply(1:length(levels(x$Year)),
                                           function(i) unlist(str_split(string=levels(x$Year)[[i]],pattern="FY|X|\\.."))[2]))+
            scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
            guides(fill=guide_legend(title="Subject"))
        
        }
      }
      if(input$Explan=="Year" && input$Breakdown==FALSE){ #
        p <- ggplot(data=x,aes(x=Year,y=Value))+
          geom_bar(stat = "identity", position = "stack")+
          theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
          scale_x_discrete(breaks=levels(x$Year),
                           labels=sapply(1:length(levels(x$Year)),
                                         function(i) unlist(str_split(string=levels(x$Year)[[i]],pattern="FY|X|\\.."))[2]))+
          scale_y_continuous(name="Funding ($)", labels = scales::comma)  
        
      }
      #
      if(input$Explan=="Species" && input$Breakdown){
        if(input$BreakdownC1=="Subject"){
          if(length(c(which(is.na(x$Species)),which(is.na(x$Subject)))) > 0){x<-x[-c(which(is.na(x$Species)),which(is.na(x$Subject))),]}
          p <- ggplot(data=x,aes(x=Species,y=Value))+
            geom_bar(stat = "identity", position = "stack",aes(fill=x$SubjectArea))+
            theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
            scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
            guides(fill=guide_legend(title="Subject"))

        }
        if(input$BreakdownC1=="Year"){
          if(length(which(is.na(x$Species))) > 0){x<-x[-which(is.na(x$Species)),]}
          p <- ggplot(data=x,aes(x=Species,y=Value))+
            geom_bar(stat = "identity", position = "stack",aes(fill=x$Year))+
            theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
            scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
            scale_fill_discrete(name="Year",breaks=levels(x$Year),
                              labels=sapply(1:length(levels(x$Year)),
                                            function(i) unlist(str_split(string=levels(x$Year)[[i]],pattern="FY|X|\\.."))[2]))


      }
      }
      if(input$Explan=="Subject" && input$Breakdown){
        if(input$BreakdownC2=="Species"){
          if(length(c(which(is.na(x$Species)),which(is.na(x$Subject)))) > 0){x<-x[-c(which(is.na(x$Species)),which(is.na(x$Subject))),]}
          p <- ggplot(data=x,aes(x=SubjectArea,y=Value))+
            geom_bar(stat = "identity", position = "stack",aes(fill=x$Species))+
            theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
            scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
            guides(fill=guide_legend(title="Species"))

        }
        if(input$BreakdownC2=="Year"){
          if(length(which(is.na(x$Subject))) > 0){x<-x[-which(is.na(x$Subject)),]}
          p <- ggplot(data=x,aes(x=SubjectArea,y=Value))+
            geom_bar(stat = "identity", position = "stack",aes(fill=x$Year))+
            theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
            scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
            scale_fill_discrete(name="Year",breaks=levels(x$Year),
                                labels=sapply(1:length(levels(x$Year)),
                                              function(i) unlist(str_split(string=levels(x$Year)[[i]],pattern="FY|X|\\.."))[2]))
            }
      }
        if(input$Explan=="Subject" && input$Breakdown==FALSE){
          if(length(which(is.na(x$Subject))) > 0){x<-x[-which(is.na(x$Subject)),]}
          p <- ggplot(data=x,aes(x=SubjectArea,y=Value))+
              geom_bar(stat = "identity", position = "stack")+
              theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
              scale_y_continuous(name="Funding ($)", labels = scales::comma)
        }

     if(input$Explan=="Species" && input$Breakdown==FALSE){
       if(length(which(is.na(x$Species))) > 0){x<-x[-which(is.na(x$Species)),]}
       p <- ggplot(data=x,aes(x=Species,y=Value))+
         geom_bar(stat = "identity", position = "stack")+
         theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
         scale_y_continuous(name="Funding ($)", labels = scales::comma)
     }
    }
    #if award type != all
    if(input$AwardTypeSelected == "ResearchProjects" || input$AwardTypeSelected == "Misc" || input$AwardTypeSelected == "GraduateFellowship" ){
      x<-filter(x, AwardType == input$AwardTypeSelected)
    if(input$Explan == "Year" && input$Breakdown==TRUE ){#
      if(input$BreakdownC3=="Species"){
        if(length(which(is.na(x$Species))) > 0){x<-x[-which(is.na(x$Species)),]}
        p <- ggplot(data=x,aes(x=Year,y=Value))+
          geom_bar(stat = "identity", position = "stack",aes(fill=x$Species))+
          theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=24))+
          scale_x_discrete(breaks=levels(x$Year),
                           labels=sapply(1:length(levels(x$Year)),
                                         function(i) unlist(str_split(string=levels(x$Year)[[i]],pattern="FY|X|\\.."))[2]))+
          scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
          guides(fill=guide_legend(title="Species"))
        
      }
      if(input$BreakdownC3=="Subject"){
        if(length(which(is.na(x$Subject))) > 0){x<-x[-which(is.na(x$Subject)),]}
        p <- ggplot(data=x,aes(x=Year,y=Value))+
          geom_bar(stat = "identity", position = "stack",aes(fill=x$SubjectArea))+
          theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
          scale_x_discrete(breaks=levels(x$Year),
                           labels=sapply(1:length(levels(x$Year)),
                                         function(i) unlist(str_split(string=levels(x$Year)[[i]],pattern="FY|X|\\.."))[2]))+
          scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
          guides(fill=guide_legend(title="Subject"))
        
      }
    }
    if(input$Explan=="Year" && input$Breakdown==FALSE){ #
      p <- ggplot(data=x,aes(x=Year,y=Value))+
        geom_bar(stat = "identity", position = "stack")+
        theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
        scale_x_discrete(breaks=levels(x$Year),
                         labels=sapply(1:length(levels(x$Year)),
                                       function(i) unlist(str_split(string=levels(x$Year)[[i]],pattern="FY|X|\\.."))[2]))+
        scale_y_continuous(name="Funding ($)", labels = scales::comma)  
      
    }
    #
    if(input$Explan=="Species" && input$Breakdown){
      if(input$BreakdownC1=="Subject"){
        if(length(c(which(is.na(x$Species)),which(is.na(x$Subject)))) > 0){x<-x[-c(which(is.na(x$Species)),which(is.na(x$Subject))),]}
        p <- ggplot(data=x,aes(x=Species,y=Value))+
          geom_bar(stat = "identity", position = "stack",aes(fill=x$SubjectArea))+
          theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
          scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
          guides(fill=guide_legend(title="Subject"))
        
      }
      if(input$BreakdownC1=="Year"){
        if(length(which(is.na(x$Species))) > 0){x<-x[-which(is.na(x$Species)),]}
        p <- ggplot(data=x,aes(x=Species,y=Value))+
          geom_bar(stat = "identity", position = "stack",aes(fill=x$Year))+
          theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
          scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
          scale_fill_discrete(name="Year",breaks=levels(x$Year),
                              labels=sapply(1:length(levels(x$Year)),
                                            function(i) unlist(str_split(string=levels(x$Year)[[i]],pattern="FY|X|\\.."))[2]))
        
        
      }
    }
    if(input$Explan=="Subject" && input$Breakdown){
      if(input$BreakdownC2=="Species"){
        if(length(c(which(is.na(x$Species)),which(is.na(x$Subject)))) > 0){ x<-x[-c(which(is.na(x$Species)),which(is.na(x$Subject))),]}
        p <- ggplot(data=x,aes(x=SubjectArea,y=Value))+
          geom_bar(stat = "identity", position = "stack",aes(fill=x$Species))+
          theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
          scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
          guides(fill=guide_legend(title="Species"))
        
      }
      if(input$BreakdownC2=="Year"){
        if(length(which(is.na(x$Subject))) > 0){x<-x[-which(is.na(x$Subject)),]}
        p <- ggplot(data=x,aes(x=SubjectArea,y=Value))+
          geom_bar(stat = "identity", position = "stack",aes(fill=x$Year))+
          theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
          scale_y_continuous(name="Funding ($)", labels = scales::comma)  +
          scale_fill_discrete(name="Year",breaks=levels(x$Year),
                              labels=sapply(1:length(levels(x$Year)),
                                            function(i) unlist(str_split(string=levels(x$Year)[[i]],pattern="FY|X|\\.."))[2]))
      }
    }
    if(input$Explan=="Subject" && input$Breakdown==FALSE){
      if(length(which(is.na(x$Subject))) > 0){x<-x[-which(is.na(x$Subject)),]}
      p <- ggplot(data=x,aes(x=SubjectArea,y=Value))+
        geom_bar(stat = "identity", position = "stack")+
        theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
        scale_y_continuous(name="Funding ($)", labels = scales::comma)
    }
    
    if(input$Explan=="Species" && input$Breakdown==FALSE){
      
      if(length(which(is.na(x$Species))) > 0){x<-x[-which(is.na(x$Species)),]}
      p <- ggplot(data=x,aes(x=Species,y=Value))+
        geom_bar(stat = "identity", position = "stack")+
        theme(axis.text.x = element_text(angle=60, hjust=1),text = element_text(size=20))+
        scale_y_continuous(name="Funding ($)", labels = scales::comma)
    }
    }
    print(p)
    p$panel$ranges[[1]]$y.major_source
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

