library(shiny)
library(ggplot2)
#setwd("D:/UW-Madison/628/Module2/Body Fat Calculator")
dat<-read.csv("./data/BodyFat_AfterCleanUp.csv",header=T)
lm1<-lm(BODYFAT~ABDOMEN+WRIST,data=dat)
shinyServer(function(input,output){
  meas <- reactive({
    switch(input$meas,met=c(1,1),eng=c(2.54,2.54))
    })
  bodyfat<-eventReactive(input$submit,{lm1$coefficients[1]+as.numeric(input$abdomen)*lm1$coefficients[2]*meas()[1]+as.numeric(input$wrist)*lm1$coefficients[3]*meas()[2]})
  result<-eventReactive(input$submit,{"Result"})
  img<-reactive({
    if(bodyfat()<=0)
      return(0)
    else if(bodyfat()<=7)
      return(1)
    else if(bodyfat()<=10)
      return(2)
    else if(bodyfat()<=14.5)
      return(3)
    else if(bodyfat()<=17.5)
      return(4)
    else if(bodyfat()<=22.5)
      return(5)
    else if(bodyfat()<=27.5)
      return(6)
    else if(bodyfat()<=32.5)
      return(7)
    else
      return(8)
  })
  output$text1 <-renderText({result()})
  
  observeEvent(input$submit,{
    output$contents<-renderTable({NULL})
    output$table1<-renderTable({
      if(bodyfat()<=0)
        cat<-"OOF! Something Goes Wrong!"
      else if(bodyfat()<=6)
        cat<-"Essential"
      else if(bodyfat()<=14)
        cat<-"Athletes"
      else if(bodyfat()<=18)
        cat<-"Fitness"
      else if(bodyfat()<=25)
        cat<-"Average"
      else
        cat<-"Obese"
      bodyfat_table<-data.frame(c(round(bodyfat(),3),cat))
      rownames(bodyfat_table)<-c("Body Fat %","Category")
      return(bodyfat_table)
    },rownames=T,colnames=F)
    output$plot<-renderPlot({
      ggplot(dat,aes(x=BODYFAT))+geom_density()+geom_vline(aes(xintercept=bodyfat()),color="red", size=2)+
      annotate("text", x = c(3,10,16,21.5,35), y=0.02, label = c("Essential", "Athletes", "Fitness", "Average","Obese"))+
      geom_rect(data=data.frame(xmin = c(0,6,14,18,25), 
                                xmax = c(6,14,18,25,45), 
                                ymin = -Inf, 
                                ymax = Inf, 
                                col =letters[1:5]),
                inherit.aes=F,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill= col), show.legend = FALSE)+
      scale_fill_manual('Region',values = adjustcolor(c("red","springgreen","green4","gold","red3"),alpha.f = 0.3), 
                        labels=c("Essential", "Athletes", "Fitness", "Average","Obese"))+
      guides(fill = guide_legend(override.aes = list(alpha = 0.3)))+
      coord_fixed(ratio=200)+theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
      })
    output$image<-renderImage({
      list(src = paste("./image",img(),".jpg",sep=""),
           contentType = 'image/png',
           width=406.2,
           height=251.6,
           alt = "OOF! The Pic Seems Broken!")
    },deleteFile = FALSE)
  })
  fname<-reactive({
    if (!is.null(input$file1$datapath))
      return(sub(".csv$", "", basename(input$file1$name)))
  })
  table2<-reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    table2<-read.csv(inFile$datapath, header=T)
    BODY_FAT<-lm1$coefficients[1]+as.numeric(input$abdomen)*lm1$coefficients[2]*meas()[1]+as.numeric(input$wrist)*lm1$coefficients[3]**meas()[2]
    table2<-cbind(BODY_FAT,table2)
    return(table2)})
  output$downloadData <- downloadHandler(
    filename = function() { paste(fname(),'_Body_Fat.csv', sep='') },
    content = function(file) {
      write.csv(table2(), file)
    })
  observeEvent(input$file1,{
    output$table1<-renderTable({NULL})
    output$plot<-renderPlot({NULL})
    output$contents <- renderTable({
      table2()
      })
    output$image<-NULL
    })
})