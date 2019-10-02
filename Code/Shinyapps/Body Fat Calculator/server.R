library(shiny)
library(ggplot2)
#setwd("D:/UW-Madison/628/Module2/Body Fat Calculator")
dat<-read.csv("./data/BodyFat.csv",header=T)
lm1<-lm(BODYFAT~WEIGHT+ABDOMEN+WRIST,data=dat)
shinyServer(function(input,output){
  meas <- reactive({
    switch(input$meas,met=c(2.2046226218488,1,1),eng=c(1,2.54,2.54))
    })
  bodyfat<-eventReactive(input$submit,{lm1$coefficients[1]+as.numeric(input$weight)*lm1$coefficients[2]*meas()[1]+as.numeric(input$abdomen)*lm1$coefficients[3]*meas()[2]+as.numeric(input$wrist)*lm1$coefficients[4]**meas()[3]})
  result<-eventReactive(input$submit,{"Result"})
  output$text1 <-renderText({result()})
  observeEvent(input$submit,{
    output$contents<-renderTable({NULL})
    output$table1<-renderTable({
      bodyfat_table<-data.frame(bodyfat())
      colnames(bodyfat_table)<-"Percentage"
      rownames(bodyfat_table)<-"Body Fat"
      return(bodyfat_table)
    },rownames=T,colnames=T)
    output$plot<-renderPlot({
      ggplot(dat,aes(x=BODYFAT))+geom_density()+geom_vline(aes(xintercept=bodyfat()),color="red", size=1)+
        annotate("text", x = c(3,10,16,21.5,35), y=0.02, label = c("Essential", "Athletes", "Fitness", "Average","Obese"))+
        geom_rect(data=data.frame(xmin = c(0,6,14,18,25), 
                                  xmax = c(6,14,18,25,45), 
                                  ymin = -Inf, 
                                  ymax = Inf, 
                                  col =letters[1:5]),
                  inherit.aes=F,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill= col), show.legend = FALSE)+
        scale_fill_manual('Region',values = adjustcolor(c("red","springgreen","green4","gold","red3"),alpha.f = 0.4), 
                        labels=c("Essential", "Athletes", "Fitness", "Average","Obese"))+
        guides(fill = guide_legend(override.aes = list(alpha = 0.4)))+
        coord_fixed(ratio=200)
    })
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
    BODY_FAT<-lm1$coefficients[1]+as.numeric(table2$WEIGHT)*lm1$coefficients[2]*meas()[1]+as.numeric(table2$ABDOMEN)*lm1$coefficients[3]*meas()[2]+as.numeric(table2$WRIST)*lm1$coefficients[4]**meas()[3]
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
    })
})