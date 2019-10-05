library(shiny)
shinyUI(fluidPage(
  titlePanel("Body Fat Calculator"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("meas", "Choose Your Measurement:",
                   list("Metric" = "met",
                        "English" = "eng")),
      conditionalPanel(condition="(input.weight==''||input.abdomen==''||input.wrist=='')&&input.mdat==false",helpText("Please Input Your Data", style = "color:red")),
      conditionalPanel(condition="input.mdat==false",
                       fluidRow(column(8,textInput("abdomen","Abdomen:",value = "")),
                                column(4,conditionalPanel(condition="input.meas=='met'",h4(HTML("<br/>cm")))),
                                column(4,conditionalPanel(condition="input.meas=='eng'",h4(HTML("<br/>inch")))))),
      conditionalPanel(condition="input.mdat==false",
                       fluidRow(column(8,textInput("wrist","Wrist:",value = "")),
                                column(4,conditionalPanel(condition="input.meas=='met'",h4(HTML("<br/>cm")))),
                                column(4,conditionalPanel(condition="input.meas=='eng'",h4(HTML("<br/>inch")))))),
      conditionalPanel(condition="input.mdat==false",actionButton("submit", "Submit")),
      tags$hr(),
      checkboxInput("mdat","Load Massive Data?",FALSE),
      conditionalPanel(condition="input.mdat==true",
                       helpText("Attention: You must upload a CSV file with at least two variables (ABDOMEN and WRIST) 
                                and show their names on the header. The measurement of varaibles must comply with your choice.", style = "color:red")),
      conditionalPanel(condition="input.mdat==true",
                      fileInput('file1', 'Choose CSV File',
                                accept=c('text/csv', 'text/comma-separated-values,text/plain'))),
      conditionalPanel(condition="input.mdat==true",tags$hr()),
      conditionalPanel(condition="input.mdat==true",downloadButton('downloadData', 'Download'))
      ),
  mainPanel(
    h2(textOutput("text1")),
    tableOutput('contents'),
    fluidRow(class="myrow1",column(5,div(tableOutput("table1"),style = "font-size:150%")),
             column(7,imageOutput("image"))),
    fluidRow(plotOutput("plot"))
    ),
  position = "left",fluid = TRUE)
  )
)