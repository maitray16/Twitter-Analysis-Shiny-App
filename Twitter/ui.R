library(shiny)

fluidPage(
  titlePanel("Twitter Archive Analysis"),
  sidebarLayout(
    sidebarPanel(
      
      h4("Analyse your twitter archive by uploading your tweets CSV file."),
      p("Learn how to download your twitter CSV by following these steps"), 
      a("Click Here", href="https://support.twitter.com/articles/20170160",target="_blank"),
      br(""),   
      
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr()
    ),
    mainPanel(
      plotOutput("contents"), plotOutput("wday"),plotOutput("month"),plotOutput("hashtag"),plotOutput("retweet"),
      plotOutput("reply"),plotOutput("chars"),plotOutput("wordCloud"),plotOutput("sentiment")
     
    )
  )
)