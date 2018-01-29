library(shiny)
library(markdown)
library(DT)
library(jiebaR)
library(wordcloud)
ui <- navbarPage('mypage',
          tabPanel('input data',
                   sidebarPanel(
                     
                     # Input: Select a file ----
                     fileInput("file1", "Choose TXT File",
                               multiple = TRUE),
                     
                     # Horizontal line ----
                     tags$hr(),
                     
                     # Input: Checkbox if file has header ----
                     checkboxInput("header", "Header", TRUE),
                     
                     # Horizontal line ----
                     tags$hr(),
                     
                     # Input: Select number of rows to display ----
                     radioButtons("disp", "Display",
                                  choices = c(Head = "head",
                                              All = "all"),
                                  selected = "head"),
                     tags$hr(),
                     h6(helpText('Author:Sun Ao')),
                     h6(helpText('contact:654225305@qq.com'))
                   ),
                   
                   # Main panel for displaying outputs ----
                   mainPanel(
                     
                     # Output: Data file ----
                     tableOutput("contents")
                     
                   )
                   
          ),
          tabPanel('table',
                 mainPanel(dataTableOutput('datable'))
          ),
          tabPanel('wordcloud',
                  mainPanel(plotOutput('plot1'))
          )       
          )

server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.table(input$file1$datapath,
                     header = input$header)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
    #######fenci
    #stopword <- read.table('https://raw.githubusercontent.com/suntiansheng/textminingApp/master/stopwords.txt',encoding = 'UTF-8',sep = '\n',quote = NULL)
    engine <- worker()
    mycom <- deparse(df)
    fenci <- segment(mycom,engine,mod = 'hmm')
    tt<- table(fenci)
    tt <- as.data.frame(tt)
    tt20 <- tt[order(tt$Freq,decreasing = T),][1:20,]
    
    
  
    output$datable <- renderDataTable({
      req(input$file1)
  
      datatable(tt20)})
    
    #wordcloud
    output$plot1 <- renderPlot({
      req(input$file1)
      wordcloud(tt20$fenci,tt20$Freq)
    })
    
  
  
}

# Create Shiny app ----
shinyApp(ui, server)
