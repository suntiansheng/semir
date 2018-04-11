library(shiny)
library(stringr)
library(ggplot2)
#library(recharts)
library(jiebaR)
#setwd("D:/workspace/BI报表")


######

ui <- navbarPage('bi',
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
                            tableOutput("contents")
                            
                          )
                          
                 ),
                 tabPanel('density',
                          
                          sidebarPanel(
                          #selectInput('category', 'category',c('短袖' = '短袖','长袖' = '长袖')),
                          textInput('category', 'category', '短袖'),
                          submitButton("Submit")
                          ),
                          
                          mainPanel(output_echart('plot1',height = "800px"))
                 
                 )       
)

server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                     header = input$header)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  

  
  ntext <- eventReactive(input$goButton, {
    input$plot
  })
  
  output$plot1 <- render_echart({
    req(input$file1)
     df <- read.csv(input$file1$datapath,
                     header = input$header)
      mydata$详情页跳出率 <- mydata$详情页跳出率 %>% str_remove('%') %>% as.numeric()
      mydata$下单转化率 <- mydata$下单转化率 %>% str_remove('%') %>% as.numeric()
      mydata$详情页跳出率 <- mydata$详情页跳出率 %>% str_remove('%') %>% as.numeric()
      mydata$支付转化率 <- mydata$支付转化率 %>% str_remove('%') %>% as.numeric()
      mydata$点击转化率 <- mydata$点击转化率 %>% str_remove('%') %>% as.numeric()
      mydata$下单支付转化率 <- mydata$下单支付转化率 %>% str_remove('%') %>% as.numeric()
      mydata$搜索支付转化率 <- mydata$搜索支付转化率 %>% str_remove('%') %>% as.numeric()
   
      d1 <- mydata[mydata$商品标题 %>% str_detect(input$category),]
      m1 <- table(d1$浏览量)
      echartr(d1[order(d1$支付买家数),],x = 客单价 ,y = 浏览量, t = 支付买家数, type = 'scatter')
      
  }
  )
  
  
}

# Create Shiny app ----
shinyApp(ui, server)