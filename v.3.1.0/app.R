library(shiny)
library(readxl)
library(stringr)
library(writexl)

ui <- fluidPage(
  titlePanel("Program za preračunavanje baz"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose xlsx file',
                accept = c(".xlsx")),
      actionButton("goButton", "Preračunaj!"),
      downloadButton('downloadData', "Download modified file"),
      selectInput("region", "Prenesi:", 
                  choices= c("Preracunave", "Analiza")),
      hr(),
      h6("Računaj in razvrščaj glede na:"),
      checkboxInput("checkbox", label = "spol", value = TRUE),
      fluidRow(column(3, verbatimTextOutput("value")))
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Tabela", tableOutput("contents")),
      tabPanel("Analiza", tableOutput("tmp"))
      )
    )
  )
)

server <- function(input, output){
  
  observeEvent(input$goButton,{
    
    if(input$checkbox){
      source("skriptaSpol.R")
      output$contents <- renderTable({baza1})
      
      output$tmp <- renderTable({
        
        tab <<- data.frame(lapply( baza1 , function(x) rbind( mean = mean(x, na.rm=TRUE) ,
                                                              sd = sd(x, na.rm=TRUE) ,
                                                              median = median(x, na.rm=TRUE) ,
                                                              minimum = min(x, na.rm=TRUE) ,
                                                              maximum = max(x, na.rm=TRUE) ,
                                                              s.size = length(x) 
        ) 
        )
        )
        #summer <- data.frame(t(tab))
      },include.rownames=TRUE)
    } else {
    source("skriptaPre.R")
    output$contents <- renderTable({baza1})

    output$tmp <- renderTable({
           
      tab <<- data.frame(lapply( baza1 , function(x) rbind( mean = mean(x, na.rm=TRUE) ,
                                               sd = sd(x, na.rm=TRUE) ,
                                               median = median(x, na.rm=TRUE) ,
                                               minimum = min(x, na.rm=TRUE) ,
                                               maximum = max(x, na.rm=TRUE) ,
                                               s.size = length(x) 
                                               ) 
                                )
                        )
                                                #summer <- data.frame(t(tab))
                                  },include.rownames=TRUE)
           }
    
    
  })
  
  output$contents <- renderTable({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    orginal <<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("izracun-", ".xlsx", sep = "")
    },
    content = function(file) {
      if (input$region == "Preracunave"){ 
        write_xlsx(baza1, file)}
      else {
        write_xlsx(tab, file, col_names = TRUE)
      }
    }
  )
  
  
}

shinyApp(ui, server)