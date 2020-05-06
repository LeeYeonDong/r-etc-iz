

총인구수 <- class001[1] %>% as.data.frame()
총인구수 <- 총인구수 %>% na.omit()
총인구수_경남 <- 총인구수 %>% filter(구분 == "경남")

총인구수$year <- 총인구수$year %>% as.integer()
총인구수_경남$year <- 총인구수_경남$year %>% as.integer()

people <- 총인구수
people_kn <- 총인구수_경남


###001

title001 <- titlePanel("총인구수")
sidebarPanel001 <- sidebarPanel(sliderInput(inputId = "bins",
                                            label = " # of bins",
                                            min = 0,
                                            max = 100,
                                            value = 25))
mainPanel001 <- mainPanel(plotOutput(outputId = "distPlot"))


##

ui001 <- fluidPage(title = title001, 
                   sidebarLayout(sidebarPanel001, mainPanel001))

##

server001 <- function(input, output){
  
  output$distPlot <- renderPlot({
    
    x <- 총인구수$value
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, 
         col = "#FF509F", 
         border = "white",
         xlab = "인구수",
         main = "총인구수")
  })
}

##

shinyApp(ui = ui001, server = server001)






###002

title002 <- titlePanel("총인구수")

selectInput002 <- selectInput(inputId = "dataset",
                              label = "데이터선택",
                              choices = c("people","people_kn"))


selectInput002 <- selectInput(inputId = "dataset",
                              label = "데이터선택",
                              choices = c("people", "people_kn"))

numericInput002 <- numericInput(inputId = "obs",
                                label = "관측값설정",
                                value = 10)



sidebarPanel002 <- sidebarPanel(selectInput002, numericInput002)


mainPanel002 <- mainPanel(verbatimTextOutput("summary"),
                          tableOutput("view"))


##

ui002 <- fluidPage(title = title002, 
                   sidebarLayout(sidebarPanel002, mainPanel002))

##

server002 <- function(input, output){
  
  datasetInput <- reactive({
    switch(input$dataset,
           "people" = people,
           "people_kn" = people_kn)
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}

##

shinyApp(ui = ui002, server = server002)




###003

title003 <- titlePanel("제목+데이터요약")

textInput003 <- textInput(inputId = "caption",
                          label = "제목",
                          value = "데이터 요약")

selectInput003 <- selectInput(inputId = "dataset",
                              label = "데이터선택",
                              choices = c("people", "people_kn"))

numericInput003 <- numericInput(inputId = "obs",
                                label = "관측값설정",
                                value = 10)

sidebarPanel003 <- sidebarPanel(textInput003,selectInput003,numericInput003)


mainPanel003 <- mainPanel(h3(textOutput("caption", container = span)),
                          verbatimTextOutput("summary"),
                          tableOutput("view"))

##

ui003 <- fluidPage(title = title003, 
                   sidebarLayout(sidebarPanel003, mainPanel003))

##


server003 <- function(input, output){
  
  datasetInput <- reactive({
    switch(input$dataset,
           "people" = people,
           "people_kn" = people_kn)
  })
  
  output$caption <- renderText({
    input$caption
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    dataset <- datasetInput()
    head(datasetInput(), n = input$obs)
  })
}


shinyApp(ui = ui003, server = server003)
