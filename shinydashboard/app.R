## app.R ##
library(shinydashboard)

ui <- dashboardPage( skin= "purple",
  dashboardHeader(title = "Diferentonas - Os Superfornecedores", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introdução", tabName = "info", icon = icon("info-circle")),
      menuItem("Sua cidade", tabName = "user", icon = icon("bar-chart-o"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "info",
              fluidRow(
                valueBoxOutput("box_00", width = 3), 
                valueBoxOutput("box_01", width = 3),
                valueBoxOutput("box_02", width = 3),
                valueBoxOutput("box_03", width = 3)
              ),
              fluidRow(
                
                conditionalPanel(
                  condition = "input.panel == 4",
                  selectInput(inputId ="panel", label = "panel", selected = 0, choices = c(0,1,2,3))
                  ),
                conditionalPanel(
                  condition = "input.panel == 0",
                  imageOutput("intro")
                ),
                conditionalPanel(
                  condition = "input.panel == 1",
                  box(plotOutput("plot1", height = 250)),
                  box(
                    title = "Controls",
                    sliderInput("slider", "Number of observations:", 1, 100, 50)
                  )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "user",
              fluidRow(
                valueBoxOutput("box_11", width = 4),
                valueBoxOutput("box_12", width = 4),
                valueBoxOutput("box_13", width = 4)
              ),
              fluidRow(
                
                conditionalPanel(
                  condition = "input.panel2 == 4",
                  selectInput(inputId ="panel2", label = "panel2", selected = 1, choices = c(1,2,3))
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$button_box_00, {
    observe({
      updateSelectInput(session, "panel",
                        selected = 0)
    })
  })
  
  observeEvent(input$button_box_01, {
    observe({
      updateSelectInput(session, "panel",
                        selected = 1)
    })
  })
  
  observeEvent(input$button_box_02, {
    observe({
      updateSelectInput(session, "panel",
                        selected = 2)
    })
  })
  
  observeEvent(input$button_box_03, {
    observe({
      updateSelectInput(session, "panel",
                        selected = 3)
    })
  })
  
  observeEvent(input$button_box_11, {
    observe({
      updateSelectInput(session, "panel2",
                        selected = 1)
    })
  })
  
  observeEvent(input$button_box_12, {
    observe({
      updateSelectInput(session, "panel2",
                        selected = 2)
    })
  })
  
  observeEvent(input$button_box_13, {
    observe({
      updateSelectInput(session, "panel2",
                        selected = 3)
    })
  })
  
  output$intro <- renderImage({
    return(list(
      src = "data/my_design.png",
      contentType = "image/png",
      width = 400,
      alt = "diagrama"
    ))
  }, deleteFile = F)
  

  output$plot1 <- renderPlot({
    
    v <- reactiveValues(data = NULL)
    
    observeEvent(input$button_box_00, {
      v$data <- runif(100)
      
    })
    
    observeEvent(input$button_box_01,{
      v$data <- rnorm(100)
    })
    
    output$plot1 <- renderPlot({
      if (is.null(v$data)) v$data <- rnorm(100)
      hist(v$data)
    })
  })
  

  output$box_00 <- renderValueBox({
    entry_00<-0
    box0<-valueBox(value=entry_00
                   ,icon = icon("compass")
                   ,width=3
                   ,color = "red"
                   ,href="#"
                   ,subtitle=HTML("<b>Cenário</b>")
    )
      
    box0$children[[1]]$attribs$class<-"action-button"
    box0$children[[1]]$attribs$id<-"button_box_00"
    return(box0)
  })
  
  output$box_01 <- renderValueBox({
    entry_01<-1
    box1<-valueBox(value=entry_01
                   ,icon = icon("globe")
                   ,width=3
                   ,color = "yellow"
                   ,href="#"
                   ,subtitle=HTML("<b>A cidade</b>")
    )
    
  #  valueBox(value = entry_01, subtitle = "Cenário", icon = icon("compass"), color = "yellow", width = 3)
    
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_box_01"
    return(box1)
  })
  
  output$box_02 <- renderValueBox({
    entry_02<-2
    box2<-valueBox(value=entry_02
                   ,icon = icon("group")
                   ,width=3
                   ,color = "yellow"
                   ,href="#"
                   ,subtitle=HTML("<b>Os contratados</b>")
    )
    
    #  valueBox(value = entry_01, subtitle = "Cenário", icon = icon("compass"), color = "yellow", width = 3)
    
    box2$children[[1]]$attribs$class<-"action-button"
    box2$children[[1]]$attribs$id<-"button_box_02"
    return(box2)
  })
  
  output$box_03 <- renderValueBox({
    entry_03<-3
    box3<-valueBox(value=entry_03
                   ,icon = icon("table")
                   ,width=3
                   ,color = "yellow"
                   ,href="#"
                   ,subtitle=HTML("<b>Mais detalhes</b>")
    )
    
    #  valueBox(value = entry_01, subtitle = "Cenário", icon = icon("compass"), color = "yellow", width = 3)
    
    box3$children[[1]]$attribs$class<-"action-button"
    box3$children[[1]]$attribs$id<-"button_box_03"
    return(box3)
  })
  
  output$box_11 <- renderValueBox({
    box11<-valueBox(value=1
                   ,icon = icon("globe")
                   ,width=4
                   ,color = "yellow"
                   ,href="#"
                   ,subtitle=HTML("<b>A cidade</b>")
    )
    
    #  valueBox(value = entry_01, subtitle = "Cenário", icon = icon("compass"), color = "yellow", width = 3)
    
    box11$children[[1]]$attribs$class<-"action-button"
    box11$children[[1]]$attribs$id<-"button_box_11"
    return(box11)
  })
  
  output$box_12 <- renderValueBox({
    box12<-valueBox(value=2
                   ,icon = icon("group")
                   ,width=4
                   ,color = "yellow"
                   ,href="#"
                   ,subtitle=HTML("<b>Os contratados</b>")
    )
    
    #  valueBox(value = entry_01, subtitle = "Cenário", icon = icon("compass"), color = "yellow", width = 3)
    
    box12$children[[1]]$attribs$class<-"action-button"
    box12$children[[1]]$attribs$id<-"button_box_12"
    return(box12)
  })
  
  output$box_13 <- renderValueBox({
    box13<-valueBox(value=3
                   ,icon = icon("table")
                   ,width=4
                   ,color = "yellow"
                   ,href="#"
                   ,subtitle=HTML("<b>Mais detalhes</b>")
    )
    
    #  valueBox(value = entry_01, subtitle = "Cenário", icon = icon("compass"), color = "yellow", width = 3)
    
    box13$children[[1]]$attribs$class<-"action-button"
    box13$children[[1]]$attribs$id<-"button_box_13"
    return(box13)
  })
}

shinyApp(ui, server)