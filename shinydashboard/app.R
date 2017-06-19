## app.R ##
library(shinydashboard)
library(readr)
library(dplyr)
library(plotly)
library(crosstalk)
library(DT)


ginicities = read_csv("data/ginicities_5.csv")
pagamentos_siconv = read_csv("data/pagamentos_sincov.csv")
cidade_resumo = read_csv("data/cidade_resumo_5.csv")

ui <- dashboardPage( skin= "purple",
  dashboardHeader(title = "As diferentonas - As Supercontratadas", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(id = "myMenu",
                menuItem("Introdução", tabName = "info", icon = icon("info-circle")),
                menuItem("Sua cidade", tabName = "user", icon = icon("bar-chart-o"))),
    conditionalPanel(condition = "input.myMenu == 'user'",
                     selectInput("selectEstado", "Estado:", choices = unique(ginicities %>%
                                                                               select(UF))),
                     selectInput("selectCidade", "Cidade:", choices = ginicities %>%
                                                                        filter(UF == "PB") %>%
                                                                        select(cidade)))
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
                  fluidRow(
                    column(width = 4,  style='padding: 20px; padding-left:35px;', imageOutput("intro1")),
                    column(width = 4, style='padding:20px;',imageOutput("intro2")),
                    column(width = 4, style='padding:20px;',imageOutput("intro3"))
                  ),
                  fluidRow(
                    column(width = 4, style='padding: 20px; padding-left:35px;', imageOutput("intro4")),
                    column(width = 4, style='padding:20px;', imageOutput("intro5")),
                    column(width = 4, style='padding:20px;', imageOutput("intro6"))
                  )
                ),
                conditionalPanel(
                  condition = "input.panel == 1",
                  fluidRow(
                    box(
                      width = 12, background = "purple", collapsible = TRUE,
                      p("Alguns municípios brasileiros adoram supercontratar. Mesmo
                      desconsiderando as grandes obras no município, algumas empresas permancecem
                      sendo as 'favoritas' em contratação."),
                      p("Selecionamos o município Abaíra, localizado
                      na Bahia, para exemplificar. Podemos ver que além deste município, há mais outros dois.
                      Os munícipios adicionais são aqueles semelhantes a Abaíra com base na população. Dessa
                      maneira podemos compará-lo com seus semelhantes para observar alguma comportamento
                      que se destaque.")
                      )
                    ),
                  fluidRow(
                    infoBox(width = 4, "Abaíra", "XX.xx%", icon = icon("percent"), color = "teal"),
                    infoBox(width = 4, "Maiquinique", "XX.xx%", icon = icon("percent"), color = "teal"),
                    infoBox(width = 4, "Planaltino", "XX.xx%", icon = icon("percent"), color = "teal")
                    ),
                  fluidRow(
                    box(width = 12,
                             #      h3("Cidade Semelhantes"),
                           DT::dataTableOutput("cidadesSemelhantes")
                                   )
                  )
                ),
                conditionalPanel(
                  condition = "input.panel == 2",
                  fluidRow(
                    box(
                      width = 12, background = "purple", collapsible = TRUE,
                      p("Há duas perperctivas para observar os covênios e empresas contratadas
                        por um município."),
                      p("No primeiro gráfico cada barra representa um convênio e
                        a quantidade de verba destinada ao mesmo. As diferentes cores em cada barra
                        são as empresas que foram contraradas para o convênio. No segundo gráfico cada
                        barra representa uma empresa e a quantidade de verba que adiquiriu ao ser
                        contratada para os convênios. As diferentes cores em cada barra são os
                        convênios aos quais a empresa forneceu algum serviço."),
                      p("Há uma empresa específica no município de Abaíra que se destaca com concentrar
                        muitos convênios e acumular uma grande quantidade de verba. Esse é um comportamento
                        diferenciado se observarmos as outras cidades semelhantes onde a verba e contratações
                        estão melhores distribuídas.")
                      )
                    ),
                  fluidRow(
                    box(plotlyOutput("plotConveniosTut1")),
                    box(plotlyOutput("plotFornecedoresTut1"))
                  )
                ),
                conditionalPanel(
                  condition = "input.panel == 3",
                  fluidRow(
                    box(
                      width = 12, background = "purple", collapsible = TRUE,
                      p("Pode-se investigar um pouco mais sobre esse cenário pesquisando na bela abaixo.
                        Se digitarmos o número da empresa podemos filtrar os dados e verificar
                        detalhes como nome, pagamentos e finalidades.")                      )
                      ),
                  fluidRow( column(width = 12,
                    DT::dataTableOutput("plotTabelaTut")
                  ))
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
                ),
                conditionalPanel(
                  condition = "input.panel2 == 2",
                  fluidRow(
                    box(plotlyOutput("plotConvenios1")),
                    box(plotlyOutput("plotFornecedores1"))
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  output$cidadesSemelhantes <- DT::renderDataTable({
    
    cidadesEstado = cidade_resumo %>%
      filter(UF_PROPONENTE == "BA")
    
    cidadesEstado$dist = dist(rbind(cidadesEstado %>%
                                      filter(MUNIC_PROPONENTE == "ABAIRA") %>%
                                      select(pop),
                                    cidadesEstado %>%
                                      select(pop)))[1:NROW(cidadesEstado)]
    
    cidades_semelhantes = (cidadesEstado %>%
                             arrange(dist) %>%
                             slice(1:3))$MUNIC_PROPONENTE
    
    tabela_resultante = cidade_resumo %>%
      filter(MUNIC_PROPONENTE %in% cidades_semelhantes,
             UF_PROPONENTE == "BA")
    
    plot_tabela = DT::datatable(tabela_resultante, 
                                filter = "top", 
                                options = list(paging = FALSE))
  })
  
  
  output$plotConveniosTut1 <- renderPlotly({
    plot_convenios = plot_ly(pagamentos_siconv %>%
                               filter(MUNIC_PROPONENTE == "ABAIRA",
                                      UF_PROPONENTE == "BA"),
                             x = ~as.factor(NR_CONVENIO),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(IDENTIF_FORNECEDOR),
                             colors = "RdPu",
                             text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Convênio - ABAIRA"),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Convênios"))
  })
  
  output$plotFornecedoresTut1 <- renderPlotly({
    
    plot_convenios = plot_ly(pagamentos_siconv %>%
                               filter(MUNIC_PROPONENTE == "ABAIRA",
                                      UF_PROPONENTE == "BA"),
                             x = ~as.factor(IDENTIF_FORNECEDOR),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(NR_CONVENIO),
                             colors = "YlGnBu",
                             text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Fornecedor - ABAIRA"),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Fornecedores"))
  })
  
  
  output$plotTabelaTut <- DT::renderDataTable({
    
    tabela_resultante = pagamentos_siconv %>%
      filter(MUNIC_PROPONENTE %in% c("ABAIRA", "MAIQUINIQUE", "PLANALTINO"),
             UF_PROPONENTE == "BA") %>%
      select(MUNIC_PROPONENTE,
             UF_PROPONENTE,
             IDENTIF_FORNECEDOR,
             NOME_FORNECEDOR,
             NR_CONVENIO,
             OBJETO_PROPOSTA,
             VL_PAGO) %>%
      group_by(MUNIC_PROPONENTE,
               UF_PROPONENTE,
               NR_CONVENIO,
               IDENTIF_FORNECEDOR,
               NOME_FORNECEDOR,
               OBJETO_PROPOSTA) %>%
      summarise(total_pago = sum(VL_PAGO))
    
    plot_tabela = DT::datatable(tabela_resultante, 
                                filter = "top", 
                                options = list(paging = FALSE))
  })
  
  
  
  
  
  
  
  shared_cidade_resumo = SharedData$new(cidade_resumo)
  
  output$plotConvenios1 <- renderPlotly({
    cidadesEstado = cidade_resumo %>%
      filter(UF_PROPONENTE == input$selectEstado)
    
    cidadesEstado$dist = dist(rbind(cidadesEstado %>%
                                      filter(MUNIC_PROPONENTE == input$selectCidade) %>%
                                      select(pop),
                                    cidadesEstado %>%
                                      select(pop)))[1:NROW(cidadesEstado)]
    
    cidades_semelhantes = (cidadesEstado %>%
                             arrange(dist) %>%
                             slice(1:3))$MUNIC_PROPONENTE
    
    
    plot_convenios = plot_ly(pagamentos_siconv %>%
                               filter(MUNIC_PROPONENTE == cidades_semelhantes[1],
                                      UF_PROPONENTE == input$selectEstado),
                             x = ~as.factor(NR_CONVENIO),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(IDENTIF_FORNECEDOR),
                         #    colors = "RdPu",
                             text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Convênio - ", cidades_semelhantes[1]),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Convênios"))
    
  })
  

  
  output$plotFornecedores1 <- renderPlotly({
    
    cidadesEstado = cidade_resumo %>%
      filter(UF_PROPONENTE == input$selectEstado)
    
    cidadesEstado$dist = dist(rbind(cidadesEstado %>%
                                      filter(MUNIC_PROPONENTE == input$selectCidade) %>%
                                      select(pop),
                                    cidadesEstado %>%
                                      select(pop)))[1:NROW(cidadesEstado)]
    
    cidades_semelhantes = (cidadesEstado %>%
                             arrange(dist) %>%
                             slice(1:3))$MUNIC_PROPONENTE
    
    plot_convenios = plot_ly(pagamentos_siconv %>%
                               filter(MUNIC_PROPONENTE == cidades_semelhantes[1],
                                      UF_PROPONENTE == input$selectEstado),
                             x = ~as.factor(IDENTIF_FORNECEDOR),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(NR_CONVENIO),
                       #      colors = "YlGnBu",
                             text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Fornecedor - ", cidades_semelhantes[1]),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Fornecedores"))
  })
  
  
  
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
  
  output$intro1 <- renderImage({
    return(list(
      src = "data/1.png",
      contentType = "image/png",
      height = "100%",
      alt = "diagrama"
    ))
  }, deleteFile = F)
  
  output$intro2 <- renderImage({
    return(list(
      src = "data/2.png",
      contentType = "image/png",
      height = "100%",
      alt = "diagrama"
    ))
  }, deleteFile = F)
  
  output$intro3 <- renderImage({
    return(list(
      src = "data/3.png",
      contentType = "image/png",
      height = "100%",
      
      alt = "diagrama"
    ))
  }, deleteFile = F)
  
  output$intro4 <- renderImage({
    return(list(
      src = "data/4.png",
      contentType = "image/png",
      height = "100%",
      
      alt = "diagrama"
    ))
  }, deleteFile = F)
  
  output$intro5 <- renderImage({
    return(list(
      src = "data/5.png",
      contentType = "image/png",
      height = "100%",
      
      alt = "diagrama"
    ))
  }, deleteFile = F)
  
  output$intro6 <- renderImage({
    return(list(
      src = "data/6.png",
      contentType = "image/png",
      height = "100%",
      
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
    box0<-valueBox(value=0
                   ,icon = icon("compass")
                   ,width=3
                   ,color = "red"
                   ,href="#"
                   ,subtitle=HTML("<b>O cenário</b>")
    )
      
    box0$children[[1]]$attribs$class<-"action-button"
    box0$children[[1]]$attribs$id<-"button_box_00"
    return(box0)
  })
  
  output$box_01 <- renderValueBox({
    box1<-valueBox(value=1
                   ,icon = icon("globe")
                   ,width=3
                   ,color = "yellow"
                   ,href="#"
                   ,subtitle=HTML("<b>A cidade</b>")
    )
    
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_box_01"
    return(box1)
  })
  
  output$box_02 <- renderValueBox({
    box2<-valueBox(value=2
                   ,icon = icon("group")
                   ,width=3
                   ,color = "yellow"
                   ,href="#"
                   ,subtitle=HTML("<b>Os fornecedores</b>")
    )
    
    box2$children[[1]]$attribs$class<-"action-button"
    box2$children[[1]]$attribs$id<-"button_box_02"
    return(box2)
  })
  
  output$box_03 <- renderValueBox({
    box3<-valueBox(value=3
                   ,icon = icon("table")
                   ,width=3
                   ,color = "yellow"
                   ,href="#"
                   ,subtitle=HTML("<b>Mais detalhes</b>")
    )
    
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
                   ,subtitle=HTML("<b>Os fornecedores</b>")
    )
    
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
    
    box13$children[[1]]$attribs$class<-"action-button"
    box13$children[[1]]$attribs$id<-"button_box_13"
    return(box13)
  })
}

shinyApp(ui, server)