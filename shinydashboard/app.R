## app.R ##
library(shinydashboard)
library(readr)
library(dplyr)
library(plotly)
library(crosstalk)
library(DT)
library(highcharter)
options(scipen=999)

source("module_functions.R")

ginicities = read_csv("data/ginicities_5.csv")
pagamentos_siconv = read_csv("data/pagamentos_sincov.csv")
cidade_resumo = read_csv("data/cidade_resumo_5.csv")
cidade_convenio = read_csv("data/cidade_convenio_5.csv")


ui <- dashboardPage( skin= "purple",
  dashboardHeader(title = "As diferentonas - As Supercontratadas", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(id = "myMenu",
                menuItem("Introdução", tabName = "info", icon = icon("magic")),
                menuItem("Sua cidade", tabName = "user", icon = icon("bar-chart-o")),
                menuItem("Sobre", tabName = "sobre", icon = icon("info-circle"))),
    conditionalPanel(condition = "input.myMenu == 'user' && input.panel2 == 1",
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
                    infoBox(width = 4, "Abaíra", "286.5911%", icon = icon("percent"), color = "teal"),
                    infoBox(width = 4, "Maiquinique", "73.60089%", icon = icon("percent"), color = "teal"),
                    infoBox(width = 4, "Planaltino", "211.9592%", icon = icon("percent"), color = "teal")
                    ),
                  fluidRow(
                    box(width = 12,
                        h3("Cidade Semelhantes"),
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
                    box(highchartOutput("plotConveniosTut1")),
                    box(highchartOutput("plotFornecedoresTut1"))
                  ),
                  fluidRow(
                    box(highchartOutput("plotConveniosTut2")),
                    box(highchartOutput("plotFornecedoresTut2"))
                  ),
                  fluidRow(
                    box(highchartOutput("plotConveniosTut3")),
                    box(highchartOutput("plotFornecedoresTut3"))
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
                  condition = "input.panel2 == 1",
                  fluidRow(
                    infoBoxOutput("giniBox1"),
                    infoBoxOutput("giniBox2"),
                    infoBoxOutput("giniBox3")
                  ),
                  fluidRow(
                    box(width = 12,
                        h3("Cidade Semelhantes"),
                        DT::dataTableOutput("cidadesSemelhantes2")
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.panel2 == 2",
                  fluidRow(
                    box(highchartOutput("plotConvenios11")),
                    box(highchartOutput("plotFornecedores11"))
                  ),
                  fluidRow(
                    box(highchartOutput("plotConvenios12")),
                    box(highchartOutput("plotFornecedores12"))
                  ),
                  fluidRow(
                    box(highchartOutput("plotConvenios13")),
                    box(highchartOutput("plotFornecedores13"))
                  )
                ),
                conditionalPanel(
                  condition = "input.panel2 == 3",
                  fluidRow( column(width = 12,
                                   DT::dataTableOutput("plotTabela2")
                  ))
                )
              )
      ),
      tabItem(tabName = "sobre")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    
    updateSelectInput(session, "selectCidade",
                      choices = ginicities %>%
                        filter(UF == input$selectEstado) %>%
                        select(cidade))
    

    
  })
  

  
  #######################################################
  
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
    
    names(tabela_resultante) = c("Cód. IBGE", "Município", "Estado", "Nº de fornecedores",
                                 "Nº de convênios", "Total arrecadado", "IDH", "IDH - Renda",
                                 "IDH - Longevidade", "IDH - Educação", "População")
    
    plot_tabela = DT::datatable(tabela_resultante, 
                                filter = "top", 
                                options = list(paging = FALSE))
  })
  
  output$plotConveniosTut1 <- renderHighchart({
    highcharter_convenios(pagamentos_siconv, "ABAIRA", "BA")
  })
  
  output$plotFornecedoresTut1 <- renderHighchart({
    highcharter_fornecedores(pagamentos_siconv, "ABAIRA", "BA")
  })
  
  output$plotConveniosTut2 <- renderHighchart({
    highcharter_convenios(pagamentos_siconv, "MAIQUINIQUE", "BA")
  })
  
  output$plotFornecedoresTut2 <- renderHighchart({
    highcharter_fornecedores(pagamentos_siconv, "MAIQUINIQUE", "BA")
  })
  
  output$plotConveniosTut3 <- renderHighchart({
    highcharter_convenios(pagamentos_siconv, "PLANALTINO", "BA")
  })
  
  output$plotFornecedoresTut3 <- renderHighchart({
    highcharter_fornecedores(pagamentos_siconv, "PLANALTINO", "BA")
  })
  
  output$plotTabelaTut <- DT::renderDataTable({
    tabela_detalhes(pagamentos_siconv, c("ABAIRA", "MAIQUINIQUE", "PLANALTINO"), "BA", cidade_convenio)
  })
  
##################################################
  
  output$cidadesSemelhantes2 <- DT::renderDataTable({
    
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
    
    tabela_resultante = cidade_resumo %>%
      filter(MUNIC_PROPONENTE %in% cidades_semelhantes,
             UF_PROPONENTE == input$selectEstado)
    
    output$giniBox1 <- renderInfoBox({
      munic = cidades_semelhantes[1]
      infoBox(width = 4, 
              munic, 
              paste((ginicities %>% filter(cidade == munic))$coef * 100, "%" ), 
              icon = icon("percent"),
              color = "teal")
    })
    
    
    output$giniBox2 <- renderInfoBox({
      munic = cidades_semelhantes[2]
      infoBox(width = 4, 
            munic, 
            paste((ginicities %>% filter(cidade == munic))$coef * 100, "%" ), 
            icon = icon("percent"),
            color = "teal")
    })
  
    output$giniBox3 <- renderInfoBox({
      munic = cidades_semelhantes[3]
      infoBox(width = 4, 
            munic, 
            paste((ginicities %>% filter(cidade == munic))$coef * 100, "%" ), 
            icon = icon("percent"),
            color = "teal")
    })
    
    output$plotConvenios11 <- renderHighchart({
      highcharter_convenios(pagamentos_siconv, cidades_semelhantes[1], input$selectEstado)
    })
    
    output$plotConvenios12 <- renderHighchart({
      highcharter_convenios(pagamentos_siconv, cidades_semelhantes[2], input$selectEstado)
    })
    
    output$plotConvenios13 <- renderHighchart({
      highcharter_convenios(pagamentos_siconv, cidades_semelhantes[3], input$selectEstado)
    })
    
    output$plotFornecedores11 <- renderHighchart({
      highcharter_fornecedores(pagamentos_siconv, cidades_semelhantes[1], input$selectEstado)
    })
    
    output$plotFornecedores12 <- renderHighchart({
      highcharter_fornecedores(pagamentos_siconv, cidades_semelhantes[2], input$selectEstado)
    })
    
    output$plotFornecedores13 <- renderHighchart({
      highcharter_fornecedores(pagamentos_siconv, cidades_semelhantes[3], input$selectEstado)
    })
    
    output$plotTabela2 <- DT::renderDataTable({
      tabela_detalhes(pagamentos_siconv, cidades_semelhantes, input$selectEstado, cidade_convenio)
    })
    
    names(tabela_resultante) = c("Cód. IBGE", "Município", "Estado", "Nº de fornecedores",
                                 "Nº de convênios", "Total arrecadado", "IDH", "IDH - Renda",
                                 "IDH - Longevidade", "IDH - Educação", "População")
    
    plot_tabela = DT::datatable(tabela_resultante, 
                                filter = "top", 
                                options = list(paging = FALSE))
  })

  
##########################################################################
  
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
        box_navegador(0, "<b>O cenário</b>", "red", "button_box_00", "compass", 3)
  })
  
  output$box_01 <- renderValueBox({
    box_navegador(1, "<b>A cidade</b>", "yellow", "button_box_01", "globe", 3)
  })
  
  output$box_02 <- renderValueBox({
    box_navegador(2, "<b>Os fornecedores</b>", "yellow", "button_box_02", "group", 3)
  })
  
  output$box_03 <- renderValueBox({
    box_navegador(3, "<b>Mais detalhes</b>", "yellow", "button_box_03", "table", 3)
  })
  
  output$box_11 <- renderValueBox({
    box_navegador(1, "<b>A cidade</b>", "yellow", "button_box_11", "globe", 4)
  })
  
  output$box_12 <- renderValueBox({
    box_navegador(2, "<b>Os fornecedores</b>", "yellow", "button_box_12", "group", 4)
  })
  
  output$box_13 <- renderValueBox({
    box_navegador(3, "<b>Mais detalhes</b>", "yellow", "button_box_13", "group", 4)
  })
  
}

shinyApp(ui, server)