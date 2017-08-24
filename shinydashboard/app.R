## app.R ##
library(shinydashboard)
library(readr)
library(dplyr)
library(DT)
library(highcharter)
options(scipen=999)

# source("shinydashboard/module_functions.R")
# ginicities = read_csv("shinydashboard/data/ginicities_5_new.csv")
# pagamentos_siconv = read_csv("shinydashboard/data/pagamentos_sincov.csv")
# cidade_resumo = read_csv("shinydashboard/data/cidade_resumo_5.csv")
# cidade_convenio = read_csv("shinydashboard/data/cidade_convenio_5.csv")

source("module_functions.R")

ginicities = read_csv("data/ginicities_5_new.csv")
pagamentos_siconv = read_csv("https://www.dropbox.com/s/hqmigwbnq9zyd0q/pagamentos_sincov.csv?raw=1")
cidade_resumo = read_csv("data/cidade_resumo_5.csv")
cidade_convenio = read_csv("data/cidade_convenio_5.csv")
coef_ref = data_frame("cod7" = c("3127909", "2916401", "4322343"),
                      "cidade" = c( "GRUPIARA", "ITAPETINGA", "UBIRETAMA"),
                      "legenda" = c( "Município com menor coef.", "Mediana de coeficientes", "Município com maior coef."),
                      "UF" = c("MG", "BA", "RS" ),
                      "pop" = c(3127909, 68273, 2296),
                      "coef" = c(0.01709639, 2.036940,19.655514))


ui <- dashboardPage( skin= "purple",
  dashboardHeader(title = "As diferentonas - As Supercontratadas", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(id = "myMenu",
                menuItem("Introdução", tabName = "info", icon = icon("magic")),
                menuItem("Sua cidade", tabName = "user", icon = icon("bar-chart-o")),
                menuItem("Notas", tabName = "nota", icon = icon("bar-chart-o")),
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
                    column(width = 3,
                           fluidRow(style='padding-top:30px;', infoBoxOutput(width = 12, "giniBox01")),
                           fluidRow(style='padding-top:30px;', infoBoxOutput(width = 12, "giniBox02")),
                           fluidRow(style='padding-top:30px;', infoBoxOutput(width = 12, "giniBox03"))
                           ),
                    column(width = 9, fluidRow(box(width = 12, highchartOutput("giniInfoCoef00"))))
                    ),
                  fluidRow(
                    box(width = 12,
                        h3("Municípios semelhantes"),
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
                    
                    column(width = 3,
                           fluidRow(style='padding-top:30px;', infoBoxOutput(width = 12, "giniBox1")),
                           fluidRow(style='padding-top:30px;', infoBoxOutput(width = 12, "giniBox2")),
                           fluidRow(style='padding-top:30px;', infoBoxOutput(width = 12, "giniBox3"))
                    ),
                    column(width = 9, fluidRow(box(width = 12, highchartOutput("giniInfoCoef11"))))
                  ),
                  fluidRow(
                    box(width = 12,
                        h3("Municípios semelhantes"),
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
      tabItem(tabName = "nota",
              box(width = 12, title = "Notas",
                  p(""),
                  p("")
                  )
                ),
      tabItem(tabName = "sobre",
              box(width = 12, title = "Sobre",
                p("Esta aplicação é resultado de um projeto de pesquisa entitulado de Mineração de dados
                  para controle social do Congresso Nacional. A pesquisa foi realizada na Universidade Federal de
                  Campina Grande (UFCG) [1] e Lab Analytics [2] e faz parte do Programa Institucional de Bolsas
                  de Iniciação Científica (PIBIC). As pessoas envolvidas são Martha Michelly Galvão Menezes, aluna de
                  graduação no curso de Ciência da Computação, e Nazareno Ferreira de Andrade, professor do Departamento de
                  Sistemas e Computação e orientador do projeto."),
                p("O projeto foi realizado com objetivo de, através de análise e visualização de dados, auxiliar no
                  na investigação e entendimento de como os municípios estão atuando no processo de contratação de
                  empresas e distribuição de verbas entre os convênios existentes. É possível realizar comparações 
                  municípios de porte semelhantes e observar se há concentração de verba em uma só empresa fornecedora
                  de serviços/produtos. Este pode ser considerado um pontapé inicial para a percepção de anomalias,
                  incentivando a busca de razões/respostas a respeito de comportamentos suspeitos."),
                p("[1] www.ufcg.edu.br"),
                p("[2] http://analytics.ufcg.edu.br")
              )
            )
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
    
    cidadesProximas = cidade_resumo
    
    cidadesProximas$dist = dist(rbind(cidadesProximas %>%
                                      filter(MUNIC_PROPONENTE == "ABAIRA") %>%
                                      select(pop),
                                      cidadesProximas %>%
                                      select(pop)))[1:NROW(cidadesProximas)]
    
    cidades_semelhantes = (cidadesProximas %>%
                             arrange(dist) %>%
                             slice(1:3))$MUNIC_PROPONENTE
    cidades_semelhantes_cod = (cidadesProximas %>%
                             arrange(dist) %>%
                             slice(1:3))$cod7

    output$giniInfoCoef00 <- renderHighchart({
      
      highcharter_coef(ginicities, cidades_semelhantes_cod, coef_ref)
      
    })

    tabela_resultante = cidade_resumo %>%
      filter(MUNIC_PROPONENTE %in% cidades_semelhantes)
  
    output$giniBox01 <- renderInfoBox({
      munic = cidades_semelhantes[1]
      infoBox(width = 4, 
              munic, 
              format(round((ginicities %>% filter(cidade == munic))$coef,3), nsmall = 3), 
              icon = icon("percent"),
              color = "teal")
    })
    
    
    output$giniBox02 <- renderInfoBox({
      munic = cidades_semelhantes[2]
      infoBox(width = 4, 
              munic, 
              format(round((ginicities %>% filter(cidade == munic))$coef,3), nsmall = 3), 
              icon = icon("percent"),
              color = "teal")
    })
    
    output$giniBox03 <- renderInfoBox({
      munic = cidades_semelhantes[3]
      infoBox(width = 4, 
              munic, 
              format(round((ginicities %>% filter(cidade == munic))$coef,3), nsmall = 3), 
              icon = icon("percent"),
              color = "teal")
    })
  output$plotConveniosTut1 <- renderHighchart({
    highcharter_convenios(pagamentos_siconv, cidades_semelhantes[1], "BA")
  })
  
  output$plotFornecedoresTut1 <- renderHighchart({
    highcharter_fornecedores(pagamentos_siconv, cidades_semelhantes[1], "BA")
  })
  
  output$plotConveniosTut2 <- renderHighchart({
    highcharter_convenios(pagamentos_siconv, cidades_semelhantes[2], "BA")
  })
  
  output$plotFornecedoresTut2 <- renderHighchart({
    highcharter_fornecedores(pagamentos_siconv, cidades_semelhantes[2], "BA")
  })
  
  output$plotConveniosTut3 <- renderHighchart({
    highcharter_convenios(pagamentos_siconv, cidades_semelhantes[3], "BA")
  })
  
  output$plotFornecedoresTut3 <- renderHighchart({
    highcharter_fornecedores(pagamentos_siconv, cidades_semelhantes[3], "BA")
  })
  
  output$plotTabelaTut <- DT::renderDataTable({
    tabela_detalhes(pagamentos_siconv, cidades_semelhantes, "BA", cidade_convenio)
  })
  
  names(tabela_resultante) = c("Cód. IBGE", "Município", "Estado", "Nº de fornecedores",
                               "Nº de convênios", "Total arrecadado", "IDH", "IDH - Renda",
                               "IDH - Longevidade", "IDH - Educação", "População")
  
  plot_tabela = DT::datatable(tabela_resultante, 
                              filter = "top", 
                              options = list(paging = FALSE))
  
  
  })
  
##################################################
  
  output$cidadesSemelhantes2 <- DT::renderDataTable({
    
    cidadesProximas = cidade_resumo
    
    cidadesProximas$dist = dist(rbind(cidadesProximas %>%
                                      filter(MUNIC_PROPONENTE == input$selectCidade) %>%
                                      select(pop),
                                      cidadesProximas %>%
                                      select(pop)))[1:NROW(cidadesProximas)]
    cidades_semelhantes = (cidadesProximas %>%
                             arrange(dist) %>%
                             slice(1:3))$MUNIC_PROPONENTE
    
    cidades_semelhantes_cod = (cidadesProximas %>%
                             arrange(dist) %>%
                             slice(1:3))$cod7
    
    
    output$giniInfoCoef11 <- renderHighchart({
      
      highcharter_coef(ginicities, cidades_semelhantes_cod, coef_ref)
      
    })
    
    
    cidades_semelhantes_id = (cidadesProximas %>%
                             arrange(dist) %>%
                             slice(1:3))$cod7
    
    tabela_resultante = cidade_resumo %>%
      filter(MUNIC_PROPONENTE %in% cidades_semelhantes)
#             UF_PROPONENTE == input$selectEstado)
    
    output$giniBox1 <- renderInfoBox({
      munic = cidades_semelhantes[1]
      infoBox(width = 4, 
              munic, 
              format(round((ginicities %>% filter(cidade == munic))$coef,3), nsmall = 3), 
              icon = icon("percent"),
              color = "teal")
    })
    
    
    output$giniBox2 <- renderInfoBox({
      munic = cidades_semelhantes[2]
      infoBox(width = 4, 
            munic, 
            format(round((ginicities %>% filter(cidade == munic))$coef,3), nsmall = 3), 
            icon = icon("percent"),
            color = "teal")
    })
  
    output$giniBox3 <- renderInfoBox({
      munic = cidades_semelhantes[3]
      infoBox(width = 4, 
            munic, 
            format(round((ginicities %>% filter(cidade == munic))$coef,3), nsmall = 3), 
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
    box_navegador(3, "<b>Mais detalhes</b>", "yellow", "button_box_13", "table", 4)
  })
  
}

shinyApp(ui, server)