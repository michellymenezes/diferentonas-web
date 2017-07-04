## app.R ##
library(shinydashboard)
library(readr)
library(dplyr)
library(plotly)
library(crosstalk)
library(DT)
library(highcharter)



ginicities = read_csv("data/ginicities_5.csv")
pagamentos_siconv = read_csv("data/pagamentos_sincov.csv")
cidade_resumo = read_csv("data/cidade_resumo_5.csv")
cidade_convenio = read_csv("data/cidade_convenio_5.csv")


ui <- dashboardPage( skin= "purple",
  dashboardHeader(title = "As diferentonas - As Supercontratadas", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(id = "myMenu",
                menuItem("Introdução", tabName = "info", icon = icon("info-circle")),
                menuItem("Sua cidade", tabName = "user", icon = icon("bar-chart-o"))),
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
                    box(plotlyOutput("plotConveniosTut2")),
                    box(plotlyOutput("plotFornecedoresTut2"))
                  ),
                  fluidRow(
                    box(plotlyOutput("plotConveniosTut3")),
                    box(plotlyOutput("plotFornecedoresTut3"))
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
                    box(plotlyOutput("plotConvenios11")),
                    box(plotlyOutput("plotFornecedores11"))
                  ),
                  fluidRow(
                    box(plotlyOutput("plotConvenios12")),
                    box(plotlyOutput("plotFornecedores12"))
                  ),
                  fluidRow(
                    box(plotlyOutput("plotConvenios13")),
                    box(plotlyOutput("plotFornecedores13"))
                  )
                ),
                conditionalPanel(
                  condition = "input.panel2 == 3",
                  fluidRow( column(width = 12,
                                   DT::dataTableOutput("plotTabela2")
                  ))
                )
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
  
  
  output$plotConveniosTut1 <- renderHighchart({
    highchart () %>%
      hc_add_series_df(data = pagamentos_siconv %>%
                         filter(MUNIC_PROPONENTE == "ABAIRA",
                                UF_PROPONENTE == "BA") %>% 
                         group_by(MUNIC_PROPONENTE, NR_CONVENIO, IDENTIF_FORNECEDOR, NOME_FORNECEDOR, OBJETO_PROPOSTA) %>% 
                         summarise(total = sum(VL_PAGO)) %>%
                         arrange(-total),
                       type = "column",
                       x = as.factor(NR_CONVENIO),
                       y = total, 
                       group = as.factor(IDENTIF_FORNECEDOR)) %>%
      hc_title(text = "Por convênio - ABAIRA") %>%
      hc_xAxis(type = "category") %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_xAxis(title = list(text="Valor")) %>%
      hc_yAxis(title = list(text="Fornecedor")) %>%
      hc_tooltip(useHTML = TRUE,
                 headerFormat = "<table>",
                 pointFormat = paste("<tr><th>Convênio</th><td>{point.OBJETO_PROPOSTA}</td></tr>",
                                    "<tr><th>Fornecedor</th><td>{point.NOME_FORNECEDOR}</td></tr>",
                                     "<tr><th>Valor do convênio</th><td>{point.total}</td></tr>",
                                     "<tr><th>Valor ao fornecedor</th><td>{point.y}</td></tr>"),
                 footerFormat = "</table>") %>%
      hc_legend(align = "right",  verticalAlign = "middle",
                layout = "vertical")
  })
  
  output$plotFornecedoresTut1 <- renderHighchart({
    
    highchart () %>%
      hc_add_series_df(data = pagamentos_siconv %>%
                         filter(MUNIC_PROPONENTE == "ABAIRA",
                                UF_PROPONENTE == "BA") %>% 
                         group_by(MUNIC_PROPONENTE, IDENTIF_FORNECEDOR, NR_CONVENIO, NOME_FORNECEDOR, OBJETO_PROPOSTA) %>% 
                         summarise(total = sum(VL_PAGO)) %>%
                         arrange(-total),
                       type = "column",
                       x = as.factor(IDENTIF_FORNECEDOR),
                       y = total, 
                       group = as.factor(NR_CONVENIO)) %>%
      hc_title(text = "Por fornecedor - ABAIRA") %>%
      hc_xAxis(type = "category") %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_xAxis(title = list(text="Valor")) %>%
      hc_yAxis(title = list(text="Fornecedor")) %>%
      hc_tooltip(useHTML = TRUE,
                 headerFormat = "<table>",
                 pointFormat = paste("<tr><th>Fornecedor</th><td>{point.NOME_FORNECEDOR}</td></tr>",
                                     "<tr><th>Convênio</th><td>{point.OBJETO_PROPOSTA}</td></tr>",
                                     "<tr><th>Valor do fornecedor</th><td>{point.total}</td></tr>",
                                     "<tr><th>Valor ao convênio</th><td>{point.y}</td></tr>"),
                 footerFormat = "</table>")%>%
      hc_legend(align = "right",  verticalAlign = "middle",
                layout = "vertical")
  })
  
  output$plotConveniosTut2 <- renderPlotly({
    plot_convenios = plot_ly(pagamentos_siconv %>%
                               filter(MUNIC_PROPONENTE == "MAIQUINIQUE",
                                      UF_PROPONENTE == "BA"),
                             x = ~as.factor(NR_CONVENIO),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(IDENTIF_FORNECEDOR),
                             colors = "RdPu",
                             text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Convênio - MAIQUINIQUE"),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Convênios"))
  })
  
  output$plotFornecedoresTut2 <- renderPlotly({
    
    plot_convenios = plot_ly(pagamentos_siconv %>%
                               filter(MUNIC_PROPONENTE == "MAIQUINIQUE",
                                      UF_PROPONENTE == "BA"),
                             x = ~as.factor(IDENTIF_FORNECEDOR),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(NR_CONVENIO),
                             colors = "YlGnBu",
                             text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Fornecedor - MAIQUINIQUE"),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Fornecedores"))
  })
  
  output$plotConveniosTut3 <- renderPlotly({
    plot_convenios = plot_ly(pagamentos_siconv %>%
                               filter(MUNIC_PROPONENTE == "PLANALTINO",
                                      UF_PROPONENTE == "BA"),
                             x = ~as.factor(NR_CONVENIO),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(IDENTIF_FORNECEDOR),
                             colors = "RdPu",
                             text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Convênio - PLANALTINO"),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Convênios"))
  })
  
  output$plotFornecedoresTut3 <- renderPlotly({
    
    plot_convenios = plot_ly(pagamentos_siconv %>%
                               filter(MUNIC_PROPONENTE == "PLANALTINO",
                                      UF_PROPONENTE == "BA"),
                             x = ~as.factor(IDENTIF_FORNECEDOR),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(NR_CONVENIO),
                             colors = "YlGnBu",
                             text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Fornecedor - PLANALTINO"),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Fornecedores"))
  })
  
  output$plotTabelaTut <- DT::renderDataTable({
    
    tabela_resultante = pagamentos_siconv %>%
      filter(MUNIC_PROPONENTE %in% c("ABAIRA", "MAIQUINIQUE", "PLANALTINO"),
             UF_PROPONENTE == "BA") %>%
      filter(NR_CONVENIO %in% cidade_convenio$NR_CONVENIO)%>%
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
    
    output$plotConvenios11 <- renderPlotly({
      
      plot_convenios = plot_ly(pagamentos_siconv %>%
                                 filter(MUNIC_PROPONENTE == cidades_semelhantes[1],
                                        UF_PROPONENTE == input$selectEstado),
                               x = ~as.factor(NR_CONVENIO),
                               y = ~VL_PAGO,
                               type = "bar",
                               color = ~as.factor(IDENTIF_FORNECEDOR),
                               colors = "RdPu",
                               text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
        layout(barmode = 'stack',
               title = paste("Perspectiva Convênio - ", cidades_semelhantes[1]),
               yaxis = list(title = "Total Pago"),
               xaxis = list(title = "Convênios"))
    })
    
    output$plotConvenios12 <- renderPlotly({
      
      plot_convenios = plot_ly(pagamentos_siconv %>%
                                 filter(MUNIC_PROPONENTE == cidades_semelhantes[2],
                                        UF_PROPONENTE == input$selectEstado),
                               x = ~as.factor(NR_CONVENIO),
                               y = ~VL_PAGO,
                               type = "bar",
                               color = ~as.factor(IDENTIF_FORNECEDOR),
                               colors = "RdPu",
                               text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
        layout(barmode = 'stack',
               title = paste("Perspectiva Convênio - ", cidades_semelhantes[2]),
               yaxis = list(title = "Total Pago"),
               xaxis = list(title = "Convênios"))
    })
    
    output$plotConvenios13 <- renderPlotly({
      
      plot_convenios = plot_ly(pagamentos_siconv %>%
                                 filter(MUNIC_PROPONENTE == cidades_semelhantes[3],
                                        UF_PROPONENTE == input$selectEstado),
                               x = ~as.factor(NR_CONVENIO),
                               y = ~VL_PAGO,
                               type = "bar",
                               color = ~as.factor(IDENTIF_FORNECEDOR),
                               colors = "RdPu",
                               text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
        layout(barmode = 'stack',
               title = paste("Perspectiva Convênio - ", cidades_semelhantes[3]),
               yaxis = list(title = "Total Pago"),
               xaxis = list(title = "Convênios"))
    })
    
    output$plotFornecedores11 <- renderPlotly({
      
      plot_convenios = plot_ly(pagamentos_siconv %>%
                                 filter(MUNIC_PROPONENTE == cidades_semelhantes[1],
                                        UF_PROPONENTE == input$selectEstado),
                               x = ~as.factor(IDENTIF_FORNECEDOR),
                               y = ~VL_PAGO,
                               type = "bar",
                               color = ~as.factor(NR_CONVENIO),
                               colors = "YlGnBu",
                               text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
        layout(barmode = 'stack',
               title = paste("Perspectiva Fornecedor - ", cidades_semelhantes[1]),
               yaxis = list(title = "Total Pago"),
               xaxis = list(title = "Fornecedores"))
    })
    
    output$plotFornecedores12 <- renderPlotly({
      
      plot_convenios = plot_ly(pagamentos_siconv %>%
                                 filter(MUNIC_PROPONENTE == cidades_semelhantes[2],
                                        UF_PROPONENTE == input$selectEstado),
                               x = ~as.factor(IDENTIF_FORNECEDOR),
                               y = ~VL_PAGO,
                               type = "bar",
                               color = ~as.factor(NR_CONVENIO),
                               colors = "YlGnBu",
                               text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
        layout(barmode = 'stack',
               title = paste("Perspectiva Fornecedor - ", cidades_semelhantes[2]),
               yaxis = list(title = "Total Pago"),
               xaxis = list(title = "Fornecedores"))
    })
    
    output$plotFornecedores13 <- renderPlotly({
      
      plot_convenios = plot_ly(pagamentos_siconv %>%
                                 filter(MUNIC_PROPONENTE == cidades_semelhantes[3],
                                        UF_PROPONENTE == input$selectEstado),
                               x = ~as.factor(IDENTIF_FORNECEDOR),
                               y = ~VL_PAGO,
                               type = "bar",
                               color = ~as.factor(NR_CONVENIO),
                               colors = "YlGnBu",
                               text = ~paste("Proposta: ", OBJETO_PROPOSTA, "<br>Fornecedor:", NOME_FORNECEDOR)) %>%
        layout(barmode = 'stack',
               title = paste("Perspectiva Fornecedor - ", cidades_semelhantes[3]),
               yaxis = list(title = "Total Pago"),
               xaxis = list(title = "Fornecedores"))
    })
    
    output$plotTabela2 <- DT::renderDataTable({
      
      tabela_resultante = pagamentos_siconv %>%
        filter(MUNIC_PROPONENTE %in% cidades_semelhantes,
               UF_PROPONENTE == input$selectEstado) %>%
        filter(NR_CONVENIO %in% cidade_convenio$NR_CONVENIO)%>%
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

# highchart() %>% 
#   hc_chart(type = "column") %>% 
#   hc_xAxis(categories = (pagamentos_siconv %>%
#                           filter(MUNIC_PROPONENTE == "ABAIRA",
#                                  UF_PROPONENTE == "BA") %>% group_by(NR_CONVENIO))$NR_CONVENIO ) %>%
#   hc_add_series((pagamentos_siconv %>%
#                    filter(MUNIC_PROPONENTE == "ABAIRA",
#                           UF_PROPONENTE == "BA") %>% group_by(IDENTIF_FORNECEDOR))$VL_PAGO) %>%
#   hc_plotOptions(column = list(stacking = "normal"))
# 
# 
# highchart () %>%
#   hc_add_series_df(data = pagamentos_siconv %>%
#                      filter(MUNIC_PROPONENTE == "ABAIRA",
#                             UF_PROPONENTE == "BA") %>% 
#                      group_by(MUNIC_PROPONENTE, NR_CONVENIO, IDENTIF_FORNECEDOR, NOME_FORNECEDOR, OBJETO_PROPOSTA) %>% 
#                      summarise(total = sum(VL_PAGO)) ,
#                    type = "column",
#                    x = as.factor(NR_CONVENIO),
#                    y = total, 
#                    group = as.factor(IDENTIF_FORNECEDOR)) %>%
#   hc_xAxis(type = "category") %>%
#   hc_plotOptions(column = list(stacking = "normal")) %>%
#   hc_tooltip(useHTML = TRUE,
#              headerFormat = "<table>",
#              pointFormat = paste("<tr><th>Fornecedor</th><td>{point.NOME_FORNECEDOR}</td></tr>",
#                                  "<tr><th>Convênio</th><td>{point.OBJETO_PROPOSTA}</td></tr>",
#                                  "<tr><th>Valor do convênio</th><td>{point.total}</td></tr>",
#                                  "<tr><th>Valor ao fornecedor</th><td>{point.y}</td></tr>"),
#              footerFormat = "</table>")
# 
# dados = data_frame(f1 = c("a", "b", "c", "a", "b", "c"), f2 = c("aa", "bb", "cc", "cc", "bb", "aa"), f3 = c(1,2,3, 3, 2,1))
# 
# highchart () %>%
#   hc_add_series_df(data = dados,
#                    type = "column",
#                    x = f1,
#                    y = f3, 
#                    group = f2) %>%
#   hc_xAxis(type = "category") %>%
#   
#   hc_plotOptions(column = list(stacking = "normal"))
# 
# 
# mpgman2 <- count(mpg, manufacturer, year)
# hc_add_series_df(highchart(), mpgman2, "column", x = manufacturer, y = n, group = year) %>% 
#   hc_xAxis(type = "category") %>%
#   hc_plotOptions(column = list(stacking = "normal"))
# 
# data = pagamentos_siconv %>%
#   filter(MUNIC_PROPONENTE == "ABAIRA",
#          UF_PROPONENTE == "BA") %>% 
#   group_by(MUNIC_PROPONENTE, NR_CONVENIO, IDENTIF_FORNECEDOR, NOME_FORNECEDOR, OBJETO_PROPOSTA) %>% 
#   summarise(total = sum(VL_PAGO)) %>% left_join(pagamentos_siconv %>%
#                                                   filter(MUNIC_PROPONENTE == "ABAIRA",
#                                                          UF_PROPONENTE == "BA") %>% 
#                                                   group_by(MUNIC_PROPONENTE, NR_CONVENIO) %>% summarise(total_conv = sum(VL_PAGO)))
