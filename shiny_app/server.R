library(shiny)
library(plotly)
library(DT)
library(reshape2)
library(readr)

ginicities = read_csv("data/ginicities_5.csv")
pagamentos_siconv = read_csv("data/pagamentos_sincov.csv")
cidade_resumo = read_csv("data/cidade_resumo_5.csv")





# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  
  observe({
   
    updateSelectInput(session, "selectCidade",
                      choices = ginicities %>%
                                filter(UF == input$selectEstado) %>%
                                select(cidade))
    

  })
  
  output$plotCidades <- renderPlotly({
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
    
    pp = plot_ly(ginicities %>% filter(UF == input$selectEstado),
                 x = ~gini_valor_convenio,
                 y = ~gini_valor_fornecedor,
                 color = ~cidade %in% cidades_semelhantes,
                 colors = "Spectral",
                 marker=list( size=10, opacity = 0.6),
                 text = ~paste("Cidade: ", cidade, "\nCoeficiente: ", coef)) %>%
      layout(showlegend = FALSE,
             title = paste("Ginicities"),
             yaxis = list(title = "Gini Fornecedor"),
             xaxis = list(title = "Gini Convênio"))
  })
  
  output$cidadesSemelhantes <- DT::renderDataTable({
    
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
    
    plot_tabela = DT::datatable(tabela_resultante, 
                                filter = "top", 
                                options = list(paging = FALSE))
  })
  
  
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
                             x = ~as.factor(IDENTIF_FORNECEDOR),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(NR_CONVENIO),
                             colors = "RdPu") %>%
                    layout(barmode = 'stack',
                           title = paste("Perspectiva Fornecedor - ", cidades_semelhantes[1]),
                           yaxis = list(title = "Total Pago"),
                           xaxis = list(title = "Convênios"))
    

  })
  
  output$plotConvenios2 <- renderPlotly({
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
                               filter(MUNIC_PROPONENTE == cidades_semelhantes[2],
                                      UF_PROPONENTE == input$selectEstado),
                             x = ~as.factor(IDENTIF_FORNECEDOR),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(NR_CONVENIO),
                             colors = "RdPu") %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Fornecedor - ", cidades_semelhantes[2]),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Convênios"))
    
    
  })
  
  output$plotConvenios3 <- renderPlotly({
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
                               filter(MUNIC_PROPONENTE == cidades_semelhantes[3],
                                      UF_PROPONENTE == input$selectEstado),
                             x = ~as.factor(IDENTIF_FORNECEDOR),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(NR_CONVENIO),
                             colors = "RdPu") %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Fornecedor - ", cidades_semelhantes[3]),
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
                             x = ~as.factor(NR_CONVENIO),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(IDENTIF_FORNECEDOR),
                             colors = "YlGnBu") %>%
                    layout(barmode = 'stack',
                           title = paste("Perspectiva Convênio - ", cidades_semelhantes[1]),
                           yaxis = list(title = "Total Pago"),
                           xaxis = list(title = "Fornecedores"))
  })
  
  
  output$plotFornecedores2 <- renderPlotly({
    
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
                               filter(MUNIC_PROPONENTE == cidades_semelhantes[2],
                                      UF_PROPONENTE == input$selectEstado),
                             x = ~as.factor(NR_CONVENIO),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(IDENTIF_FORNECEDOR),
                             colors = "YlGnBu") %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Convênio - ", cidades_semelhantes[2]),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Fornecedores"))
  })
  
  
  output$plotFornecedores3 <- renderPlotly({
    
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
                               filter(MUNIC_PROPONENTE == cidades_semelhantes[3],
                                      UF_PROPONENTE == input$selectEstado),
                             x = ~as.factor(NR_CONVENIO),
                             y = ~VL_PAGO,
                             type = "bar",
                             color = ~as.factor(IDENTIF_FORNECEDOR),
                             colors = "YlGnBu") %>%
      layout(barmode = 'stack',
             title = paste("Perspectiva Convênio - ", cidades_semelhantes[3]),
             yaxis = list(title = "Total Pago"),
             xaxis = list(title = "Fornecedores"))
  })
  
  output$plotTabela <- DT::renderDataTable({
    
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
    
    tabela_resultante = pagamentos_siconv %>%
                          filter(MUNIC_PROPONENTE %in% cidades_semelhantes,
                                  UF_PROPONENTE == input$selectEstado) %>%
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
})
