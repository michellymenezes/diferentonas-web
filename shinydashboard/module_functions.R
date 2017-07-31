library(highcharter)
library(dplyr)
library(shinydashboard)
library(DT)


highcharter_fornecedores = function(pagamentos_siconv, cidade, estado){
  hc = highchart () %>%
    hc_add_series_df(data = pagamentos_siconv %>%
                       filter(MUNIC_PROPONENTE == cidade) %>%
#                              UF_PROPONENTE == estado) %>% 
                       group_by(MUNIC_PROPONENTE, IDENTIF_FORNECEDOR, NR_CONVENIO, NOME_FORNECEDOR, OBJETO_PROPOSTA) %>% 
                       summarise(total = sum(VL_PAGO)) %>%
                       arrange(-total),
                     type = "column",
                     x = as.factor(IDENTIF_FORNECEDOR),
                     y = total, 
                     group = as.factor(NR_CONVENIO)) %>%
    hc_title(text = paste("Por fornecedor - ", cidade)) %>%
    hc_xAxis(type = "category", title = list(text="Fornecedor")) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_yAxis(title = list(text="Valor")) %>%
    hc_tooltip(useHTML = TRUE,
               headerFormat = "<table>",
               pointFormat = paste("<tr><th>Fornecedor</th><td>{point.NOME_FORNECEDOR}</td></tr>",
                                   "<tr><th>Convênio</th><td>{point.OBJETO_PROPOSTA}</td></tr>",
                                   "<tr><th>Valor do fornecedor</th><td>{point.total}</td></tr>",
                                   "<tr><th>Valor ao convênio</th><td>{point.y}</td></tr>"),
               footerFormat = "</table>")%>%
    hc_legend(align = "right",  verticalAlign = "middle",
              layout = "vertical")
  
  return(hc)
}

highcharter_convenios = function(pagamentos_siconv, cidade, estado){
  highchart () %>%
    hc_add_series_df(data = pagamentos_siconv %>%
                       filter(MUNIC_PROPONENTE == cidade)%>%
#                              UF_PROPONENTE == estado) %>% 
                       group_by(MUNIC_PROPONENTE, UF_PROPONENTE, NR_CONVENIO, IDENTIF_FORNECEDOR, NOME_FORNECEDOR, OBJETO_PROPOSTA) %>% 
                       summarise(total = sum(VL_PAGO)) %>%
                       arrange(-total),
                     type = "column",
                     x = as.factor(NR_CONVENIO),
                     y = total, 
                     group = as.factor(IDENTIF_FORNECEDOR)) %>%
    hc_title(text = paste("Por convênio - ", cidade)) %>%
    hc_xAxis(type = "category", title = list(text="Convênio")) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_yAxis(title = list(text="Valor")) %>%
    hc_tooltip(useHTML = TRUE,
               headerFormat = "<table>",
               pointFormat = paste("<tr><th>Convênio</th><td>{point.OBJETO_PROPOSTA}</td></tr>",
                                   "<tr><th>Fornecedor</th><td>{point.NOME_FORNECEDOR}</td></tr>",
                                   "<tr><th>Valor do convênio</th><td>{point.total}</td></tr>",
                                   "<tr><th>Valor ao fornecedor</th><td>{point.y}</td></tr>"),
               footerFormat = "</table>") %>%
    hc_legend(align = "right",  verticalAlign = "middle",
              layout = "vertical")
}

highcharter_coef = function(ginicities, cidades_semelhantes, coef_ref){
  
  temp = ginicities %>% filter(cidades_semelhantes[1] == cidade |
                                 cidades_semelhantes[2] == cidade |
                                 cidades_semelhantes[3] == cidade) %>%
    mutate("legenda" = cidade) %>%
    select(cod7, cidade, UF, pop, coef, legenda)
  
  
  info_coef = coef_ref %>% rbind(temp)
  
  highchart () %>%
    hc_add_series_df(data = info_coef, 
                     type = "bubble", 
                     x = as.factor(legenda),
                     y= coef, size = pop) %>%
    hc_title(text = "Municípios e seus coeficientes") %>%
    hc_xAxis(type = "category", title = list(text="Municípios")) %>%
    hc_yAxis(title = list(text="Coeficiente de supercontratação")) %>%
    hc_legend(enabled = F) %>%
    hc_tooltip(useHTML = TRUE,
               headerFormat = "<table>",
               pointFormat = paste("<tr><th>Cidade</th><td>{point.cidade}</td></tr>",
                                   "<tr><th>Estado</th><td>{point.UF}</td></tr>",
                                   "<tr><th>Coeficiente</th><td>{point.y}</td></tr>",
                                   "<tr><th>População</th><td>{point.pop}</td></tr>"),
               footerFormat = "</table>")
}

box_navegador = function(valor, nome, cor, id, icone, coluna){
  
  box0<-valueBox(value=valor
                 ,icon = icon(icone)
                 ,width=coluna
                 ,color = cor
                 ,href="#"
                 ,subtitle=HTML(nome)
  )
  
  box0$children[[1]]$attribs$class<-"action-button"
  box0$children[[1]]$attribs$id<-id
  return(box0)
  
}

tabela_detalhes = function(pagamentos_siconv, cidades_semelhantes, estado, cidade_convenio){
  tabela_resultante = pagamentos_siconv %>%
    filter(MUNIC_PROPONENTE %in% cidades_semelhantes) %>%
#           UF_PROPONENTE == estado) %>%
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
    summarise(total_pago = format(round(sum(VL_PAGO), 2), nsmall=2, big.mark=","))
  
  names(tabela_resultante) = c("Município", "Estado", "Nº do convênio", "CNPJ do forncecedor",
                               "Nome do fornecedor", "Proposta do convênio", "Valor pago")
  
  plot_tabela = DT::datatable(tabela_resultante, 
                              filter = "top", 
                              options = list(paging = FALSE))
  
  return(plot_tabela)
}