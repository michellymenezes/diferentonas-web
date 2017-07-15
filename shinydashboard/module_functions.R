library(highcharter)
library(dplyr)

highcharter_fornecedores = function(pagamentos_siconv, cidade, estado){
  hc = highchart () %>%
    hc_add_series_df(data = pagamentos_siconv %>%
                       filter(MUNIC_PROPONENTE == cidade,
                              UF_PROPONENTE == estado) %>% 
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
                       filter(MUNIC_PROPONENTE == cidade,
                              UF_PROPONENTE == estado) %>% 
                       group_by(MUNIC_PROPONENTE, NR_CONVENIO, IDENTIF_FORNECEDOR, NOME_FORNECEDOR, OBJETO_PROPOSTA) %>% 
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
  