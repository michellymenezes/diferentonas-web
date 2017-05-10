library(dplyr)
library(reldist)
library(readr)
source("funcoes.R")

cria_pagamentos_sincov = function(){
pagamentos = read.csv("siconv/siconv_pagamento.csv", sep = ";")
convenios = read.csv("siconv/siconv_convenio.csv", sep = ";")
propostas = read.csv("siconv/siconv_proposta.csv", sep = ";")

convenios_siconv = left_join(convenios,
                             propostas,
                             by = c("ID_PROPOSTA"))
convenios_siconv = convenios_siconv %>%
                  filter(!duplicated(convenios_siconv))

pagamentos_siconv = left_join(pagamentos,
                              convenios_siconv,
                              by = c("NR_CONVENIO")) %>%
                    filtra_convenios()


pagamentos_siconv = pagamentos_siconv %>%
                    filter(nchar(as.character(pagamentos_siconv$IDENTIF_FORNECEDOR)) == 14)

pagamentos_siconv$VL_PAGO = gsub(",", ".", pagamentos_siconv$VL_PAGO) %>% 
                            as.numeric()

return(pagamentos_siconv)
}

cria_cidade_fornecedor = function(pagamentos_siconv){
# para cada cidade, uma contagem de quantas vezes um fornecedor recebeu 
# pagamento e o total pago a cada um
cidade_fornecedor = pagamentos_siconv %>% 
                    group_by(COD_MUNIC_IBGE, MUNIC_PROPONENTE, UF_PROPONENTE, IDENTIF_FORNECEDOR) %>% 
                    summarise(count = n(), soma_por_fornecedor = sum(VL_PAGO))

return(cidade_fornecedor)
}

cria_cidade_convenio = function(pagamentos_siconv){
# para cada cidade, todos os seus convenios e o valor de cada um
cidade_convenio = pagamentos_siconv %>% 
                  group_by(COD_MUNIC_IBGE, MUNIC_PROPONENTE, UF_PROPONENTE, NR_CONVENIO) %>% 
                  summarise(count = n(), soma_por_convenio = sum(VL_PAGO))

return(cidade_convenio)
}

cria_cidade_resumo = function(cidade_fornecedor, cidade_convenio){
  
municipios = read_csv("dados2010.csv", 
                        col_types = cols(.default = col_double(),
                                         cod6 = col_character(),
                                         cod7 = col_character(),
                                         municipio = col_character(),
                                         UF = col_character()))

municipios$municipio = rm_accent(toupper(municipios$municipio))
  
# para cada cidade, a quantidade de convenios, pessoas juridicas e total em valor
cidade_resumo = cidade_fornecedor %>%
                group_by(COD_MUNIC_IBGE, MUNIC_PROPONENTE, UF_PROPONENTE) %>% 
                summarise(n_pessoa_juridica = n()) %>% 
                left_join(cidade_convenio %>% 
                      group_by(COD_MUNIC_IBGE, MUNIC_PROPONENTE, UF_PROPONENTE) %>% 
                      summarise(n_convenio = n(), total_pago = sum(soma_por_convenio)))

names(cidade_resumo) = c("cod7","MUNIC_PROPONENTE", "UF_PROPONENTE", "n_pessoa_juridica", "n_convenio", "total_pago")
cidade_resumo$cod7 = as.character(cidade_resumo$cod7)

# adicionando características de cada municipio
cidade_resumo = cidade_resumo %>% left_join(municipios[,c("cod7", "idhm", "idhm_renda", "idhm_longev", "idhm_edu", "pop")])

return(cidade_resumo)
}

filtra_cidade_resumo = function(cidade_resumo, nConv) {
  cidade_resumo = cidade_resumo %>%
                  subset(n_convenio >= nConv)
  
  return(cidade_resumo)
}

atualiza_cidade_convenio = function(cidade_convenio, cidade_resumo){
  
  cidade_convenio$COD_MUNIC_IBGE = as.character(cidade_convenio$COD_MUNIC_IBGE)
  cidade_convenio = cidade_convenio %>%
                    semi_join(cidade_resumo, by = c("MUNIC_PROPONENTE", "UF_PROPONENTE"))
  
  return(cidade_convenio)
}

atualiza_cidade_fornecedor= function(cidade_fornecedor, cidade_resumo){
  
  cidade_fornecedor$COD_MUNIC_IBGE = as.character(cidade_fornecedor$COD_MUNIC_IBGE)
  cidade_fornecedor = cidade_fornecedor %>%
                      semi_join(cidade_resumo, by = c("MUNIC_PROPONENTE", "UF_PROPONENTE"))
  
  return(cidade_fornecedor)
}

filtra_quantile_convenio = function(cidade_convenio, quant) {

  quantiles = cidade_convenio %>%
              left_join(cidade_convenio %>%
              group_by(COD_MUNIC_IBGE, MUNIC_PROPONENTE, UF_PROPONENTE) %>%
              summarise(quant = quantile(soma_por_convenio,0.9)))
  
  cidade_convenio = quantiles %>%
                    filter(soma_por_convenio <= quant)
  
  return(cidade_convenio[,1:6])
}

cria_ginicities = function(cidade_fornecedor, cidade_convenio){
ginicities = aggregate(soma_por_convenio ~ MUNIC_PROPONENTE + UF_PROPONENTE ,
                       data = cidade_convenio,
                       FUN = "gini") %>%
            left_join(aggregate(soma_por_fornecedor ~ MUNIC_PROPONENTE + UF_PROPONENTE,
                                data = cidade_fornecedor,
                                FUN = "gini")) 

names(ginicities) <- c("cidade", "UF", "gini_valor_convenio", "gini_valor_fornecedor")

ginicities$coef = ginicities$gini_valor_fornecedor/ginicities$gini_valor_convenio
ginicities$coef[ginicities$gini_valor_convenio == 0] = NA

return(ginicities)
}

