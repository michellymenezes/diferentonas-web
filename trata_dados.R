library(dplyr)
source("funcoes.R")

pagamentos = read.csv("siconv/siconv_pagamento.csv", sep = ";")
convenios = read.csv("siconv/siconv_convenio.csv", sep = ";")
propostas = read.csv("siconv/siconv_proposta.csv", sep = ";")
municipios = read_csv("dados2010.csv", 
                      col_types = cols(.default = col_double(),
                                       cod6 = col_character(),
                                       cod7 = col_character(),
                                       municipio = col_character(),
                                       UF = col_character()))

convenios_siconv = left_join(convenios,
                             propostas,
                             by = c("ID_PROPOSTA"))
convenios_siconv = filter(!duplicated(convenios_siconv))

pagamentos_siconv = left_join(pagamentos,
                              convenios_siconv,
                              by = c("NR_CONVENIO")) %>%
                    filtra_convenios()

pagamentos_siconv = pagamentos_siconv %>%
                    filter(nchar(as.character(pagamentos_siconv$IDENTIF_FORNECEDOR)) == 14)

pagamentos_siconv$VL_PAGO = gsub(",", ".", pagamentos_siconv$VL_PAGO) %>% 
                            as.numeric()

# para cada cidade, uma contagem de quantas vezes um fornecedor recebeu 
# pagamento e o total pago a cada um
cidade_fornecedor = pagamentos_siconv %>% 
                    group_by(COD_MUNIC_IBGE, MUNIC_PROPONENTE, IDENTIF_FORNECEDOR) %>% 
                    summarise(count = n(), soma_por_fornecedor = sum(VL_PAGO))

# para cada cidade, todos os seus convenios e o valor de cada um
cidade_convenio = pagamentos_siconv %>% 
                  group_by(COD_MUNIC_IBGE, MUNIC_PROPONENTE, NR_CONVENIO) %>% 
                  summarise(count = n(), soma_por_convenio = sum(VL_PAGO))

# para cada cidade, a quantidade de convenios, pessoas juridicas e total em valor
cidade_resumo = cidade_fornecedor %>%
                group_by(COD_MUNIC_IBGE, MUNIC_PROPONENTE) %>% 
                summarise(n_pessoa_juridica = n()) %>% 
                left_join(cidade_convenio %>% 
                      group_by(COD_MUNIC_IBGE, MUNIC_PROPONENTE) %>% 
                      summarise(n_convenio = n(), total_pago = sum(soma_por_convenio)))

names(cidade_resumo) = c("cod7","municipio", "n_pessoa_juridica", "n_convenio", "total_pago")
cidade_resumo$cod7 = as.character(cidade_resumo$cod7)

# adicionando características de cada municipio
cidade_resumo = cidade_resumo %>% left_join(municipios[,c("cod7", "idhm", "idhm_renda", "idhm_longev", "idhm_edu", "pop")])

ginicities = aggregate(soma_por_convenio ~ MUNIC_PROPONENTE,
                       data = cidade_convenio,
                       FUN = "gini") %>%
            left_join(aggregate(soma_por_fornecedor ~ MUNIC_PROPONENTE,
                                data = cidade_fornecedor,
                                FUN = "gini")) 

names(ginicities) <- c("cidade", "gini_valor_convenio", "gini_valor_fornecedor")

ginicities$coef = ginicities$gini_valor_fornecedor/ginicities$gini_valor_convenio
ginicities$coef[ginicities$gini_valor_convenio == 0] = NA
