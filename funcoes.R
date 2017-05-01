
#' Recebe as iniciativas de um arquivo baixado do siconv e 
#' descarta as partes que não utilizamos no Diferentonas. 
#' 
filtra_convenios = function(convenios) {
  require(dplyr, warn.conflicts = FALSE)
  futile.logger::flog.info("Antes de filtrar: %d convênios", NROW(convenios))
  
  ## seleciona e salva apenas os convênios propostos no âmbito municipal
  futile.logger::flog.info("Filtrando aqueles depois de 2013 e celebrados por municípios + Brasília")
  t = convenios %>%
    filter(ANO_PROP >= 2013,
           (NATUREZA_JURIDICA == "Administração Pública Municipal") |
             ((MUNIC_PROPONENTE == "BRASILIA") & # Os convênios de Brasília não são municípios
                (NATUREZA_JURIDICA == "Administração Pública Estadual ou do Distrito Federal")
             )) %>%
    select(-NATUREZA_JURIDICA)
  
  futile.logger::flog.info(paste("Filtrados são ", NROW(convenios), "convênios"))
  return(t)
}