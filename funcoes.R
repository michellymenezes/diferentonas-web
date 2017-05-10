
#' Recebe as iniciativas de um arquivo baixado do siconv e 
#' descarta as partes que não utilizamos no Diferentonas. 
#' 
filtra_convenios = function(convenios) {
  require(dplyr, warn.conflicts = FALSE)
  futile.logger::flog.info("Antes de filtrar: %d convênios", NROW(convenios))
  
  ## seleciona e salva apenas os convênios propostos no âmbito municipal
  futile.logger::flog.info("Filtrando aqueles depois de 2013 e celebrados por municípios + Brasília")
  t = convenios %>%
    filter(ANO_PROP >= 2009,
           (NATUREZA_JURIDICA == "Administração Pública Municipal") |
             ((MUNIC_PROPONENTE == "BRASILIA") & # Os convênios de Brasília não são municípios
                (NATUREZA_JURIDICA == "Administração Pública Estadual ou do Distrito Federal")
             )) %>%
    select(-NATUREZA_JURIDICA)
  
  futile.logger::flog.info(paste("Filtrados são ", NROW(convenios), "convênios"))
  return(t)
}


rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}