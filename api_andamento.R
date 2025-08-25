# api_andamento.R

library(plumber)
library(dplyr)
library(forcats)
library(jsonlite)

# Carrega o modelo treinado
modelo_treinado <- readRDS("modelos/modelo_predicao_andamento.rds")

levels_pipeline <- c("CACHOEIRO", "PAVILHÃO OUTUBRO", "RIO DE JANEIRO", "SÃO PAULO ")
levels_segmento <- c("Acessórios/Joias", "Brinquedos/Infantil", "Calçados",
                     "Casa/Decoração", "Comércio/Indústria",
                     "Cosméticos/Perfumaria", "Moda/Vestuário")


#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* @post /prever_andamento
#* @param dados_empresa:json Um JSON com os dados da empresa.
#* @serializer json
function(req) {
  tryCatch({
    novos_dados <- jsonlite::fromJSON(req$postBody, flatten = TRUE)
    novos_dados <- as.data.frame(novos_dados, stringsAsFactors = FALSE)
    
    if (nrow(novos_dados) == 0) {
      return(list(previsao = "Dados inválidos: O JSON está vazio."))
    }
    
    colunas_esperadas <- c("pipeline", "ticket", "lojas", "funcionarios", "segmento")
    for (col in colunas_esperadas) {
      if (!col %in% names(novos_dados)) {
        novos_dados[[col]] <- NA
      }
    }
    
    # A sua lógica de correção de espaços, que é crucial
    novos_dados$pipeline <- ifelse(
      novos_dados$pipeline == "SÃO PAULO",
      "SÃO PAULO ",
      novos_dados$pipeline
    )
    
    novos_dados <- novos_dados %>%
      mutate(
        ticket       = as.numeric(ticket),
        lojas        = as.numeric(lojas),
        funcionarios = as.numeric(funcionarios),
        pipeline     = factor(pipeline, levels = levels_pipeline),
        segmento     = fct_explicit_na(factor(segmento, levels = levels_segmento))
      )
    
    if (anyNA(novos_dados)) {
      return(list(error = paste(
        "Dados inválidos: existem valores ausentes ou categorias não reconhecidas.",
        "pipeline recebido:", as.character(novos_dados$pipeline),
        "| segmento recebido:", as.character(novos_dados$segmento)
      )))
    }
    
    previsoes_probabilidade <- predict(modelo_treinado, newdata = novos_dados, type = "prob")
    
    prob_avanca <- previsoes_probabilidade$Avançou
    prob_nao_avanca <- previsoes_probabilidade$`Não Avançou`
    
    mensagem <- if_else(
      prob_avanca > prob_nao_avanca,
      paste0("Avança com ", round(prob_avanca * 100, 1), "% de probabilidade."),
      paste0("Não avança com ", round(prob_nao_avanca * 100, 1), "% de probabilidade.")
    )
    
    return(list(previsao = mensagem))
    
  }, error = function(e) {
    return(list(error = paste("Erro no processamento:", e$message)))
  })
}