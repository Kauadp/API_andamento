# API de Previsão de Andamento de Oportunidades

Esta API fornece previsões sobre o **andamento de oportunidades de negócios** com base em informações de pipeline, ticket médio, número de lojas, funcionários e segmento de atuação. O modelo é construído usando **Random Forest** via o pacote **caret** no R.

------------------------------------------------------------------------

## 1. Contexto

O projeto processa dados de oportunidades e comentários de clientes para construir um modelo preditivo que informa se uma oportunidade **vai avançar** ou **não avançar** no pipeline de vendas.

O pipeline de construção envolve:

1.  ETL dos dados de oportunidades via API (`get_leads2b`) e CSVs de qualificações.
2.  Limpeza e transformação das informações (lojas, funcionários, ticket médio, segmento) utilizando **expressões regulares (regex)**.
3.  Mesclagem dos dados de diferentes fontes (API + CSVs) para consolidar informações.
4.  Imputação de valores faltantes usando **mice**.
5.  Treinamento de modelo **Random Forest** com validação cruzada (10 folds) usando **caret**.
6.  Salvamento do modelo final (`modelo_predicao_andamento.rds`) para uso na API.

------------------------------------------------------------------------

## 2. ETL e Processamento de Dados

### 2.1 Download e Consolidação de Dados

-   **API Leads2B:** Os dados de oportunidades são baixados em lotes de 200 registros, utilizando `start_at` e `end_at` para definir o período. Os dados são concatenados em uma única base.\
-   **Arquivos CSV de Qualificação:** Diversos arquivos CSV (`quali1.csv` a `quali9.csv`) contendo informações de comentários dos clientes são carregados manualmente e combinados em uma única base.

``` r
dados_opor <- c()
for (i in seq(1, 10, by = 1)) {
  dados_opor <- bind_rows(get_leads2b("/api/v1/opportunities/list",
                                      list(start_at = "2025-06-01 00:00:00",
                                           end_at = "2025-08-14 00:00:00",
                                           limit = 200,
                                           offset = i)),
                          dados_opor)
}

quali <- c()
for (i in 1:9) {
  quali <-  bind_rows(quali,
                      read.csv(paste("dados_quali/quali",i,".csv",sep = "")))
}
```

### 2.2 Extração de informações com Regex

As informações de interesse (lojas, funcionários, ticket médio e segmento) são extraídas de textos não estruturados usando *expressões regulares*:

``` r
matriz <- quali |> 
  filter(Tipo == "Comentário") |> 
  filter(str_detect(Mensagem, "Matriz de Qualificação")) |> 
  mutate(
    lojas = as.numeric(str_match(Mensagem, "Quantas lojas\\? : .*?([0-9]+)")[,2]),
    funcionarios = as.numeric(str_match(Mensagem, "Quantos funcionários\\? : .*?([0-9]+)")[,2]),
    ticket = as.numeric(str_match(Mensagem, "Média de preço dos produtos : .*?([0-9]+)")[,2])
  ) |> 
  select(ID.do.negócio, ticket, lojas, funcionarios)

estudo <- quali |> 
  filter(Tipo == "Comentário") |> 
  filter(str_detect(Mensagem, "Estudo de Empresa")) |> 
  mutate(
    segmento = str_match(Mensagem, "(?i)Produtos/Serviços: Anote os principais produtos ou serviços oferecidos.: .*?([[:alpha:]]+)")[,2]
  ) |> 
  select(ID.do.negócio, segmento)
```

### 2.3 Mesclagem e Limpeza

As bases processadas (matriz e estudo) são combinadas usando left join e renomeadas para nomes consistentes:

``` r
quali_processado <- left_join(matriz, estudo, by = "ID.do.negócio", relationship = "many-to-many")
names(quali_processado) <- c("opportunity_id", "ticket", "lojas", "funcionarios", "segmento")
```

O dataset final de treinamento é obtido juntando os dados da API com os dados processados:

``` r
dados <- left_join(dados_opor, quali_processado, by = "opportunity_id")
dados_sel <- dados |> 
  select(pipeline, pipeline_step, company_name, ticket, lojas, funcionarios, segmento)
```

### 2.4 Tratamento de Segmentos e Valores Faltantes

-   Segmentos dos clientes são limpos e categorizados com regras de regex, agrupando categorias semelhantes.

-   Valores faltantes em ticket, lojas, funcionarios e segmento são imputados usando o pacote mice.

```r
dados_para_imputar <- dados_sel |> 
  select(pipeline_step, pipeline, ticket, lojas, funcionarios, segmento) |> 
  mutate(across(where(is.character), as.factor))

imputacao_modelo <- mice(dados_para_imputar, m = 5, printFlag = FALSE)
dados_sel <- complete(imputacao_modelo, 1)
```
 - As colunas de pipeline são padronizadas (trim, toupper) e a variável resposta andamento_negociacao é criada com base em regras de negócio.
 
## 3. Modelagem

O modelo é construído usando Random Forest com caret e validação cruzada de 10 folds

## 4. Endpoint

POST /prever_andamento

*Body (JSON)*:

```r
{
  "pipeline": "SAO PAULO",
  "ticket": 150,
  "lojas": 2,
  "funcionarios": 8,
  "segmento": "Calcados"
}
```
*Response (JSON)*:

```r
{
  "previsao": "Avançou"
}
```
Use *POST*, não *GET*, e envie o JSON completo.

## 5. Observações

 - ETL combina dados manuais e via API, usando regex para extrair informações importantes.

 - Modelo treinado com dados entre 2025-06-01 e 2025-08-14.

 - Seguir padrão de JSON e categorias para obter previsões corretas.

 - Segmentos são agrupados em categorias padronizadas para consistência.