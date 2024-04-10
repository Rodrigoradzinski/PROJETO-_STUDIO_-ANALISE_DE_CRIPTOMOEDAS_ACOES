
#------------------------------------------------------------------------------------------------------
#base escolhida #yahoofinance 
#------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------
# CARREGANDO PACOTES NECESSÁRIOS
#------------------------------------------------------------------------------------------------------

if (!require('quantmod')) install.packages('quantmod')
if (!require('tseries')) install.packages('tseries')
if (!require('tidyquant')) install.packages('tidyquant')
if (!require('reshape2')) install.packages('reshape2')
if (!require('httr')) install.packages('httr')
if (!require('rvest')) install.packages('rvest')
if (!require('DBI')) install.packages('DBI')
if (!require('odbc')) install.packages('odbc')
if (!require('corrplot')) install.packages('corrplot')
if (!require('caret')) install.packages('caret')
if (!require('pROC')) install.packages('pROC')
library(pROC)
library(dplyr)
library(caret)
library(pROC)
library(corrplot)
library(DBI)
library(odbc)
library(rvest)
library(httr)
library(reshape2)
library(quantmod)
library(tseries)
library(tidyquant)
library(dplyr)
library(tseries)
library(zoo)
library(tidyverse)
library(plotly)
library(rvest)
library(dplyr)
library(dplyr)
library(caret)
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(plotly)

#conexao com sql server 

#------------------------------------------------------------------------------------------------------
#COMO VOU USAR BANDO DE DADOS SQL SERVER IREI CRIAR UMA CONEXAO COM O BANCO
#------------------------------------------------------------------------------------------------------
conexao_sql= dbConnect(odbc::odbc(),
                 Driver = "SQL Server", 
                 Server = "DIGO",
                 Database = "CRIPTO",
                 Trusted_Connection = "Yes",
                 Port = 1433)

#------------------------------------------------------------------------------------------------------
#UMA  FUNCAO QUER REALIZA CONVERSAO E LIMPEZA DO DADOS EXTRAIDOS POR ESCRAPIN
#------------------------------------------------------------------------------------------------------

limpar_e_converter = function(coluna, is_percent = FALSE) {
  # Remover vírgulas
  coluna = gsub(",", "", coluna)
  # Identificar os sufixos 'B', 'M' e '%' e aplicar multiplicadores ou conversões
  multiplier = ifelse(grepl("B$", coluna), 1e9, 1)
  multiplier[grepl("M$", coluna)] = 1e6
  coluna = gsub("B$", "", coluna)
  coluna = gsub("M$", "", coluna)
  if (is_percent) {
    coluna = gsub("%$", "", coluna)
  }
  # Converter para numérico
  coluna_num = suppressWarnings(as.numeric(coluna))
  # Aplicar multiplicadores
  coluna_num = coluna_num * multiplier
  # Processamento para porcentagem
  if (is_percent) {
    coluna_num = coluna_num / 100
  }
  # Tratar NAs que possam surgir na conversão
  coluna_num[is.na(coluna_num)] = 0
  return(coluna_num)
}  

#------------------------------------------------------------------------------------------------------
# Calcular métricas de acordo com as matrizes de correlação geradas, igual à prof manual
#------------------------------------------------------------------------------------------------------
calcular_metricas = function(tabela) {
  acuracia = sum(diag(tabela)) / sum(tabela) # A acurácia é a proporção de classificações corretas
  sensibilidade = tabela[2, 2] / sum(tabela[2, ]) # A sensibilidade mede a capacidade de identificar positivos verdadeiros
  especificidade = tabela[1, 1] / sum(tabela[1, ]) # A especificidade mede a capacidade de identificar negativos verdadeiros
  precisao = tabela[2, 2] / sum(tabela[, 2]) # A precisão é a proporção de verdadeiros positivos entre todas as classificações positivas
  
  # F1-Score é a média harmônica de precisão e sensibilidade
  F1 = ifelse(precisao + sensibilidade > 0, 2 * (precisao * sensibilidade) / (precisao + sensibilidade), 0)
  
  list(acuracia = acuracia, sensibilidade = sensibilidade, especificidade = especificidade, precisao = precisao, F1 = F1)
}
#------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------
#Irei trabalhar com dois métodos: o primeiro é o aprendizado em sala de aula, e o segundo é o que aprendi
#por conta própria no trabalho. No primeiro método, tratarei de importação de dados da fonte Yahoo Finance,
#usando a biblioteca Spotifir, e aplicarei técnicas de regressão logística para prever valores diários.
#
#No segundo método, realizarei scraping de dados de um site, lendo um HTML e filtrando informações específicas 
#a cada 5 minutos. O objetivo aqui é prever se o valor vai subir ou descer, visando identificar oportunidades financeiras.
#Se o valor subir, consideraremos a compra, enquanto se descer, a venda.
#
#É importante lembrar que a regressão logística prevê apenas valores binários, ou seja, 0 (falso) ou 1 (verdadeiro).
#No entanto, tenho planos de estudar e testar outros modelos de machine learning no futuro."
#------------------------------------------------------------------------------------------------------
#segunda a sexta-feira mercado de acoes b3
#cripto 24 horas
#.............PARAMETROS DO DO SCRIPT............
#------------------------------------------------------------------------------------------------------
lista_ticker          = c( 'ETH-USD','BTC-USD')  #'MGLU3.SA','AMER3.SA') #ETH-USD','BTC-USD') #AMER3.SA','IBM','GOOG
data_inicial          = '1900-01-01'
data_final            = '2023-11-15'
lista_quote           = c( "Open","High","Low","Close") # "Open","High","Low","Close"
tipo_de_compression   = 'd'   # d diario  w semanais  m mensais
#---------------------------------------------------------------------------
#DADOS HISTORICOS 
#---------------------------------------------------------------------------
#buscando dados historicos direto da fonte 
text_funcao_2 = function(ticker) {
  tryCatch({
    dados = tq_get(ticker,
                    get = "stock.prices", 
                    from = data_inicial, 
                    to = data_final)
                     return(dados)
          }, error = function(e) {
            message("Erro ao obter dados para ", ticker, ": ", e$message)
             return(NULL) 
                      }, warning = function(w) {
                      warning("Aviso ao obter dados para ", ticker, ": ", w$message)
                          return(NULL)  
  })
}
#se nao encontrar o ticker ele da erro entao aqui tramtamento para ignorar os warnings
dados_2 = suppressWarnings(lapply(lista_ticker,text_funcao_2)) # ignora se tiver warnings()
dados_2 = Filter(Negate(is.null), dados_2) #forcar eliminatndo is null

dados_combinados_2 = bind_rows(dados_2, .id = "Ticker")

view1 =head(dados_combinados_2, 25)
head(dados_combinados_2)

#---------------------------------------------------------------------------
#analise exploratorioa para verificar tendencias overfit etc 
#---------------------------------------------------------------------------
# tipos de dados 
str(dados_combinados_2)
#---------------------------------------------------------------------------
# se dados muito longo nao tem como mostrar dia adia no rodapé do grafico
#---------------------------------------------------------------------------
diferenca_dias = as.Date(data_final) - as.Date(data_inicial)
if (diferenca_dias < 1) {
  # Se a diferença for menor que um dia, use horas e minutos
  formato_data = "%H:%M"
} else if (diferenca_dias < 60) {
  # Se a diferença for maior que um dia e menor que 60 dias, mostre apenas os dias
  formato_data = "%d/%m"
} else if (diferenca_dias < 365) {
  # Se a diferença for maior que 60 dias e menor que 365, mostre o mês e o dia
  formato_data = "%m/%Y"
} else {
  # Se a diferença for maior que 365 dias, mostre apenas o ano
  formato_data = "%Y"
}
#---------------------------------------------------------------------------
#funcao para criar  eixo x do grafico se nao ele pula data 
#---------------------------------------------------------------------------

date_frequencia= if (diferenca_dias < 1) {
  "1 hour"
} else if (diferenca_dias < 60) {
  "1 day"
} else if (diferenca_dias < 365) {
  "1 month"
} else {
  "1 year"
}
date_frequencia
#-----------------------------------------------------------------------------------------
# grafico 1 mostra os fechamentos  historicos
#-----------------------------------------------------------------------------------------
if (length(dados_2) > 0 && !any(sapply(dados_2, nrow) == 0)) {
  titulo_grafico = paste("Preços de Fechamento:", paste(names(dados_2), collapse = " , "))
  
  plot_1 = ggplot(dados_combinados_2, aes(x = date, y = close, color = symbol)) +
    geom_line(size = 0.4, linetype = "solid") +  # Aumente o tamanho da linha e use linha sólida
    labs(title = titulo_grafico,
         subtitle = "Comparação dos Preços de Fechamento das Criptomoedas",
         x = "Data",
         y = "Preço de Fechamento (USD)",
         caption = "Fonte: Yahoo Finance") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#f7f7f7"),  # Cor de fundo do gráfico
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 18, color = "black"),  # Aumente o tamanho do título
      plot.subtitle = element_text(face = "italic", size = 14, color = "black"),  # Aumente o tamanho do subtítulo
      plot.caption = element_text(color = "gray"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      legend.title = element_text(color = "black"),
      legend.text = element_text(color = "black"),
      panel.grid.major = element_line(size = 0.2, color = "gray"),  # Estilo da grade
      panel.grid.minor = element_blank()  # Remova a grade menor
    ) +
    scale_x_date(date_labels = formato_data, date_breaks = date_frequencia) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black")) +
    guides(color = guide_legend(title = "Moeda")) +
    scale_color_manual(values = c("black", "red", "#B8F1B0", "#E3FCBF"))  # Definir cores manualmente
  
  # Converter o gráfico ggplot2 para um gráfico plotly interativo
  plot_1 = ggplotly(plot_1) %>%
    layout(plot_bgcolor = '#f7f7f7',  # Cor de fundo do gráfico
           paper_bgcolor = 'white',  # Cor de fundo do papel
           legend = list(bgcolor = 'white',
                         title = list(text = 'Moeda', font = list(color = 'black')), 
                         font = list(color = 'black')))
  plot_1
} else {
  print("Nenhum dado para plotar.")
}

#------------------------------------------------------------------------------------------------
#Volume de negociação por moeda;
#------------------------------------------------------------------------------------------------

dados_agrupados = aggregate(volume ~ symbol, data = dados_combinados_2, sum)

fig = plot_ly(dados_agrupados, labels = ~symbol, values = ~volume, type = 'pie', textinfo = 'label+percent',
              marker = list(colors = RColorBrewer::brewer.pal(n = 8, name = "Set1"))) %>%
  layout(title = 'Distribuição do Volume de Negociação por Símbolo',
         paper_bgcolor = '#f7f7f7',  # Define a cor de fundo do papel
         plot_bgcolor = 'rgba(0,0,0,1)',
         showlegend = TRUE,
         legend = list(font = list(color = 'black')),
         titlefont = list(color = 'black'))

fig
#------------------------------------------------------------------------------------------------
#Um gráfico de velas é útil para visualizar a variação de preços diária
#------------------------------------------------------------------------------------------------
plot_5 =  xts(dados_combinados_2[,c("open", "high", "low", "close")], order.by = as.Date(dados_combinados_2$date))
chartSeries(plot_5, type = "candlesticks", name = "ETH-USD Candlestick")

ggplotly(plot_5)
#------------------------------------------------------------------------------------------------
num_cols = sapply(dados_combinados_2, is.numeric)
df_corr = dados_combinados_2[, num_cols]
cor_matrix = cor(df_corr, use = "complete.obs") 
#REVER DEPOIS gráfico Calcular a matriz de correlação
# Crie uma visualização da matriz de correlação
corrplot(cor_matrix, method = "color", 
         type = "upper",  # Mostra apenas a metade superior da matriz
         tl.col = "black",  # Cor do texto dos rótulos
         tl.srt = 45,  # Rotação dos rótulos
         addCoef.col = "black",  # Cor dos coeficientes de correlação
         cl.pos = 'n',  # Posição da legenda de cores
         number.cex = .7,  # Tamanho do texto dos coeficientes
         insig = 'blank')  # Como tratar valores insignificantes


#------------------------------------------------------------------------------------------------
#CRIO UM DF PARA VISUALIZAR MELHOR
#------------------------------------------------------------------------------------------------
df_historico = dados_combinados_2 %>% 
        select(DATA= date,
               SIMBOLO =symbol,
               OPEN= open,
               HIGH=high,
               LOW =low,
               CLOSE =close,
               VOLUME =volume,
               ADJUSTED =adjusted)

view2 =head(df_historico,25)
view2
#------------------------------------------------------------------------------------------------
#APOS ALGUNS GREAFICOS  ANALISO SEMELHOR USNADO...
#------------------------------------------------------------------------------------------------
summary(df_historico) #VeRIFICAR OS QUARTIS ETC
df_historico$SIMBOLO = factor(df_historico$SIMBOLO) #TRANSFORMAR EM FACTOR POIS E CATEGORICA
sum(is.na(df_historico)) # VER SE NAO TEM VALORES NULOS

#------------------------------------------------------------------------------------------------
# sumarizando se uma maneira diferente em df 
#------------------------------------------------------------------------------------------------
# Data frame mensal
#------------------------------------------------------------------------------------------------
df_mensal=dados_combinados_2 %>%
  mutate(Month = floor_date(as.Date(date), "month")) %>%
  group_by(Month, symbol) %>%
  summarise(
    CLOSE_MEDIANA = median(close, na.rm = TRUE),
    CLOSE_MEDIA = mean(close, na.rm = TRUE),
    CLOSE_MIN = min(close, na.rm = TRUE),
    CLOSE_MAX = max(close, na.rm = TRUE)
  )
#------------------------------------------------------------------------------------------------
# Data frame semestral
#------------------------------------------------------------------------------------------------
df_semestral=dados_combinados_2 %>%
  mutate(Semetre = ifelse(month(date) <= 6, paste(year(date), "1 SEMESTRE"), paste(year(date), "2 SEMESTRE"))) %>%
  group_by(Semetre, symbol) %>%
  summarise(
    CLOSE_MEDIANA = median(close, na.rm = TRUE),
    CLOSE_MEDIA = mean(close, na.rm = TRUE),
    CLOSE_MIN = min(close, na.rm = TRUE),
    CLOSE_MAX = max(close, na.rm = TRUE)
  )
#------------------------------------------------------------------------------------------------
# Data frame anual
#------------------------------------------------------------------------------------------------
df_anual=dados_combinados_2 %>%
  mutate(Year = year(date)) %>%
  group_by(Year, symbol) %>%
  summarise(
    CLOSE_MEDIANA = median(close, na.rm = TRUE),
    CLOSE_MEDIA = mean(close, na.rm = TRUE),
    CLOSE_MIN = min(close, na.rm = TRUE),
    CLOSE_MAX = max(close, na.rm = TRUE)
  )
#------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------
#aqui escoho apenas um dado para fazer a regressao faço isso porque anterioremente consigo escolher
#uma lista te ticker somente para analisar uma coias versus a outra
#------------------------------------------------------------------------------------------------
ticker =unique(df_historico$SIMBOLO) 
print (ticker) 
ticker = 'BTC-USD'

df_regressa_logistica = df_historico %>%
                         filter(SIMBOLO ==ticker)
#--------------------------------------------------------------------------------------------------
#criando modelo de regressão logistica
#--------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#criando a variavel que indica se o close ou seja fechamento do mercado e maiior que o dia anterior variavel alvo
#------------------------------------------------------------------------------------------------
df_regressa_logistica= df_regressa_logistica %>%
  arrange(DATA) %>%
  mutate(real_preco_subiu = if_else(CLOSE > lag(CLOSE, default = first(CLOSE)), 1, 0))
 

set.seed(123)

num_cols = sapply(df_regressa_logistica, is.numeric)
df_corr = df_regressa_logistica[, num_cols]
cor_matrix = cor(df_corr, use = "complete.obs") 
#REVER DEPOIS gráfico Calcular a matriz de correlação
# Crie uma visualização da matriz de correlação
corrplot(cor_matrix, method = "color", 
         type = "upper",  # Mostra apenas a metade superior da matriz
         tl.col = "black",  # Cor do texto dos rótulos
         tl.srt = 45,  # Rotação dos rótulos
         addCoef.col = "black",  # Cor dos coeficientes de correlação
         cl.pos = 'n',  # Posição da legenda de cores
         number.cex = .7,  # Tamanho do texto dos coeficientes
         insig = 'blank')  # Como tratar valores insignificantes


#--------------------------------------------------------------------------------------------------
#SEPARANDO EM TREINO E TESTE 
#--------------------------------------------------------------------------------------------------
indices_treino  = createDataPartition(df_regressa_logistica$real_preco_subiu, p = 0.7, list = FALSE)
dados_treino    = df_regressa_logistica[indices_treino, ]
dados_teste     = df_regressa_logistica[-indices_treino, ]

# SERIA IDENAL FAZER A A NORMALIZACAO DOS DADOS.



#--------------------------------------------------------------------------------------------------
# Construir diferentes modelos excluindo variáveis preditoras para ver se melhora
#--------------------------------------------------------------------------------------------------
#modelo_logistico1 = glm(real_preco_subiu ~  CLOSE, data = dados_treino, family = binomial()) Muito 
#modelo_logistico1 = glm(real_preco_subiu ~ CLOSE + VOLUME, data = dados_treino, family = binomial()) #ruim
#modelo_logistico1 = glm(real_preco_subiu ~ retorno_diario + VOLUME, data = dados_treino, family = binomial())
#modelo_logistico1 = glm(real_preco_subiu ~ I(HIGH - LOW) + VOLUME, data = dados_treino, family = binomial())

#modelo_logistico1 = glm(real_preco_subiu ~ OPEN + HIGH + LOW + VOLUME, data = dados_treino, family = binomial())


modelo_logistico1 = glm(real_preco_subiu ~ CLOSE + LOW, data = dados_treino, family = binomial())
#--------------------------------------------------------------------------------------------------
#fazendo predicao do modelo para dasos de treino  
predict_treino = predict(modelo_logistico1, type = "response")
table_treino = table(dados_treino$real_preco_subiu, predict_treino >= 0.5)

# Calcular métricas para dados de treino
metricas_treino = calcular_metricas(table_treino)
print(metricas_treino)

#--------------------------------------------------------------------------------

# Previsão nos dados de teste
predict_teste = predict(modelo_logistico1, type = "response", newdata = dados_teste)
dados_teste$Previsao1 = as.numeric(predict_teste >= 0.5)

# Criar tabela de contingência para dados de teste
table_teste = table(dados_teste$real_preco_subiu, dados_teste$Previsao1)

# Calcular métricas para dados de teste
metricas_teste = calcular_metricas(table_teste)
print(metricas_teste)
#--------------------------------------------------------------------------------

##VALIDAR DEPOIS
roc_result = roc(dados_teste$real_preco_subiu, predict_teste)
auc_roc = auc(roc_result)
print(paste("AUC-ROC:", auc_roc))

# Plotando a Curva ROC  
plot(roc_result, main="Curva ROC",
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)


#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
dados_teste$Acertou = ifelse(dados_teste$real_preco_subiu == dados_teste$Previsao1, 'ACERTOU', 'ERROU')

# Criar um DataFrame com os resultados
resultado_df = data.frame(
  Data = dados_teste$DATA,
  SIMBOLO =dados_teste$SIMBOLO,
  Preco_Real_Subiu = dados_teste$real_preco_subiu,
  Previsao_Subiu = dados_teste$Previsao1,
  Acertou = dados_teste$Acertou
)


#--------------------------------------------------------------------------------
#teste finaldf rodar modelo para outra  base para testar o modelo
#--------------------------------------------------------------------------------
ticker = unique(df_historico$SIMBOLO) 
#print (ticker) 
ticker = 'BTC-USD'

df_historico_final = df_regressa_logistica

df_historico_final = df_regressa_logistica %>%
  filter (SIMBOLO==ticker)

#--------------------------------------------------------------------------------
#realizado a previsao
#--------------------------------------------------------------------------------
previsao_final = predict(modelo_logistico1, newdata = df_historico_final, type = "response")
df_historico_final$previsao_se_dia_hoje_maior_que_dia_anterior = as.numeric(previsao_final >= 0.5)

#--------------------------------------------------------------------------------
#mostro resultado se acertou ou errou 
#--------------------------------------------------------------------------------
df_historico_final = df_historico_final %>%
  arrange(DATA) %>%
  mutate(acertou = if_else(previsao_se_dia_hoje_maior_que_dia_anterior ==  real_preco_subiu, "ACERTOU", "ERROU"))

#--------------------------------------------------------------------------------
#ANALISE DE ERROS  PERCENTUAL DE ACERTO 
#--------------------------------------------------------------------------------
df_table = table(df_historico_final$acertou)
percentual_acertos = (df_table["ACERTOU"] / count(df_historico_final)) * 100
percentual_erros = (df_table["ERROU"] /count(df_historico_final)) * 100

percentual_acertos = as.numeric(percentual_acertos)
percentual_erros = as.numeric(percentual_erros)

df_grafico  =  data.frame(
  Categoria = c("Acertos", "Erros"),
  Percentual = c(percentual_acertos, percentual_erros)
)

str (df_grafico)
df_grafico$Percentual  =  as.numeric(as.character(df_grafico$Percentual))

df_grafico

#---------------------------------------------------------------------------

plot_ly(df_grafico, labels = ~Categoria, values = ~Percentual, type = 'pie',
        textinfo = 'label+percent', insidetextorientation = 'radial',
        marker = list(colors = c("#1E90FF", "#FF6347")),
        textfont = list(size = 18)) 
layout(title = "Percentual de Acertos e Erros do Modelo",
       showlegend = TRUE,
       legend = list(orientation = "h", x = 0.5, xanchor = 'center', y = -0.15),
       paper_bgcolor = '#FFFAFA',
       plot_bgcolor = '#FFFAFA') 
#---------------------------------------------------------------------------

##VALIDAR DEPOIS
roc_result = roc(df_historico_final$real_preco_subiu, previsao_final)
auc_roc = auc(roc_result)
print(paste("AUC-ROC:", auc_roc))

# Plotando a Curva ROC  
plot(roc_result, main="Curva ROC",
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grud=c(0.1,0.2),
     grid.col=c("green","red"), 
     max.auc.polygon=TRUE, 
     auc.polygon.col="lightgreen", 
     print.thres=TRUE)


#-----------------------------------------------------------------------
# MODELO SCRAPING EM PAGINA HTML 
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#**************PARAMETROS *****************
#-----------------------------------------------------------------------
 rm(DF_Y)
  
  tempo_inicio= Sys.time()
  tempo_total = 30 * 60  # EXECUTAR ATE QUAL TEMPO?
  
  # Defina o intervalo entre execuções (por exemplo, 5 EM 5 MINUTOS )
  intervalo= 1 * 60  # intervalod e 1  minutos 
  num_paginas = 1  ## offset=100 PARA PASSAR EM TODAS AS PAGINAS DO SITE
  count = 0 #contador de regitros 
  qtd_regitros_inseridos = 0 #contador de quantidade de registros
  
  
  #Inicialize listas para armazenar os dados de cada coluna
  todos_tickers = c()
  todos_nomes = c()
  todos_precos = c()
  todos_changes = c()
  todos_changes_percent = c()
  todos_market_caps = c()
  todos_volumes_since_utc = c()
  todos_volumes_24hr = c()
  todos_volumes_all = c()
  todos_circulating_supply = c()
  todos_data = c() 

#-----------------------------------------------------------------------
# TEM QUE ATIVAR O WHILE SE QUISER DEIXAR CAMPTURANDO O DADOS
#-----------------------------------------------------------------------
while (difftime(Sys.time(), tempo_inicio, units = "secs") < tempo_total) {
  
# CRIO UM dataframe, se ele ainda não existir
if (!exists("DF_Y")) {
  DF_Y = data.frame(
    DATA = as.POSIXct(character()),
    SIMBOLO = character(),
    NOME = character(),
    PRECO_ATUAL = numeric(),
    FECHAMENTO_ANTERIOR = numeric(),
    MUDANCA_VALOR = numeric(),
    MUDANCA_PERCENTUAL = numeric(),
    VALOR_MERCADO = character(),
    VOLUME_DESDE_UTC = character(),
    VOLUME_24HR = character(),
    VOLUME_TOTAL_NEGOCI_24HR = character(),
    QTD_MOEDA_EM_CIRCULACAO = character(),
    OPEN =numeric(),
    HIGH =  numeric(),
    LOW  =  numeric(),
    CLOSE = numeric() ,
    VOLUME =numeric(),
    ADJUSTED = numeric(),
    stringsAsFactors = FALSE  
  )
}
#-----------------------------------------------------------------------
##INICIO DO FOR PARA PERCORRER PAGINA HMTML E FILTRAR OS DADOS 
#-----------------------------------------------------------------------

for (i in seq(0, (num_paginas - 1) * 100, by = 100)) {
  url = sprintf("https://finance.yahoo.com/crypto/?count=100&offset=%d", i)
  pagina = read_html(url)
  
  tickers = pagina %>%
     html_nodes("a[data-test='quoteLink']") %>%
     html_text()
#  print (tickers)
  tickers = unique(tickers)
  nomes_moedas = pagina %>%
    html_nodes("td[aria-label='Name']") %>%
    html_text(trim = TRUE)
  nomes_moedas = unique (nomes_moedas)
  precos = pagina %>%
    html_nodes("fin-streamer[data-field='regularMarketPrice']") %>%
    html_text(trim = TRUE)
  #print (precos)
  changes = pagina %>%
    html_nodes("fin-streamer[data-field='regularMarketChange']") %>%
    html_text(trim = TRUE)
  #print (precos)
  changes_percent = pagina %>%
    html_nodes("fin-streamer[data-field='regularMarketChangePercent']") %>%
    html_text(trim = TRUE)
  #print (changes_percent)
  market_caps = pagina %>%
    html_nodes("fin-streamer[data-field='marketCap']") %>%
    html_text(trim = TRUE)
  #print (market_caps)
  volumes_since_utc = pagina %>%
    html_nodes("td[aria-label='Volume in Currency (Since 0:00 UTC)']") %>%
    html_text(trim = TRUE)
  #print (volumes_since_utc)
  volumes_24hr = pagina %>%
    html_nodes("td[aria-label='Volume in Currency (24Hr)']") %>%
    html_text(trim = TRUE)
  #print (volumes_24hr)
  volumes_all = pagina %>%
    html_nodes("td[aria-label='Total Volume All Currencies (24Hr)']") %>%
    html_text(trim = TRUE)
  #print (volumes_all)
  circulating_supply = pagina %>%
    html_nodes("td[aria-label='Circulating Supply']") %>%
    html_text(trim = TRUE)
 # print(circulating_supply)

  #data_formatada= ymd_hms(data_formatada) 
  
  data_formatada = format(Sys.time() , "%Y-%m-%d %H:%M:%S")# + un dia = 86400 = 
  todos_data = c(todos_data, rep(data_formatada, length(tickers)))
  todos_tickers = c(todos_tickers, tickers)
  todos_nomes = c(todos_nomes, nomes_moedas)
  todos_precos = c(todos_precos, precos)
  todos_changes = c(todos_changes, changes)
  todos_changes_percent = c(todos_changes_percent, changes_percent)
  todos_market_caps = c(todos_market_caps, market_caps)
  todos_volumes_since_utc = c(todos_volumes_since_utc, volumes_since_utc)
  todos_volumes_24hr = c(todos_volumes_24hr, volumes_24hr)
  todos_volumes_all = c(todos_volumes_all, volumes_all)
  todos_circulating_supply = c(todos_circulating_supply, circulating_supply)
  
  #PASSO O FILTRO PARA CONVERTER EM NUMEROS REMOVER CARACTERES ESPECIAIS
  precos_limpos = limpar_e_converter(precos)
  changes_limpos = limpar_e_converter(changes)
  changes_percent_limpos = limpar_e_converter(changes_percent, is_percent = TRUE)
  market_caps_limpos = limpar_e_converter(market_caps)
  volumes_since_utc_limpos = limpar_e_converter(volumes_since_utc)
  volumes_24hr_limpos = limpar_e_converter(volumes_24hr)
  volumes_all_limpos = limpar_e_converter(volumes_all)
  circulating_supply_limpos = limpar_e_converter(circulating_supply)
  
  #LIMPO O DATA FRAME PARA NAO SALVAR SUJEIRA
  dados_temp = distinct(dados_temp)
  rm(dados_temp)
  dados_temp = data.frame(
    DATA = rep(data_formatada, length(tickers)),
    SIMBOLO = (tickers),
    NOME = (nomes_moedas),
    PRECO_ATUAL = precos_limpos,
    FECHAMENTO_ANTERIOR = limpar_e_converter(as.numeric(precos_limpos) - as.numeric(changes_limpos)),
    MUDANCA_VALOR = changes_limpos,
    MUDANCA_PERCENTUAL = changes_percent_limpos,
    VALOR_MERCADO = market_caps_limpos,
    VOLUME_DESDE_UTC = volumes_since_utc_limpos,
    VOLUME_24HR = volumes_24hr_limpos,
    VOLUME_TOTAL_NEGOCI_24HR = volumes_all_limpos,
    QTD_MOEDA_EM_CIRCULACAO = circulating_supply_limpos,
    OPEN = 0,
    HIGH = 0, 
    LOW = 0,
    CLOSE =0,
    VOLUME = 0, 
    ADJUSTED= 0 ,
    stringsAsFactors = FALSE
    )

  dados_temp$DATA = as.POSIXct(dados_temp$DATA) 
  dados_temp$DATA = format(dados_temp$DATA,"%Y-%m-%d %H:%M:00")
  
  dados_temp = distinct(dados_temp)
  
  
  DF_Y = rbind(DF_Y, dados_temp)
  
  DF_Y$DATA = as.POSIXct(DF_Y$DATA) 
  DF_Y$DATA = format(DF_Y$DATA,"%Y-%m-%d %H:%M:00")
  
  
  DF_Y  =  DF_Y %>%
    mutate(
      DATA = as.POSIXct(DATA, format = "%Y-%m-%d %H:%M"),
      DATA_SOMENTE = as.Date(DATA)  
    ) %>%
    group_by(SIMBOLO, DATA_SOMENTE) %>%
    mutate(
      OPEN = first(FECHAMENTO_ANTERIOR),      # O primeiro 'FECHAMENTO_ANTERIOR' para cada data
      HIGH = max(PRECO_ATUAL, na.rm = TRUE),  # O maior preço atual para cada data
      LOW = min(PRECO_ATUAL, na.rm = TRUE),   # O menor preço atual para cada data
      CLOSE = last(PRECO_ATUAL),              # O primeiro 'FECHAMENTO_ANTERIOR' para cada data
      VOLUME = last(VOLUME_24HR ),            # A soma de 'VOLUME_24HR' para cada data
      ADJUSTED = last(PRECO_ATUAL), 
    ) %>%
    ungroup() %>%
    select(-DATA_SOMENTE)
  
  DF_Y = distinct(DF_Y)
}
  
  
  precos_limpos = limpar_e_converter(precos)
  changes_limpos = limpar_e_converter(changes)
  changes_percent_limpos = limpar_e_converter(changes_percent, is_percent = TRUE)
  market_caps_limpos = limpar_e_converter(market_caps)
  volumes_since_utc_limpos = limpar_e_converter(volumes_since_utc)
  volumes_24hr_limpos = limpar_e_converter(volumes_24hr)
  volumes_all_limpos = limpar_e_converter(volumes_all)
  circulating_supply_limpos = limpar_e_converter(circulating_supply)



colnames(DF_Y)


DF_Y = distinct(DF_Y)
DF_SQL = DF_Y

DF_SQL = distinct(DF_SQL)

#------------------------------------------------------------------------------------------------------
# altero nome das colunas momentanio pois quero salvar no bando de dados sql server 
#por seguranca e para fazer essa analise fururamente  e tranformando em dados historicos
#------------------------------------------------------------------------------------------------------
names(DF_SQL)= c( "DATA"
               ,"SIMBOLO" 
               ,"NOME"
               ,"PRECO_ATUAL"
               ,"FECHAMENTO_ANTERIOR"
               ,"MUDANCA_VALOR"           
               ,"MUDANCA_PERCENTUAL"
               ,"VALOR_MERCADO"
               ,"VOLUME_DESDE_UTC" 
               ,"VOLUME_24HR" 
               ,"VOLUME_TOTAL_NEGOCI_24HR" 
               ,"QTD_MOEDA_EM_CIRCULACAO" 
               ,"OPEN_DIARIO"
               ,"HIGH_DIARIO"  
               ,"LOW_DIARIO" 
               ,"CLOSE_DIARIO"
               ,"VOLUME_DIARIO"
               ,"ADJUSTED" )

#Capturo colunas para converter
colunas_para_converter = c("PRECO_ATUAL", "FECHAMENTO_ANTERIOR", "MUDANCA_VALOR", 
                            "MUDANCA_PERCENTUAL", "VALOR_MERCADO", "VOLUME_DESDE_UTC", 
                            "VOLUME_24HR", "VOLUME_TOTAL_NEGOCI_24HR", "QTD_MOEDA_EM_CIRCULACAO",
                            "OPEN_DIARIO", "HIGH_DIARIO", "LOW_DIARIO", "CLOSE_DIARIO",
                            "VOLUME_DIARIO", "ADJUSTED")

# Convertendo as colunas para numérico
for (coluna in colunas_para_converter) {
  DF_SQL[[coluna]] = as.numeric(gsub(",", ".", DF_SQL[[coluna]]))
}
#analize de validacao 
# Verificando se a coversao foi bem suicedida
str(DF_SQL)
summary(DF_SQL) 
sum(is.na(DF_SQL))
  
#faço um  insert no banco 
dbWriteTable(conexao_sql, "INTRADATE_DETAIL", DF_SQL, append = TRUE) 

    

#------------------------------------------------------------------------------------------------------
#volto par nomes originais  e removo o df para evitar fazer insert de dados duplicados.
#------------------------------------------------------------------------------------------------------
names(DF_SQL) = c( "DATA"
                 ,"SIMBOLO" 
                 ,"NOME"
                 ,"PRECO_ATUAL"
                 ,"FECHAMENTO_ANTERIOR"
                 ,"MUDANCA_VALOR"           
                 ,"MUDANCA_PERCENTUAL"
                 ,"VALOR_MERCADO"
                 ,"VOLUME_DESDE_UTC" 
                 ,"VOLUME_24HR" 
                 ,"VOLUME_TOTAL_NEGOCI_24HR" 
                 ,"QTD_MOEDA_EM_CIRCULACAO" 
                 ,"OPEN"
                 ,"HIGH"  
                 ,"LOW" 
                 ,"CLOSE"
                 ,"VOLUME"
                 ,"ADJUSTED" )

rm(DF_SQL)
#------------------------------------------------------------------------------------------------------
#crio um df para fazer teste somente com dados igual do yahoo finance
#------------------------------------------------------------------------------------------------------
  df_intradate=DF_Y %>%
    filter(SIMBOLO %in%lista_ticker) %>%
    select(
      DATA, #DATAmpra 
      SIMBOLO,#SIMBOLO
      OPEN, #OPEN
      HIGH,
      LOW,
      CLOSE , #CLOSE
      VOLUME ,
      ADJUSTED ,  #ADJUSTED COM DIVIDENDOS ETC MAS COMO NAO SABERMO USAREI O PREÇO ATUAL COMO ULTIMO FECHAMENTO
    )
#------------------------------------------------------------------------------------------------------
#analise exploratoria
#------------------------------------------------------------------------------------------------------
summary(df_intradate) 
sum(is.na(df_intradate))
#df1 = rbind(df_intradate, df_historico)
#------------------------------------------------------------------------------------------------------
#criando modelo de regressao logistica para prever se o valor vai subir analisandi de 5 em 5 minutos
#------------------------------------------------------------------------------------------------------


 count = count + 1
qtd_regitros_inseridos = qtd_regitros_inseridos + (num_paginas * 100) 

print(paste("qtd x passou :", count))
print(paste("qtd registros iseridos:", qtd_regitros_inseridos))

 Sys.sleep(intervalo)
}

#------------------------------------------------------------------------------------------------------  
#busco os dados do banco
#------------------------------------------------------------------------------------------------------
query  =  "
SELECT 
    DATA,
    SIMBOLO,
    NOME,
    PRECO_ATUAL,
    FECHAMENTO_ANTERIOR,
    MUDANCA_VALOR,
    MUDANCA_PERCENTUAL,
    VALOR_MERCADO,
    VOLUME_DESDE_UTC,
    VOLUME_24HR,
    VOLUME_TOTAL_NEGOCI_24HR,
    QTD_MOEDA_EM_CIRCULACAO,
    OPEN_DIARIO AS [OPEN], 
    HIGH_DIARIO AS [HIGH], 
    LOW_DIARIO AS [LOW], 
    CLOSE_DIARIO AS [CLOSE], 
    VOLUME_DIARIO AS [VOLUME], 
    ADJUSTED 
FROM INTRADATE_DETAIL 
WHERE SIMBOLO = 'BTC-USD'
"

 

dfsql_regressa_logistica_diario= dbGetQuery(conexao_sql, query)


dfsql_regressa_logistica_diario_validacao_final = dfsql_regressa_logistica_diario  
dfsql_regressa_logistica_diario  = unique(dfsql_regressa_logistica_diario)


dfsql_regressa_logistica_diario$QTD_MOEDA_EM_CIRCULACAO <- runif(
  n = nrow(dfsql_regressa_logistica_diario), 
  min = 19547000, 
  max = 20547000
)
#-----------------------------------------------------------------------------------
#criando uma matriz de correlaçao para identifciar melhores  features 
#-----------------------------------------------------------------------------------
num_cols = sapply(dfsql_regressa_logistica_diario, is.numeric)
df_corr = dfsql_regressa_logistica_diario[, num_cols]


cor_matrix = cor(df_corr, use = "complete.obs")  
corrplot(cor_matrix, method = "color")



# Visualizando a matriz de correlação com valores
cor_matrix = cor(df_corr, use = "complete.obs")
corrplot(cor_matrix,
         method = "color", 
         type = "upper",
         tl.col = "black",
         tl.srt = 90,
         addCoef.col = "black",
         cl.pos = 'n',
         number.cex = .7,
         insig = 'blank'
)

#-------------------------------------------------------------------------------------   
#CRIANDO UM DATA FRAME PARA FACILITAR ANALISE
#------------------------------------------------------------------------------------- 
tabela_matrix = melt(cor_matrix)
tabela_matrix = tabela_matrix[order(-abs(tabela_matrix$value)), ]
print(head(tabela_matrix, 100)) 

names(tabela_matrix) = c('variavel_1','variavel_2','correlacao')
 #------------------------------------------------------------------------------------------------
  #aqui escoho apenas um dado para fazer a regressao
  #------------------------------------------------------------------------------------------------
  #dfsql_regressa_logistica_diario = df_intradate %>%
  #         filter(SIMBOLO =='BTC-USD')
   str(dfsql_regressa_logistica_diario)
  
  #------------------------------------------------------------------------------------------------
  #criando a variavel que indica se o close ou seja fechamento do mercado e maiior que o dia anterior
  #------------------------------------------------------------------------------------------------
  dfsql_regressa_logistica_diario= dfsql_regressa_logistica_diario %>%
    arrange(DATA) %>%
    mutate(real_preco_subiu = if_else(CLOSE > lag(CLOSE, default = first(CLOSE)), 1, 0))
  

  #----------------------------------------------------------------------------------------------------
  set.seed(123)
  #--------------------------------------------------------------------------------------------------
  #SEPARANDO DADOS EM TREINO E TESTE
  #--------------------------------------------------------------------------------------------------
  indices_treino  = createDataPartition(dfsql_regressa_logistica_diario$real_preco_subiu, p = 0.7, list = FALSE)
  dados_treino    = dfsql_regressa_logistica_diario[indices_treino, ]
  dados_teste     = dfsql_regressa_logistica_diario[-indices_treino, ]
  
  #--------------------------------------------------------------------------------------------------
  # Construir diferentes modelos excluindo variáveis preditoras para ver se melhora
  #--------------------------------------------------------------------------------------------------
  #modelo_logistico1 = glm(real_preco_subiu ~ OPEN + HIGH + LOW + VOLUME, data = dados_treino, family = binomial())
  modelo_logistico1 = glm(real_preco_subiu ~ OPEN + HIGH + LOW + VOLUME + PRECO_ATUAL +
                            VALOR_MERCADO + VOLUME_DESDE_UTC + VOLUME_24HR + 
                            VOLUME_TOTAL_NEGOCI_24HR + QTD_MOEDA_EM_CIRCULACAO, data = dados_treino, family = binomial())
  
  
  
  #modelo_logistico2 = glm(real_preco_subiu ~ OPEN + HIGH + LOW, data = dados_treino, family = binomial())
  #--------------------------------------------------------------------------------------------------
  
  predict_treino = predict(modelo_logistico1, type = "response")
  table_treino = table(dados_treino$real_preco_subiu, predict_treino >= 0.7)
  
  # Calcular métricas para dados de treino
  metricas_treino = calcular_metricas(table_treino)
  print(metricas_treino)
  
  #--------------------------------------------------------------------------------
  
  # Previsão nos dados de teste
  predict_teste = predict(modelo_logistico1, type = "response", newdata = dados_teste)
  dados_teste$Previsao1 = as.numeric(predict_teste >= 0.7)
  
  # Criar tabela de contingência para dados de teste
  table_teste = table(dados_teste$real_preco_subiu, dados_teste$Previsao1)
  
  # Calcular métricas para dados de teste
  metricas_teste = calcular_metricas(table_teste)
  print(metricas_teste)
  
  
  
  roc_result = roc(dados_teste$real_preco_subiu, predict_teste)
  auc_roc = auc(roc_result)
  print(paste("AUC-ROC:", auc_roc))
  
  # Plotando a Curva ROC  
  plot(roc_result, main="Curva ROC",
       print.auc=TRUE, 
       auc.polygon=TRUE, 
       grud=c(0.1,0.2),
       grid.col=c("green","red"), 
       max.auc.polygon=TRUE, 
       auc.polygon.col="lightblue", 
       print.thres=TRUE)

#-------------------------------------------------------------------------------------  
#GRAFICO MATRIZ DE CORRELAÇÃO 
#-------------------------------------------------------------------------------------  
  # Calculando a matriz de correlação sem valores
  num_cols = sapply(dfsql_regressa_logistica_diario, is.numeric)
  df_corr = dfsql_regressa_logistica_diario[, num_cols]

  cor_matrix = cor(df_corr, use = "complete.obs")  # 'use' lida com missing values
    corrplot(cor_matrix, method = "color")
  
  

# Visualizando a matriz de correlação com valores
  cor_matrix = cor(df_corr, use = "complete.obs")
  corrplot(cor_matrix,
           method = "color", 
           type = "upper",
           tl.col = "black",
           tl.srt = 90,
            addCoef.col = "black",
           cl.pos = 'n',
           number.cex = .7,
                      insig = 'blank'
           )

  #--------------------------------------------------------------------------------
  # analise de resultados
  #--------------------------------------------------------------------------------
  
  # Identificar acertos
  dados_teste$Acertou = dados_teste$real_preco_subiu == dados_teste$Previsao1
  
  # Criar um DataFrame com os resultados
  resultado_df = data.frame(
    Data = dados_teste$DATA,
    SIMBOLO =dados_teste$SIMBOLO,
    NOME =dados_teste$NOME,
    Preco_Real_Subiu = dados_teste$real_preco_subiu,
    Previsao_Subiu = dados_teste$Previsao1,
    Acertou = dados_teste$Acertou
  )
  
  #------------------------------------------------------------------------------------------------
  # Validação Cruzada Tesntando
  #------------------------------------------------------------------------------------------------
  
  num_folds  = 5
  folds  =  createFolds(dfsql_regressa_logistica_diario$real_preco_subiu, k = num_folds, list = TRUE)
  
  accuracy_results  =     numeric(num_folds)
  sensitivity_results  =  numeric(num_folds)
  specificity_results  =  numeric(num_folds)
  
  for(i in seq_along(folds)) {
    training_indices  =  folds[[i]]
    testing_indices  =  setdiff(seq_len(nrow(dfsql_regressa_logistica_diario)), training_indices)
    training_data  =  dfsql_regressa_logistica_diario[training_indices, ]
    testing_data  =  dfsql_regressa_logistica_diario[testing_indices, ]
    
    modelo_logistico_cv = glm(real_preco_subiu ~ OPEN + HIGH + LOW + VOLUME + PRECO_ATUAL +
                                VALOR_MERCADO + VOLUME_DESDE_UTC + VOLUME_24HR + 
                                VOLUME_TOTAL_NEGOCI_24HR + QTD_MOEDA_EM_CIRCULACAO,
                              data = training_data, family = binomial())
    
    
    predictions  =  predict(modelo_logistico_cv, newdata = testing_data, type = "response")
    predicted_classes  =  ifelse(predictions > 0.5, 1, 0)
    
    conf_matrix  =  table(testing_data$real_preco_subiu, predicted_classes)
    metricas  =  calcular_metricas(conf_matrix)
    accuracy_results[i]  =  metricas$acuracia
    sensitivity_results[i]  =  metricas$sensibilidade
    specificity_results[i]  =  metricas$especificidade
  }
  
  mean_accuracy  =  mean(accuracy_results)
  mean_sensitivity  =  mean(sensitivity_results)
  mean_specificity  =  mean(specificity_results)
  
  print(paste("Acurácia média da validação cruzada:", mean_accuracy))
  print(paste("Sensibilidade média da validação cruzada:", mean_sensitivity))
  print(paste("Especificidade média da validação cruzada:", mean_specificity))
  
  df_metricas = data.frame(c(mean(accuracy_results)),
                           c(mean(sensitivity_results)),
                           c(mean(specificity_results) )
                             )  
  
   names(df_metricas) = c('ACURACIA','SENSIBILIDADE','ESPECIFICIDADE')
  
  

  
  #Teste Final

#Rodar o modelo para base original
   dfsql_regressa_logistica_diario_validacao_final = dfsql_regressa_logistica_diario
  dfsql_regressa_logistica_diario_validacao_final = dfsql_regressa_logistica_diario_validacao_final #%>%
    #filter (SIMBOLO==ticker)
  #rm(dfsql_regressa_logistica_diario_validacao_final)
  
  #faço a predicao usnado o modelo
  previsao_final = predict(modelo_logistico1, newdata = dfsql_regressa_logistica_diario_validacao_final, type = "response")
  dfsql_regressa_logistica_diario_validacao_final$previsao_se_tempo_maior_que_tempo_anterior = as.numeric(previsao_final >= 0.5)
  
  
  #crio um coluna falando se acertou ou erro 
  dfsql_regressa_logistica_diario_validacao_final = dfsql_regressa_logistica_diario_validacao_final %>%
    arrange(DATA) %>%
    mutate(acertou = if_else(previsao_se_tempo_maior_que_tempo_anterior == real_preco_subiu, "ACERTOU", "ERROU"))
  #--------------------------------------------------------------------------------
  #ANALISE DE ERROS PERCENTUAL DE ACERTO 
  #--------------------------------------------------------------------------------
  df_table = table(dfsql_regressa_logistica_diario_validacao_final$acertou)
  percentual_acertos  = (df_table["ACERTOU"] / count(dfsql_regressa_logistica_diario_validacao_final)) * 100
  percentual_erros  = (df_table["ERROU"] /count(dfsql_regressa_logistica_diario_validacao_final)) * 100
  
  
  
  percentual_acertos = as.numeric(percentual_acertos)
  percentual_erros = as.numeric(percentual_erros)
  #--------------------------------------------------------------------------------
  #GERANDO GRAFICO DE PIZZA PARA MOSTRA ERROS X ACERTOS
  #-------------------------------------------------------------------------------- 
  df_grafico  =  data.frame(
    Categoria = c("Acertos", "Erros"),
    Percentual = c(percentual_acertos, percentual_erros)
  )
  
  str (df_grafico)

  df_grafico$Percentual  =  as.numeric(as.character(df_grafico$Percentual))
  
  # Criando o gráfico de pizza diretamente com plotly
  plot_ly(df_grafico, labels = ~Categoria, values = ~Percentual, type = 'pie',
          textinfo = 'label+percent', insidetextorientation = 'radial',
          marker = list(colors = c("#FF6600", "#FFFF00")),
          textfont = list(size = 18)) %>%  # Ajustando o tamanho da fonte aqui
    layout(title = "Percentual de Acertos e Erros do Modelo",
           showlegend = TRUE,
           legend = list(orientation = "h", x = 0.5, xanchor = 'center', y = -0.15),
           paper_bgcolor = '#FFFAFA',
           plot_bgcolor = '#FFFAFA') 
  
  
  
  
  DF_FIM = dfsql_regressa_logistica_diario_validacao_final %>%
    select(DATA,SIMBOLO,NOME,PRECO_ATUAL,FECHAMENTO_ANTERIOR,
           REAL_PREVISAO =real_preco_subiu
           ,VALOR_MERCADO_SUBIU = previsao_se_tempo_maior_que_tempo_anterior,
           MODELO_ACERTOU =acertou
           )
    
  DF_FIM
  
  
  
  
  
  
  
  
  
  

