# Análise de dataset de vendas de carros usando estatística básica

# Definindo a pasta de trabalho
# Substitua o caminho abaixo pela pasta no seu computador inverta a "\" por "/"
setwd("Cole o caminho aqui")
getwd()

# Carregando o dataset (renomeio o arquivo para o nome "Vendas" e atribuindo à variável "vendas"
vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252")

# Analisando o dataset
# Exibindo a tabela
View(vendas)

# Resumo das variáveis da tabela
str(vendas)

# Resumo estatístico da tabela
summary(vendas$Valor)

# Resumo estatístico da coluna custo
summary(vendas$Custo)

# Cabeçalho (10 primeiras linhas)
head(vendas)

# Final (10 últimas linhas)
tail(vendas)

# Média 
mean(vendas$Valor)
mean(vendas$Custo)

# Média Ponderada
weighted.mean(vendas$Valor, w = vendas$Custo)

# Mediana
median(vendas$Valor)
median(vendas$Custo)

# Moda
# Criando uma função para obter a moda
moda <- function(v) {
  valor_unico <- unique(v)
  valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

# Obtendo a Moda
resultado <- moda(vendas$Valor)
print(resultado)

resultado_custo <- moda(vendas$Custo)
print(resultado_custo)

# Criando gráfico de Média de Valor Por Estado com ggplot2
install.packages("ggplot2")
library(ggplot2)

# Cria o gráfico
ggplot(vendas) + 
  stat_summary(aes(x = Estado, 
                   y = Valor),
               fun = mean, 
               geom = "bar", 
               fill = "lightgreen", 
               col = "grey50") +
  labs(title = "Média de valor por Estado") +

# Variância
var(vendas$Valor)

# Desvio Padrão
sd(vendas$Valor)

# Medidas de Tendência Central
summary(vendas$Valor)
summary(vendas[c('Valor', 'Custo')])

# Explorando variáveis numéricas
mean(vendas$Valor) 
median(vendas$Valor)
# Calculando os quartis
quantile(vendas$Valor)
quantile(vendas$Valor, probs = c(0.01, 0.99))
quantile(vendas$Valor, seq(from = 0, to = 1, by = 0.20))
IQR(vendas$Valor) # Diferença entre Q3 e Q1
range(vendas$Valor)
summary(vendas$Valor)
diff(range(vendas$Valor))
