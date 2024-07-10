## AULA 09 - Delineamento em Quadrado Latino

## ----
# Exemplo:
#   Título: Mutagênese em batata com radiação gama
#   Fontes de variação: Dose da radiação (Gy)
#   Variável resposta: Peso de tubérculos comerciais produzidos na parcela (7 plantas)

## ----
## PACOTES UTILIZADOS
library(MASS)
library(here)
library(ggplot2)
library(readxl)
library(dplyr)
library(ExpDes.pt)
library(lawstat)
library(RColorBrewer)
library(lmtest)
library(agricolae)

## ---- 
## ENTRADA DE DADOS, ALEATORIZAÇÃO DAS PARCELAS E CROQUI

# Data Frame único para o agricolae gerar o croqui
Gama <- c("0Gy", "2Gy", "4Gy", "6Gy", "8Gy")

# Gera a matriz com o delineamento
croqui <- design.lsd(Gama, serie = 2, seed = 1704)
print(croqui)

# Armazena apenas a matriz do delineamento na variável plano
plano <- croqui$book

# Paleta do RColorBrewer
paleta <- brewer.pal(9, "BrBG")[1:5]
display.brewer.all()

# Plota o croqui da área
croquiDQL <- ggplot(plano, aes(x = col, y = row, label = Gama, fill = Gama)) +
  geom_tile(color = "white",lwd = 1) +
  geom_text(aes(label = Gama), color = "black", size = 4) +
  scale_fill_manual(values = paleta) +
  labs(
    x = "Coluna", 
    y = "Linha", 
    title = "DQL Batata | Croqui",
    fill = "Radiação") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # Ajustar a posição dos rótulos do eixo x
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
print(croquiDQL)

## ----
## ANÁLISE EXPLORATÓRIA

# Entrada de dados
DQLpotato <- read_xlsx("DQL-potato.xlsx", col_names = T)

# Vamos associar as posições do croqui com o conjunto de dados
plano <- plano[order(plano$Gama), ]                              # Ordena o croqui pela dose, pareando com o conjunto de dados
DQLpotato <- cbind(DQLpotato, plano)                             # Unifica a tabela com posições do croqui com os dados amostrais
DQLpotato <- subset(DQLpotato, select = -c(plots, Gama))         # Remove colunas não necessárias

# Transforma dose em um fator 
DQLpotato$Dose <- as.factor(DQLpotato$Dose)

# Gráfico de pontos
ggplot(DQLpotato, aes(x = Dose, y = pTub)) +
  geom_point() +
  expand_limits(y = 0) +
  labs(
    x = "Dose de radiação (Gy)", 
    y = "Peso de tubérculos comerciais (kg)", 
    title = "DQL Batata | Gráfico de Pontos") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Gráfico boxplot
ggplot(DQLpotato, aes(x = Dose, y = pTub)) +
  geom_boxplot() +
  expand_limits(y = 0) +
  labs(
    x = "Dose de radiação (Gy)", 
    y = "Peso de tubérculos comerciais (kg)", 
    title = "DQL Batata | BoxPlot") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Gráfico em escala de produção
ggplot(DQLpotato, aes(x = col, y = row, label = Name, fill = pTub)) +
  geom_tile(color = "white",lwd = 1) +
  geom_text(aes(label = Name), color = "black", size = 4) +
  scale_fill_gradient(low = "#F1E7C3", high = "#7D5119") +
  geom_text() +
  labs(
    x = "Linha", 
    y = "Coluna", 
    title = "DQL Batata | Produção",
    fill = "Produção (Kg)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # Ajustar a posição dos rótulos do eixo x
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
)

## ----
## ANÁLISE DE HOMOCEDASTICIDADE E NORMALIDADE

# Modelo para análise
mDQL = aov(pTub ~ row+col+Dose, DQLpotato)
resDQL <- residuals(mDQL)                    # Resíduos 
resStudDQL <- rstandard(mDQL)                # Resíduos studentizados

# Breusch-Pagan | Teste de homogeneidade de variâncias
bptest(mDQL)

# Shapiro-Wilk | Teste de normalidade dos resíduos
shapiro.test(resStudDQL)

# Box-Cox | Verificação do lambda
BoxCox <- boxcox(mDQL, 
              lambda = seq(-1, 2, 0.01),
              ylab = "Logaritmo da verossimilhança")
title(main = "DQL Batata | Gráfico Box-Cox")

# Transformação de dados p/ lambda = -1 
DQLpotato$pTubT <- 1 / (DQLpotato$pTub + 0.5)

# Modelo para análise para dados após transformação
mDQLt = aov(pTubT ~ row+col+Dose, DQLpotato)
resDQLt <- residuals(mDQLt)                    # Resíduos 
resStudDQLt <- rstandard(mDQLt)                # Resíduos studentizados

# Breusch-Pagan | Teste de homogeneidade de variâncias após transformação
bptest(mDQLt)

# Shapiro-Wilk | Teste de normalidade dos resíduos após transformação
shapiro.test(resStudDQLt)

# Box-Cox | Verificação do lambda após transformação
tBoxCox <- boxcox(mDQLt, 
              lambda = seq(-1, 2, 0.01),
              ylab = "Logaritmo da verossimilhança")
title(main = "DQL Batata | Gráfico Box-Cox após transformação")


# Gráfico Q-Qplot
qqnorm(resStudDQLt, col = 1)
qqline(resStudDQLt, col = 2)

## ----
## ANÁLISE ESTATÍSTICA

# Análise de Variância
anova(mDQLt)

# DQL
DQLpotato$Dose <- as.numeric(as.character(DQLpotato$Dose))
with(DQLpotato, 
     dql(Dose,
         row, 
         col, 
         pTubT,
         quali = F, 
         sigF = 0.05, 
         sigT = 0.05))

## ----
## REGRESSÃO

# Modelo ajustado pelos regressores quadráticos e com a retransfomação dos dados 
functionDQL <- function(x){
  (1/(0.2310 - 0.0204*x + 0.0068*x^2)) - 0.5
}

# Gráfico da regressão com dados de altura originais
ggplot(DQLpotato, aes(x=Dose, y = pTub)) +
  stat_summary(fun = mean, geom = "point", color = "red3") +
  stat_function(fun = functionDQL, color = "black", size = 0.8) +
  expand_limits(y = 1) +
  scale_x_continuous(breaks = seq(min(DQLpotato$Dose), max(DQLpotato$Dose), by = 2)) +
  labs(
    x = "Dose (Gy)", 
    y = "Peso de tubérculos (Kg)", 
    title = "DQL Batata | Regressão polinomial (x²)") +
  theme(
    plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 5, y = 4.6, label = "y = 1/(0.2310 - 0.0204x + 0.0068x²)) - 0.5", 
           color = "blue4", size = 3, hjust = 0, vjust = 4)
