## Pacotes
library(readr)
library(ExpDes.pt)
library(ggplot2)
library(MASS)
library(lawstat)
library(lmtest)

## Leitura dos dados
data <- read_csv2("Psub-DBC.csv", col_names = T)

## ----
## ANÁLISE EXPLORATÓRIA

# Modelo linear
modeloPsub <- lm(BotaoFloral~Repeticao + Genotipo*Dose, data)
resPsub <- residuals(modeloPsub)                                                # Resíduos 
resStudPsub <- rstandard(modeloPsub)                                          # Resíduos studentizados

# QQplot
QQplot <- qqnorm(resStudPsub, col = 1) +
  qqline(resStudPsub, col = 2)

# Shapiro-Wilk | Teste de normalidade dos resíduos
shapiro.test(resPsub)

# Breusch-Pagan | Teste de homogeneidade de variâncias
bptest(modeloPsub)

# Box-Cox
plot_boxcox <- boxcox(modeloPsub, ylab = "Logaritmo da verossimilhança")
title(main = "Gráfico Box-Cox")

## ----
## TRANSFORMAÇÃO DE DADOS

# Transformação de dados p/ lambda = 0.5
data$BotaoFloralT <- sqrt((data$BotaoFloral)+5)

# Modelo linear após transformação
modeloPsubT <- lm(BotaoFloralT~Repeticao + Dose*Genotipo, data)
resPsubT <- residuals(modeloPsubT)
resStudPsubT <- rstandard(modeloPsubT)

# QQplot | Resíduos transformados
QQplot <- qqnorm(resStudPsubT, col = 1) +
  qqline(resStudPsub, col = 2)

# Shapiro-Wilk | Teste de normalidade dos resíduos
shapiro.test(resPsubT)

# Breusch-Pagan | Teste de homogeneidade de variâncias
bptest(modeloPsubT)

# Box-Cox
plot_boxcox <- boxcox(modeloPsubT, ylab = "Logaritmo da verossimilhança")
title(main = "Gráfico Box-Cox após transformação sqrt(y+0.5)")

## ----
## ANÁLISE ESTATÍSTICA

# Fatorial duplo em DBC
with(data,
     fat2.dbc(
       Genotipo,
       Dose,
       Repeticao,
       BotaoFloralT,
       quali = c(TRUE, FALSE),
       mcomp = "tukey",
       fac.names = c("Genotipo", "Dose"),
       sigT = 0.05,
       sigF = 0.05,
       unfold = NULL
     ))

## Parcela Sub-dividida em DBC
# with(data,
#   psub2.dbc(
#     data$Genotipo,
#     data$Dose,
#     data$Repeticao,
#     data$BotaoFloral,
#     quali = c(TRUE, FALSE),
#     fac.names = c("Genótipo", "Dosagem"),
#     unfold = NULL)
# )

## ----
## REGRESSÃO DA DOSE

## Dados não transformados | Regressores para modelo quadrático [x²] 
data$BF2ajustado <- 42.1119 + 0.1801 * data$Dose - 0.0002 * data$Dose^2
## Dados transformados | Regressores para modelo quadrático [x²] 
data$BFT2ajustado <- 6.2425 + 0.0127 * data$Dose - 0.00001 * data$Dose^2

## Dados não transformados | Regressores para modelo cúbico [x³]
data$BF3ajustado <- 0.0042 + 0.6911 * data$Dose - 0.0014 * data$Dose^2 + 0.000001 * data$Dose^3
## Dados transformados | Regressores para modelo cúbico [x³]
data$BFT3ajustado <- 0.0004 + 0.0646 * data$Dose - 0.0001 * data$Dose^2 + 0.00000 * data$Dose^3

# Equação da regressão e R²
equation <- "y = 42.1119 + 0.1801*x - 0.0002*x²"
rSquare <- paste0("R² = 0.55")

# Crie o gráfico com ggplot2
ggplot(data, aes(x = Dose, y = BotaoFloralT)) +
  geom_point(color = 'black', size = 1) + 
  geom_line(aes(y = BFT2ajustado), color = 'red4', size = 0.5) + 
  labs(title = "Regressão quadrática | Resposta do botão floral em função das doses",
       x = "Dose",
       y = "Botão Floral Transformado") +
  annotate("text", x = 820, y = 180, label = equation, size = 3, color = "black", parse = F) + 
  annotate("text", x = 700, y = 170, label = rSquare, size = 3, color = "black")
