## AULA x - Experimentos fatoriais

## ----
# Exemplo:
#   Título: Capacidade de multiplicação de genótipos de batata
#   Fontes de variação: Acessos de Solanum tuberosum
#   Variável resposta: Número de tubérculos

## ----
## PACOTES UTILIZADOS
library(here)
library(dplyr)
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(dae)
library(MASS)          #Plota o BoxCox
library(lawstat)       #Testes de normalidade e homocedasticidade 
library(lmtest)        #Teste de homocedasticidade Breusch-Pagan
library(ExpDes.pt)     #Análise de experimentos em múltiplos delineamentos

## ----
## ENTRADA DE DADOS

# Dados
FatDBC <- read_excel("FatDBC-soybean.xlsx", col_names = T)

# Tranformação em fatores
FatDBC$Tratanento <- as.factor(FatDBC$Tratanento)
FatDBC$Stress <- as.factor(FatDBC$Stress)
FatDBC$Time <- as.factor(FatDBC$Time)
FatDBC$Rep <- as.factor(FatDBC$Rep)

## ----
## ANÁLISE EXPLORATÓRIA

# Sumarização e análise exploratória
summary(FatDBC)

# Gráfico de pontos
ggplot(FatDBC, aes(x = Tratanento, y = Var)) +
  geom_point() +
  expand_limits(y = 0) +
  labs(
    x = "Tratamento", 
    y = "Var. resposta", 
    title = "Fatorial DBC | Gráfico de Pontos") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Gráfico boxplot
ggplot(FatDBC, aes(x = Tratanento, y = Var)) +
  geom_boxplot() +
  expand_limits(y = 0) +
  labs(
    x = "Tratamento", 
    y = "Var. resposta", 
    title = "Fatorial DBC | BoxPlot") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Modelos
modelo <- lm(Var~Rep + Stress*Time, data = FatDBC)
res_Stud <- rstandard(modelo)

# QQ-plot | Dispersão dos dados
qqnorm(res_Stud)
qqline(res_Stud, col=2)

# Shapiro-Wilk | Teste de normalidade
shapiro.test(res_Stud)

# Breusch-Pagan | Teste de homogeneidade de variâncias
bptest(modelo)

# Dispersão das variâncias
ggplot(FatDBC, aes(x=fitted(modelo), y=res_Stud))+
  geom_point()

# BoxCox | Verifica o intervalo de confiança do lambda
boxcox(modelo)

## ----

# Tranformação em fatores
FatDBC$Tratanento <- as.numeric(FatDBC$Tratanento)
FatDBC$Stress <- as.numeric(FatDBC$Stress)
FatDBC$Time <- as.numeric(FatDBC$Time)
FatDBC$Rep <- as.numeric(FatDBC$Rep)

# ExpDes.pt ANOVA 
with(FatDBC,
  fat2.dbc(
    Stress,
    Time, 
    Rep,
    Var,
    quali = c(F,T),
    mcomp = "tukey",
    fac.names = c("Water", "Time"),
    sigT = 0.05,
    sigF = 0.05,
    unfold = NULL
  )   
)
