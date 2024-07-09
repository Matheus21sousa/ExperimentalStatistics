## AULA 06 - Delineamentos Inteiramente Casualizados - Fatores Quantitativos

## ----
# Exemplo:
#   Título: Desempenho de genótipos de batata submetidos a doses de radiação gama
#   Fontes de variação: Dose de radiação gama (Gy)
#   Variável resposta: Peso de tubérculos (kg) colhidos por parcela (7 plantas)

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

DICpII <- read_xlsx("DIC-potatoII.xlsx", col_names = T)

## ----
## CROQUI

# Aleatorização das parcelas experimentais
set.seed(1704)
SampleDICpII <- DICpII[sample(nrow(DICpII)), ]

# Cria um dataframe com linhas (n = x) e (n = y) colunas associarmos ao DICpotato
Linha <- rep(1:8, each = 6)
Coluna <- rep(1:6, times = 8)
Arranjo <- data.frame(LINHA = Linha, COLUNA = Coluna)
SampleDICpII <- cbind(SampleDICpII, Arranjo)

# Transforma dose em um fator 
SampleDICpII$Dose <- as.factor(SampleDICpII$Dose)

# Paleta do RColorBrewer
paleta <- brewer.pal(6, "Blues")
display.brewer.all()

# Plota o croqui da área
croquiDICpII <- ggplot(SampleDICpII, aes(x = LINHA, y = COLUNA, fill = Dose)) +
  geom_tile(color = "white", lwd = 1) +
  geom_text(aes(label = Repetition), color = "black", size = 4) +
  scale_fill_manual(values = paleta) +  
  scale_x_continuous(breaks = unique(SampleDICpII$LINHA), labels = unique(SampleDICpII$LINHA),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(SampleDICpII$COLUNA), labels = unique(SampleDICpII$COLUNA),
                     expand = c(0, 0)) +
  labs(
    x = "Coluna", 
    y = "Linha", 
    title = "DIC Batata II | Croqui", 
    fill = "Dose") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # Ajustar a posição dos rótulos do eixo x
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # Ajustar a posição dos rótulos do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
print(croquiDICpII)

## ----
## ANÁLISE EXPLORATÓRIA

# Transforma dose em um fator 
DICpII$Dose <- as.factor(DICpII$Dose)

# Gráfico de pontos
ggplot(DICpII, aes(x = Dose, y = pTub)) +
  geom_point() +
  expand_limits(y = 0) +
  labs(
    x = "Genótipo", 
    y = "Peso de tubérculos colhidos (kg)", 
    title = "DIC Batata II | Gráfico de Pontos") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Gráfico boxplot
ggplot(DICpII, aes(x = Dose, y = pTub)) +
  geom_boxplot() +
  expand_limits(y = 0) +
  labs(
    x = "Genótipo", 
    y = "Peso de tubérculos colhidos (kg)", 
    title = "DIC Batata II | BoxPlot") +
  theme(
    plot.title = element_text(hjust = 0.5))

## ----
## ANÁLISE DE HOMOCEDASTICIDADE E NORMALIDADE

# Análise de Variância
aovDICpII = aov(pTub ~ Dose, DICpII)
anova(aovDICpII)
plot(aovDICpII) 

# Modelo linear
lmDICpII = lm(pTub~Dose, DICpII)
resDICpII <- residuals(lmDICpII)           # Resíduos 
resStudDICpII <- rstandard(lmDICpII)       # Resíduos studentizados

# Levene | Teste de homogeneidade de variâncias
with(DICpII,
     levene.test(pTub, Dose, location = "mean"))

# Shapiro-Wilk | Teste de normalidade dos resíduos
shapiro.test(resStudDICpII)

# Gráfico Q-Qplot
qqnorm(resStudDICp, col = 1)
qqline(resStudDICp, col = 2)

## ----
## ANÁLISE ESTATÍSTICA

# Análise de Variância
anova(lmDICpII)

# DIC
DICpII$Dose <- as.numeric(DICpII$Dose)
with(DICpII, 
     dic(Dose, pTub, hvar = "levene", quali = F, sigF = 0.05, sigT = 0.05))
