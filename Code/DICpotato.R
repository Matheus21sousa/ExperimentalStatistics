## AULA 03 e 05 - DIC e Comparações Múltiplas

## ----
# Exemplo:
#   Título: Capacidade de multiplicação de genótipos de batata
#   Fontes de variação: Acessos de Solanum tuberosum
#   Variável resposta: Número de tubérculos

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

## ----
## SIMULAÇÃO DE DADOS

# Seed para aleatorizações (6 primeiros n° USP)
set.seed(2001)

# Genótipos e repetições
genotype <- c("Asterix", "St57", "St91", "St346", "St467", "St614")
repetition <- rep(1:8, each = 1)

# Dataframe para armazenar os dados de genotipo e repetição
df1 <- data.frame(
  Genotype = rep(genotype, each = 8),
  Repetition = repetition
)

# # Gerar nTub para cada combinação de genótipo e repetição
# nTub <- simulation(length(genotype) * length(1:8), media = 32, desvio = 15)

# Tamanho de grupos 
grupo1 <- c("Asterix", "St57")
grupo2 <- c("St91")
grupo3 <- c("St346", "St467", "St614")

# Função para gerar valores de nTub entre 1 e 63 com distribuição normal
simulation <- function(n, media, desvio) {
  contagem <- round(rnorm(n, mean = media, sd = desvio))
  contagem <- pmax(pmin(contagem, 63), 1)
  return(contagem)
}

# Gerar nTub para os dois grupos
nTub1 <- simulation(length(grupo1) * 8, media = 44.7, desvio = 12)
nTub2 <- simulation(length(grupo2) * 8, media = 39.5, desvio = 14)
nTub3 <- simulation(length(grupo3) * 8, media = 29.1, desvio = 9)

# Cria um dataframe para combinar os 3 grupos de distribuição do número de tubérculos
df2 <- data.frame(
  nTub = c(nTub1, nTub2, nTub3)
)

# Combina o dataframe com dados de genótipo + repetições com o dataframe contendo o número de tubérculos
DICpotato <- bind_cols(df1, df2)

write.csv(DICpotato, file = "DIC-potato.csv")

## ----
## ALEATORIZAÇÃO DAS PARCELAS E CROQUI

# Aleatorização das parcelas experimentais
set.seed(2001)
SampleDICp <- DICpotato[sample(nrow(DICpotato)), ]

# Cria um dataframe com linhas (n = x) e (n = y) colunas associarmos ao DICgarlic
Linha <- rep(1:8, each = 6)
Coluna <- rep(1:6, times = 8)
Arranjo <- data.frame(LINHA = Linha, COLUNA = Coluna)
SampleDICp <- cbind(SampleDICp, Arranjo)

# Transforma o genótipo em um fator 
SampleDICp$Genotype <- as.factor(SampleDICp$Genotype)

# Paleta do RColorBrewer
paleta <- brewer.pal(6, "Greens")
display.brewer.all()

# Plota o croqui da área
croquiDICp <- ggplot(SampleDICp, aes(x = LINHA, y = COLUNA, fill = Genotype)) +
  geom_tile(color = "white", lwd = 1) +
  geom_text(aes(label = Repetition), color = "black", size = 4) +
  scale_fill_manual(values = paleta) +  # Usar uma escala contínua
  scale_x_continuous(breaks = unique(SampleDICp$LINHA), labels = unique(SampleDICp$LINHA),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(SampleDICp$COLUNA), labels = unique(SampleDICp$COLUNA),
                     expand = c(0, 0)) +
  labs(
    x = "Coluna", 
    y = "Linha", 
    title = "DIC Batata | Croqui", 
    fill = "Genótipo") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # Ajustar a posição dos rótulos do eixo x
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # Ajustar a posição dos rótulos do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
print(croquiDICp)

## ----
## ANÁLISE EXPLORATÓRIA

# Transforma o genótipo em um fator 
DICpotato$Genotype <- as.factor(DICpotato$Genotype)

# Gráfico de pontos
ggplot(DICpotato, aes(x = Genotype, y = nTub)) +
  geom_point() +
  expand_limits(y = 0) +
  labs(
    x = "Genótipo", 
    y = "Número de tubérculos", 
    title = "DIC Batata | Gráfico de Pontos") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Gráfico boxplot
ggplot(DICpotato, aes(x = Genotype, y = nTub)) +
  geom_boxplot() +
  expand_limits(y = 0) +
  labs(
    x = "Genótipo", 
    y = "Número de tubérculos", 
    title = "DIC Batata | BoxPlot") +
  theme(
    plot.title = element_text(hjust = 0.5))

## ----
## ANÁLISE DE HOMOCEDASTICIDADE E NORMALIDADE

# Análise de Variância
aovDICp = aov(nTub ~ Genotype, DICpotato)
anova(aovDICp)
plot(aovDICp) 

# Modelo linear
lmDICp = lm(nTub~Genotype, DICpotato)
resDICp <- residuals(lmDICp)           # Resíduos 
resStudDICp <- rstandard(lmDICp)       # Resíduos studentizados

# Levene | Teste de homogeneidade de variâncias
with(DICpotato,
     levene.test(nTub, Genotype, location = "mean"))

# Shapiro-Wilk | Teste de normalidade dos resíduos
shapiro.test(resStudDICp)

# Gráfico Q-Qplot
qqnorm(resStudDICp, col = 1)
qqline(resStudDICp, col = 2)

## ----
## ANÁLISE ESTATÍSTICA

# Análise de Variância
anova(lmDICp)

# DIC
DICpotato$Genotype <- as.character(DICpotato$Genotype)
with(DICpotato, 
     dic(Genotype, nTub, hvar = "levene", quali = T, mcomp = "tukey", sigF = 0.05, sigT = 0.05))
