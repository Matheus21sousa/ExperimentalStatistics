## AULA 03 - Delineamentos Inteiramente Casualizados

## ---- 
# Exemplo:
#   Título: Mutagênese em alho com radiação gama
#   Fontes de variação: Dose da radiação (Gy)
#   Variável resposta: Altura de plantas

## ----
## PACOTES UTILIZADOS
library(MASS)
library(here)
library(ggplot2)
library(readxl)
library(ExpDes.pt)
library(lawstat)

## ----
# Definição de caminhos utilizando o pacote "here"
dirRaw <- here("RawData")
dirProcessed <- here("DIC")

# Lê o arquivo em formato Excel
pathRaw <- file.path(dirRaw, "DIC-garlic.xlsx")
DICgarlic <- read_excel(pathRaw)

# Seed para aleatorizações (n° USP)
set.seed(15380462)

# Aleatorização das parcelas experimentais
SampleDIC <- DICgarlic[sample(nrow(DICgarlic)), ]

# Cria um dataframe com linhas (n = x) e (n = y) colunas associarmos ao DICgarlic
Linha <- rep(1:7, each = 4)
Coluna <- rep(1:4, times = 7)
Arranjo <- data.frame(LINHA = Linha, COLUNA = Coluna)
SampleDIC <- cbind(SampleDIC, Arranjo)

# Transforma a dose em um fator 
SampleDIC$DOSE <- as.factor(SampleDIC$DOSE)

# Plota o croqui da área
croquiDIC <- ggplot(SampleDIC, aes(x = LINHA, y = COLUNA, fill = DOSE)) +
  geom_tile(color = "white", lwd = 1) +
  geom_text(aes(label = REPETIÇÃO), color = "white", size = 4) +
  scale_color_continuous() +  # Usar uma escala contínua
  scale_x_continuous(breaks = unique(SampleDIC$LINHA), labels = unique(SampleDIC$LINHA),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(SampleDIC$COLUNA), labels = unique(SampleDIC$COLUNA),
                     expand = c(0, 0)) +
  labs(
    x = "Coluna", 
    y = "Linha", 
    title = "DIC Alho | Croqui", 
    fill = "Raios Gama (Gy)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # Ajustar a posição dos rótulos do eixo x
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # Ajustar a posição dos rótulos do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
)
print(croquiDIC)

## ----
## ANÁLISE EXPLORATÓRIA

# Transforma a dose em um fator 
DICgarlic$DOSE <- as.factor(DICgarlic$DOSE)

# Gráfico de pontos
ggplot(DICgarlic, aes(x = DOSE, y = ALTURA)) +
  geom_point() +
  expand_limits(y = 0) +
  labs(
    x = "Dose (Gy)", 
    y = "Altura (cm)", 
    title = "DIC Alho | Diagrama de Dispersão") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Gráfico boxplot
ggplot(DICgarlic, aes(x = DOSE, y = ALTURA)) +
  geom_boxplot() +
  expand_limits(y = 0) +
  labs(
    x = "Dose (Gy)", 
    y = "Altura (cm)", 
    title = "DIC Alho | BoxPlot") +
  theme(
    plot.title = element_text(hjust = 0.5))

## ----
## ANÁLISE DE HOMOCEDASTICIDADE E NORMALIDADE

# Análise de Variância
aovDIC = aov(ALTURA ~ DOSE, DICgarlic)
anova(aovDIC)
plot(aovDIC) 

# Modelo linear
lmDIC = lm(ALTURA~DOSE, DICgarlic)
resDIC <- residuals(lmDIC)           # Resíduos 
resStudDIC <- rstandard(lmDIC)       # Resíduos studentizados

# Levene | Teste de homogeneidade de variâncias
with(DICgarlic,
  levene.test(ALTURA, DOSE, location = "mean"))

# Shapiro-Wilk | Teste de normalidade dos resíduos
shapiro.test(resStudDIC)

# Gráfico Q-Qplot
qqnorm(resStudDIC, col = 1)
qqline(resStudDIC, col = 2)

## ----
## TRANSFORMAÇÃO DOS DADOS

# Gráfico Box-Cox - Avaliação do lambda
with(DICgarlic, {
       plot_boxcox <- boxcox(ALTURA ~ DOSE, 
                             ylab = "Logaritmo da verossimilhança")
       title(main = "DIC Alho | Gráfico Box-Cox")
       print(plot_boxcox)
     })

# Transformação de dados p/ lambda = 0
DICgarlic$ALTURAt <- log(DICgarlic$ALTURA)

# Modelo linear após transformação
lmDICt = lm(ALTURAt~DOSE, DICgarlic)
resDICt <- residuals(lmDICt)           # Resíduos 
resStudDICt <- rstandard(lmDICt)       # Resíduos studentizados

# Levene | Teste de homogeneidade de variâncias após transformação
with(DICgarlic,
     levene.test(ALTURAt, DOSE, location = "mean"))

# Shapiro-Wilk | Teste de normalidade dos resíduos após transformação
shapiro.test(resStudDICt)

# Gráfico Q-Qplot | Respíduos transformados
qqnorm(resStudDICt, col = 1)
qqline(resStudDICt, col = 2)

# Gráfico Box-Cox | Validação da transformação - Lambda = 1 pertence ao intervalo de confiança
with(DICgarlic, {
  plot_boxcox <- boxcox(ALTURAt ~ DOSE, 
                        ylab = "Logaritmo da verossimilhança")
  title(main = "DIC Alho | Gráfico Box-Cox após transformação")
  print(plot_boxcox)
})

## ----
## ANÁLISE ESTATÍSTICA

# Análise de Variância
anova(lmDICt)

# DIC
DICgarlic$DOSE <- as.numeric(DICgarlic$DOSE)
with(DICgarlic, 
     dic(DOSE, ALTURAt, hvar = "levene", quali = F, mcomp = "tukey", sigF = 0.05, sigT = 0.05))