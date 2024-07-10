## AULA 08 - Delineamentos em Blocos Casualizados 

## ----
# Exemplo:
#   Título: Desempenho produtivo de diferentes acessos (genótipos) de Solanum lycopersicum 
#   Fontes de variação: Genótipos de tomate diferentes
#   Variável resposta: Peso total de frutos comerciais (kg) colhidos por parcela (6 plantas)

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

## ----
## ENTRADA DE DADOS, ALEATORIZAÇÃO DAS PARCELAS E CROQUI

# Entrada de dados
DBCtomato <- read_xlsx("DBC-tomato.xlsx", col_names = T)

# Aleatorização das parcelas experimentais 
set.seed(1704)
SampleDBCtomato <- DBCtomato[sample(nrow(DBCtomato)), ]

# Para que cada bloco contenha um número igual de colunas, esse foi colocado em ordem mas os genótipos já foram aleatorizados;
SampleDBCtomato <- SampleDBCtomato[order(SampleDBCtomato$Bloco), ]

# Assim, criamos um dataframe com as colunas para associarmos ao DBCtomato
Coluna <- rep(1:6, times = 5)
Arranjo <- data.frame(COLUNA = Coluna)
SampleDBCtomato <- cbind(SampleDBCtomato, Arranjo)

# Transforma dose em um fator 
SampleDBCtomato$Genotype <- as.factor(SampleDBCtomato$Genotype)

# Paleta do RColorBrewer
paleta <- brewer.pal(9, "Purples")[4:9]
display.brewer.all()

# Determinando os intervalos para os marcadores do eixo x e y
intervalos_x <- seq(1, 6, by = 1)
intervalos_y <- seq(1, 5, by = 1)

# Plota o croqui da área
croquiDBCtomato <- ggplot(SampleDBCtomato, aes(x = COLUNA, y = 1, fill = Genotype)) +
  geom_tile(color = "white",lwd = 1) +
  facet_grid(Bloco ~ ., switch = "y", labeller = labeller(
    Bloco = c("1" = "Bloco 01", "2" = "Bloco 02", "3" = "Bloco 03", "4" = "Bloco 04", "5" = "Bloco 05"))) +
  geom_text(aes(label = Genotype), color = "white", size = 4) +
  scale_fill_manual(values = paleta) +  
  scale_y_reverse(breaks = intervalos_y) +
  scale_x_continuous(breaks = intervalos_x) +
  labs(
    x = NULL, 
    y = NULL, 
    title = "DBC Tomate | Croqui", 
    fill = NULL) +
  theme_light() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  # Ajustar a posição dos rótulos do eixo x
    axis.text.y = element_blank(),                                    # Retira o rótulo do eixo y
    axis.ticks.y = element_blank(),                                   # Retira o marcador de rótiulo do eixo y
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
print(croquiDBCtomato)

## ----
## ANÁLISE EXPLORATÓRIA

# Transforma dose em um fator 
DBCtomato$Genotype <- as.factor(DBCtomato$Genotype)

# Gráfico de pontos
ggplot(DBCtomato, aes(x = Genotype, y = pFruto)) +
  geom_point() +
  expand_limits(y = 0) +
  labs(
    x = "Genótipo", 
    y = "Peso de frutos comerciais (kg/parcela)", 
    title = "DBC Tomate | Gráfico de Pontos") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Gráfico boxplot
ggplot(DBCtomato, aes(x = Genotype, y = pFruto)) +
  geom_boxplot() +
  expand_limits(y = 0) +
  labs(
    x = "Genótipo", 
    y = "Peso de frutos comerciais (kg/parcela)", 
    title = "DBC Tomate | BoxPlot") +
  theme(
    plot.title = element_text(hjust = 0.5))

## ----
## ANÁLISE DE HOMOCEDASTICIDADE E NORMALIDADE

# Análise de Variância
aovDBCtomato = aov(pFruto ~ Genotype, DBCtomato)
anova(aovDBCtomato)
plot(aovDBCtomato) 

# Modelo linear
lmDBCtomato = lm(pFruto~Bloco+Genotype, DBCtomato)
resDBCtomato <- residuals(lmDBCtomato)           # Resíduos 
resStudDBCtomato <- rstandard(lmDBCtomato)       # Resíduos studentizados

# Breusch-Pagan | Teste de homogeneidade de variâncias
bptest(lmDBCtomato)

# Shapiro-Wilk | Teste de normalidade dos resíduos
shapiro.test(resStudDBCtomato)

# Gráfico Q-Qplot
qqnorm(resStudDICp, col = 1)
qqline(resStudDICp, col = 2)

## ----
## ANÁLISE ESTATÍSTICA

# Análise de Variância
anova(lmDBCtomato)

# DBC
DBCtomato$Genotype <- as.character(DBCtomato$Genotype) 
with(DBCtomato, 
     dbc(Genotype, 
         Bloco, 
         pFruto, 
         hvar = "oneillmathews", 
         quali = T, 
         mcomp = "tukey", 
         sigF = 0.05, 
         sigT = 0.05))

## ----
## Gráfico com resultado do Tukey

# Calcula a média dos valores de pFruto por genótipo
meanDBCt <- DBCtomato %>%
  group_by(Genotype) %>%
  summarise(meanHarvest = mean(pFruto, na.rm = TRUE))

# Cria um dataframe com o resultado do teste Tukey; 
Tukey <- tibble(
  Genotype = c("Sl-031", "Sl-057", "Sl-060", "Sl-062", "Sl-098", "Sl-112"),
  tTest = c("bcd", "ab", "abc", "d", "cd", "a")
)

# Adiciona os grupos Tukey às médias 
meanDBCt <- left_join(meanDBCt, Tukey, by="Genotype")

# Paleta de cores definidas no gráfico do croqui (armazenada na var. "paleta")
# Gráfico de Barras 
ggplot(meanDBCt, aes(x = Genotype, y = meanHarvest, fill = Genotype)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = tTest), vjust = -0.5, color = "black") +
  scale_fill_manual(values = paleta) +
  theme_light() +
  labs(title = "DBC Tomate | Gráfico de Barras + Tukey",
       x = "Genótipo",
       y = "Produção média de frutos por parcela (kg)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),                 # Deixa em ângulo o nome dos genótipos
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  # Centraliza o título
