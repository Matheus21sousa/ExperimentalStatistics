## AULA 10 - Experimentos fatoriais

## ----
# Exemplo:
#   Título: Combinação de acessos de tomate com porta-enxertos populares
#   Fontes de variação:
#       F.V.01: Acessos de tomate do laboratório de melhoramento genético de hortaliças como enxertos
#       F.V.02: Porta-enxertos com resistências conhecidas e compatibilidade com tomate
#   Variável resposta: Diâmetro de frutos comerciais

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
FatDBCt <- read_xlsx("FatDBC-tomato.xlsx", col_names = T)

# Aleatorização das parcelas experimentais 
set.seed(1704)
sFatDBCt <- FatDBCt[sample(nrow(FatDBCt)), ]

# Para que cada bloco contenha um número igual de colunas, esse foi colocado em ordem mas os genótipos já foram aleatorizados;
sFatDBCt <- sFatDBCt[order(sFatDBCt$Repetition), ]

# Assim, criamos um dataframe com as colunas para associarmos ao fatDBCt
Coluna <- rep(1:24, times = 4)
Arranjo <- data.frame(col = Coluna)
sFatDBCt <- cbind(sFatDBCt, Arranjo)

# Paleta do RColorBrewer
paleta <- brewer.pal(9, "Reds")[2:5]
display.brewer.all()

# Plota o croqui da área
croquiDBCtomato <- ggplot(sFatDBCt, aes(x = col, y = 1, fill = Rootstock)) +
  geom_tile(color = "white",lwd = 1) +
  facet_grid(Repetition ~ ., switch = "y", labeller = labeller(
    Repetition = c("1" = "Bloco 01", "2" = "Bloco 02", "3" = "Bloco 03", "4" = "Bloco 04"))) +
  geom_text(aes(label = Genotype), color = "black", size = 4, angle = 90) +
  scale_fill_manual(values = paleta) +
  scale_x_continuous(breaks = unique(sFatDBCt$col), labels = unique(sFatDBCt$col),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(sFatDBCt$row), labels = unique(sFatDBCt$row),
                     expand = c(0, 0)) +
  labs(
    x = NULL, 
    y = NULL, 
    title = "Fatorial DBC Tomate | Croqui",
    fill = "Porta-Enxerto") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
print(croquiDBCtomato)

## ----
## ANÁLISE EXPLORATÓRIA

# Cria uma coluna que concatena o nome das duas fontes de variação
FatDBCt <- FatDBCt %>%
  mutate(Treatment = paste(Rootstock, Genotype, sep = "-"))

# Transforma colunas com tratamentos em fatores
FatDBCt$Treatment <- as.factor(FatDBCt$Treatment)
FatDBCt$Genotype <- as.factor(FatDBCt$Genotype)
FatDBCt$Rootstock <- as.factor(FatDBCt$Rootstock)

# Calcula a média de dFruto
meanDF <- mean(FatDBCt$dFruto)

# Gráfico de pontos geral
ggplot(FatDBCt, aes(x = Treatment, y = dFruto)) +
  geom_point() +
  geom_hline(yintercept = meanDF, color = "red", linetype = "dashed") +
  expand_limits(y = 0) +
  scale_y_continuous(limits = c(110, 270), breaks = seq(110, 270, by = 20)) +
  coord_cartesian(ylim = c(100, 270)) +
  labs(
    x = "Genótipo", 
    y = "Diâmetro médio de fruto (mm)", 
    title = "Fatorial DBC Tomate | Gráfico de Pontos") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(hjust = 0.5))

# Gráfico BoxPlot geral
ggplot(FatDBCt, aes(x = Treatment, y = dFruto)) +
  geom_boxplot() +
  geom_hline(yintercept = meanDF, color = "red", linetype = "dashed") +
  expand_limits(y = 0) +
  scale_y_continuous(limits = c(110, 270), breaks = seq(110, 270, by = 20)) +
  coord_cartesian(ylim = c(100, 270)) +
  labs(
    x = "Genótipo", 
    y = "Diâmetro médio de fruto (mm)", 
    title = "Fatorial DBC Tomate | Gráfico BoxPlot") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(hjust = 0.5))

# Gráfico de linha: Porta-enxerto
ggplot(FatDBCt, aes(x = Rootstock, y = dFruto)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1)) +
  scale_y_continuous(limits = c(110, 270), breaks = seq(110, 270, by = 10)) +
  coord_cartesian(ylim = c(160, 240)) +
  labs(
    x = "Porta-enxerto", 
    y = "Diâmetro médio de fruto (mm)", 
    title = "Fatorial DBC Tomate | Gráfico de Linha") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Gráfico de linha: Genotipo
ggplot(FatDBCt, aes(x = Genotype, y = dFruto)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1)) +
  scale_y_continuous(limits = c(110, 270), breaks = seq(110, 270, by = 10)) +
  coord_cartesian(ylim = c(160, 240)) +
  labs(
    x = "Genótipo", 
    y = "Diâmetro médio de fruto (mm)", 
    title = "Fatorial DBC Tomate | Gráfico de Linha") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Paleta do RColorBrewer
paleta01 <- brewer.pal(9, "Reds")[4:7]

# Gráfico de linhas: Estratificação por Genótipo
ggplot(FatDBCt, aes(x = Genotype, y = dFruto, group = Rootstock, color = Rootstock)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  scale_y_continuous(limits = c(110, 270), breaks = seq(110, 270, by = 10)) +
  coord_cartesian(ylim = c(160, 240)) +
  scale_color_manual(values = paleta01) +
  labs(
    x = "Genótipo", 
    y = "Diâmetro médio de fruto (mm)", 
    title = "Gráfico de linhas | Estratificação por Genótipo") +
  theme(
    plot.title = element_text(hjust = 0.5))

# Paleta do RColorBrewer
paleta02 <- brewer.pal(9, "Reds")[4:9]

#Gráfico de linhas: Estratificação por Porta-enxerto
ggplot(FatDBCt, aes(x = Rootstock, y = dFruto, group = Genotype, color = Genotype)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  scale_y_continuous(limits = c(110, 270), breaks = seq(110, 270, by = 10)) +
  coord_cartesian(ylim = c(160, 240)) +
  scale_color_manual(values = paleta02) +
  labs(
    x = "Porta-Enxerto", 
    y = "Diâmetro médio de fruto (mm)", 
    title = "Gráfico de linhas | Estratificação por Porta-enxerto") +
  theme(
    plot.title = element_text(hjust = 0.5))

## ----
## ANÁLISE DE HOMOCEDASTICIDADE E NORMALIDADE

# Modelo para análise
mFatDBC = lm(dFruto ~ Repetition + Genotype*Rootstock, FatDBCt)
resFatDBC <- residuals(mFatDBC)                    # Resíduos 
resStudFatDBC <- rstandard(mFatDBC)                # Resíduos studentizados

# Breusch-Pagan | Teste de homogeneidade de variâncias
bptest(mFatDBC)

# Shapiro-Wilk | Teste de normalidade dos resíduos
shapiro.test(resStudFatDBC)

# Box-Cox | Verificação do lambda
BoxCox <- boxcox(mFatDBC, 
                 lambda = seq(-1, 3, 0.01),
                 ylab = "Logaritmo da verossimilhança")
title(main = "Fatorial DBC Tomate | Gráfico Box-Cox")

# Gráfico Q-Qplot
qqnorm(resStudFatDBC, col = 1)
qqline(resStudFatDBC, col = 2)

## ----
## ANÁLISE ESTATÍSTICA

# Fatorial DBC

FatDBCt$Genotype <- as.character(FatDBCt$Genotype)
FatDBCt$Rootstock <- as.character(FatDBCt$Rootstock)

with(FatDBCt, 
     fat2.dbc(Genotype,
              Rootstock,
              Repetition,
              dFruto,
              quali = c(TRUE, TRUE), 
              fac.names = c("Genótipo", "Porta-Enxerto"),
              sigF = 0.05, 
              sigT = 0.05))

## ----
## TUKEY BY ROOTSTOCK

# Calcula a média dos valores de dFruto por porta-enxerto
meanDFroot <- FatDBCt %>%
  group_by(Rootstock) %>%
  summarise(meanDF = mean(dFruto, na.rm = TRUE))

# Cria um dataframe com o resultado do teste Tukey; 
TukeyRoot <- tibble(
  Rootstock = c("Beaufort", "Emperador", "Multifort", "Robusta"),
  tTest = c("a", "ab", "a", "b")
)

# Adiciona os grupos Tukey às médias 
meanDFroot <- left_join(meanDFroot, TukeyRoot, by="Rootstock")

# Paleta do RColorBrewer
pTukeyRoot <- brewer.pal(9, "Reds")[3:6]

# Gráfico de Barras 
ggplot(meanDFroot, aes(x = Rootstock, y = meanDF, fill = Rootstock)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.6) +
  geom_text(
    aes(label = tTest), vjust = -0.5, color = "black") +
  scale_y_continuous(
    limits = c(0, 240), breaks = seq(150, 240, by = 10)) +
  coord_cartesian(ylim = c(160, 230)) +
  scale_fill_manual(values = pTukeyRoot) +
  theme_light() +
  labs(title = "Fat DBC Tomate | Tukey p/ Porta-Enxerto",
       x = "Porta-Enxerto",
       y = "Diâmetro médio de fruto (mm)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),                 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  

## ----
## TUKEY BY GENOTYPE

# Calcula a média dos valores de dFruto por genótipo
meanDFgeno <- FatDBCt %>%
  group_by(Genotype) %>%
  summarise(meanDF = mean(dFruto, na.rm = TRUE))

# Cria um dataframe com o resultado do teste Tukey; 
TukeyGeno <- tibble(
  Genotype = c("Sl-031", "Sl-057", "Sl-060", "Sl-062", "Sl-098", "Sl-112"),
  tTest = c("cd", "b", "bc", "d", "bcd", "a")
)

# Adiciona os grupos Tukey às médias 
meanDFgeno <- left_join(meanDFgeno, TukeyGeno, by="Genotype")

# Paleta do RColorBrewer
pTukeyGeno <- brewer.pal(9, "Reds")[4:9]

# Gráfico de Barras 
ggplot(meanDFgeno, aes(x = Genotype, y = meanDF, fill = Genotype)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.6) +
  geom_text(
    aes(label = tTest), vjust = -0.5, color = "black") +
  scale_y_continuous(
    limits = c(0, 240), breaks = seq(150, 240, by = 10)) +
  coord_cartesian(ylim = c(160, 230)) +
  scale_fill_manual(values = pTukeyGeno) +
  theme_light() +
  labs(title = "Fat DBC Tomate | Tukey p/ Genótipo",
       x = "Genótipo",
       y = "Diâmetro médio de fruto (mm)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),                 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  
