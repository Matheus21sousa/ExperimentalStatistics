## AULA 11 - Experimentos fatoriais

## ----
# Exemplo:
#   Título: Uso da radiação gama como promotora de mutagênese sobre os parâmetros produtivos de genótipos comerciais de alho
#   Fontes de variação:
#       F.V.01: Doses de radiação Gama (Gy)
#       F.V.02: Genótipos de alho
#   Variável resposta: Peso médio de bulbo

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
FatDICg <- read_xlsx("FatDIC-garlic.xlsx", col_names = T)

# Seed para aleatorizações
set.seed(2001)

# Aleatorização das parcelas experimentais
sFatDICg <- FatDICg[sample(nrow(FatDICg)), ]

# Cria um dataframe com linhas (n = x) e (n = y) colunas associarmos ao FatDICg
Linha <- rep(1:6, each = 8)
Coluna <- rep(1:8, times = 6)
Arranjo <- data.frame(row = Linha, col = Coluna)
sFatDICg <- cbind(sFatDICg, Arranjo)

# Transforma as fontes de variação em fator 
sFatDICg$Genotype <- as.factor(sFatDICg$Genotype)
sFatDICg$Dose <- as.factor(sFatDICg$Dose)

# Paleta do RColorBrewer
paleta <- brewer.pal(9, "Greys")[4:7]
display.brewer.all()

# Cria uma nova coluna para servir como a label da var. Dose
sFatDICg$DoseName <- paste(FatDICg$Dose, "Gy", sep = " ")

# Plota o croqui da área
croquiFat <- ggplot(sFatDICg, aes(x = col, y = row, fill = Genotype)) +
  geom_tile(color = "white",lwd = 1) +
  geom_text(aes(label = DoseName), color = "white", size = 4) +
  scale_fill_manual(values = paleta) +
  scale_x_continuous(breaks = unique(sFatDICg$col), labels = unique(sFatDICg$col),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(sFatDICg$row), labels = unique(sFatDICg$row),
                     expand = c(0, 0)) +
  labs(
    x = "Coluna", 
    y = "Linha", 
    title = "Fatorial DIC Alho | Croqui",
    fill = "Genótipo") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
print(croquiFat)

## ----
## ANÁLISE DE HOMOCEDASTICIDADE E NORMALIDADE

# Modelo para análise
mFatDIC = lm(pBulbo ~ Genotype*Dose, FatDICg)
resFatDIC <- residuals(mFatDIC)                    # Resíduos 
resStudFatDIC <- rstandard(mFatDIC)                # Resíduos studentizados

# Breusch-Pagan | Teste de homogeneidade de variâncias
bptest(mFatDIC)

# Shapiro-Wilk | Teste de normalidade dos resíduos
shapiro.test(resStudFatDIC)

# Box-Cox | Verificação do lambda
BoxCox <- boxcox(mFatDIC, 
                 lambda = seq(-1, 3, 0.01),
                 ylab = "Logaritmo da verossimilhança")
title(main = "Fatorial DIC Alho | Gráfico Box-Cox")

# Gráfico Q-Qplot
qqnorm(resStudFatDIC, col = 1)
qqline(resStudFatDIC, col = 2)

## ----
## ANÁLISE ESTATÍSTICA

# Fatorial DIC
with(FatDICg, 
     fat2.dic(Genotype,
              Dose,
              pBulbo,
              quali = c(TRUE, F), 
              fac.names = c("Genótipo", "Dose"),
              sigF = 0.05, 
              sigT = 0.05))
