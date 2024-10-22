## AULA 12 - Experimentos em parcelas subdivididas

## ----
# Exemplo:
#   Título: Preparo de solo e diferentes adubos na qualidade de tubérculos de batata inglesa
#   Fontes de variação:
#       F.V.01: Preparo de Solo (Parcela)
#       F.V.02: Tipo de adubo utilizado (Sub-parcela). 
#   Variável resposta: Teor de sólidos totais 

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

# Entrada de dados
PsubDBC <- read_xlsx("Psub-potato.xlsx", col_names = T)

# Índices para i, k, j
I = 3       # Nº de parcelas
K = 3       # Nº de sub-parcelas
J = 4       # Nº de repetições/blocos

# Criação de um vetor para os níveis de estudo
Psolo = c("Grade Pesada", "Grade Leve", "Cultivo Mínimo")     # Parcelas
Adubo = c("Orgânico", "Mineral", "Organomineral")             # Sub-parcelas

# Função de sorteio para o delineamento Par. Sub. do Agricolae
(Plano <- design.split(
  Psolo,
  Adubo, 
  r = J,
  design = "rcbd",
  serie = 0
))

# Extraí apenas o delineamento sorteado
(Plano <- Plano$book)

# Extrai as posições relativas de cada fator
with(Plano, table(block, Psolo))
with(Plano, table(block, Adubo))
with(Plano, table(block, Psolo, Adubo))

# Criação de uma função que abrevia o nome das fontes de variação
Sigla <- function(Psolo, Adubo) {
  # Abreviações dos Adubos
  sAdubo <- ifelse(Adubo == "Orgânico", "Orga",
                   ifelse(Adubo == "Mineral", "Mine",
                          ifelse(Adubo == "Organomineral", "OrMi", NA)))
  
  # Abreviações dos Preparos de Solo
  sPsolo <- ifelse(Psolo == "Grade Pesada", "GP",
                   ifelse(Psolo == "Grade Leve", "GL",
                          ifelse(Psolo == "Cultivo Mínimo", "CM", NA)))
  # Combina as abreviações
  paste(sPsolo, sAdubo, sep = " + ")
}

# Associa as abreviações ao data frame
Plano <- Plano %>%
  mutate(Sigla = mapply(Sigla, Psolo, Adubo))

# Paleta do RColorBrewer
paleta <- brewer.pal(9, "BrBG")[4:6]
display.brewer.all()

# Plota o croqui da área
ggplot(Plano, aes(x= rep(1:I, each = K, times = J), y = splots, fill = Psolo)) +
  geom_tile(color="black") +
  geom_text(aes(label = Sigla), color = "black", size = 4, angle = 90) +
  facet_wrap(~ block, ncol = 4, labeller = labeller(                                      # ncol = 4 -> todos blocos em uma linha
    block = c("1" = "Bloco 01", "2" = "Bloco 02", "3" = "Bloco 03", "4" = "Bloco 04"))) +
  scale_fill_manual(values = paleta) +
  labs(
    x = NULL, 
    y = NULL, 
    title = "Parcelas Subdivididas DBC | Croqui",
    fill = "Preparo de Solo") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),  
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
)

## ----
## ANÁLISE DE MODELO

# Modelo para análise
mPsubDBC = aov(Tsolidos ~ Rep + Psolo*Adubo + Error(Rep:Psolo/Adubo), data = PsubDBC)
summary(mPsubDBC)

## ----
## ANÁLISE ESTATÍSTICA

# Parcelas subdivididas em DBC
with(PsubDBC,
     psub2.dbc(
       Psolo,
       Adubo, 
       Rep,
       Tsolidos,
       quali = c(T,T),
       mcomp = "tukey",
       fac.names = c("Preparo de solo", "Adubação"),
       sigT = 0.05,
       sigF = 0.05,
       unfold = NULL
     )   
)

## ----
## TUKEY BY SOIL TILLAGE

# Calcula a média dos valores de Tsolidos por preparo de solo
meanTSsoil <- PsubDBC %>%
  group_by(Psolo) %>%
  summarise(meanTSsoil = mean(Tsolidos, na.rm = TRUE))

# Cria um dataframe com o resultado do teste Tukey; 
TukeySoil <- tibble(
  Psolo = c("Grade Pesada", "Grade Leve", "Cultivo Mínimo"),
  tTest = c("a", "ab", "c")
)

# Adiciona os grupos Tukey às médias 
meanTSsoil <- left_join(meanTSsoil, TukeySoil, by="Psolo")

# Paleta do RColorBrewer
pTukeySoil <- brewer.pal(9, "BrBG")[4:6]

# Gráfico de Barras 
ggplot(meanTSsoil, aes(x = Psolo, y = meanTSsoil, fill = Psolo)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.6) +
  geom_text(
    aes(label = tTest), vjust = -0.5, color = "black") +
  scale_y_continuous(
    limits = c(0, 25), breaks = seq(0, 25, by = 1)) +
  coord_cartesian(ylim = c(12, 22)) +
  scale_fill_manual(values = pTukeySoil) +
  theme_light() +
  labs(title = "Psub DBC Batata | Tukey p/ preparo de solo",
       x = NULL,
       y = "% Sólidos") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  

## ----
## TUKEY BY FERTILIZER

# Calcula a média dos valores de Tsolidos por adubo
meanTSfert <- PsubDBC %>%
  group_by(Adubo) %>%
  summarise(meanTSfert = mean(Tsolidos, na.rm = TRUE))

# Cria um dataframe com o resultado do teste Tukey; 
TukeyFert <- tibble(
  Adubo = c("Mineral", "Organomineral", "Orgânico"),
  tTest = c("b", "a", "c")
)

# Adiciona os grupos Tukey às médias 
meanTSfert <- left_join(meanTSfert, TukeyFert, by="Adubo")

# Paleta do RColorBrewer
pTukeyFert <- brewer.pal(9, "BrBG")[4:6]

# Gráfico de Barras 
ggplot(meanTSfert, aes(x = Adubo, y = meanTSfert, fill = Adubo)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.6) +
  geom_text(
    aes(label = tTest), vjust = -0.5, color = "black") +
  scale_y_continuous(
    limits = c(0, 25), breaks = seq(0, 25, by = 1)) +
  coord_cartesian(ylim = c(12, 22)) +
  scale_fill_manual(values = pTukeyFert) +
  theme_light() +
  labs(title = "Psub DBC Batata | Tukey p/ adubo",
       x = NULL,
       y = "% Sólidos") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  












