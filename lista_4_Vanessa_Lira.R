# Lista 4

# Exercício 1:

# Exercício 2

# Carregando os pacotes
library(ffbase) 
library(tidyverse)
library(dplyr)

# Para usar o operador %>%
install.packages("magrittr")
library(magrittr)

# Definindo diretório
setwd("C:/Users/Dell/OneDrive/Acadêmico/Doutorado/2. Análise de Dados/R/AD-UFPE-2019-Vanessa")

# Carregando base de dados docentes (censo escolar 2016)
load("docentes_pe_censo_escolar_2016.RData")
load("matricula_pe_censo_escolar_2016.RData")

# Carregando dados do PNUD
install.packages("readxl")
library(readxl)
pnud_idh <- read_excel("atlas2013_dadosbrutos_pt.xlsx", "MUN 91-00-10")

# Transformando em data.frame
matricula_pe <- as.data.frame(matricula_pe)
docentes_pe <- as.data.frame(docentes_pe)

# Verificando estrutura da base de dados
dim(docentes_pe)
dim(matricula_pe)

# Vendo as variaveis em docentes
names(docentes_pe)
head(docentes_pe$CO_MUNICIPIO)
table(docentes_pe$CO_MUNICIPIO)
docentes_pe$CO_MUNICIPIO

# Para identificar o nome da variável de interesse (idade dos docentes)
names(docentes_pe)

# Sabemos que o nome da variável é NU_IDADE

# Para restringir as idades dos docentes entre 18 e 70 anos:
docentes_pe_selecao <- docentes_pe %>% filter(NU_IDADE >= 18 & NU_IDADE <= 70)

# Conferindo
docentes_pe_selecao$NU_IDADE

# Numero de docentes por municipio

docentes_pe_selecao_2 <- docentes_pe_selecao %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_docentes = n())
View(docentes_pe_selecao_2)

# Para restringir as idades dos alunos entre 1 e 25 anos:
matricula_pe_selecao <- matricula_pe %>% filter(NU_IDADE >= 1 & NU_IDADE <= 25)

# Vendo as variaveis em matriculas
names(matricula_pe_selecao)

# Conferindo
matricula_pe_selecao$NU_IDADE

# Numero de alunos por municipio

matricula_pe_selecao_2 <- matricula_pe_selecao %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_alunos = n())
View(matricula_pe_selecao_2)

# Juntando os data frames (docentes e matricula)
docentes_matricula <- left_join(docentes_pe_selecao_2, matricula_pe_selecao_2, by = "CO_MUNICIPIO")
View(docentes_matricula)

# Criando uma nova variavel

docentes_alunos <- mutate(docentes_matricula, aluno_docente = n_alunos/n_docentes)
View(docentes_alunos)

# Estatistica descritiva do número de alunos por docente nos municípios de PE
summary(docentes_alunos$aluno_docente)

# Min. - 4.431 
# 1st Qu. - 5.464 
# Median - 5.945 
# Mean - 6.043 
# 3rd Qu. - 6.584  
# Max. - 9.557 
         
# Média Aritmética é de 6.042907
mean(docentes_alunos$aluno_docente)

# Mediana é de 5.945007
median(docentes_alunos$aluno_docente)

# Moda
y <- c(sample(4:9, 185, replace = T))
table(y)
table(y)[which.max(table(y))]
#  O numero 9  se repetiu 35 vezes

# Quantis, sendo o 1st Qu. de 5.464 e o 3rd Qu. de 6.584
summary(docentes_alunos$aluno_docente)

# Percentis / Decis...
quantile(docentes_alunos$aluno_docente, probs = seq(0,1, .1))

# 0%  - 4.430962    
# 10% - 5.022380  
# 20% - 5.284936  
# 30% - 5.596355
# 40% - 5.728637
# 50% - 5.945007
# 60% - 6.149992
# 70% - 6.414382
# 80% - 6.705255
# 90% - 7.225328  
# 100% - 9.556772
  

# Medidas de dispersão

# Amplitude de 5.125809
max(docentes_alunos$aluno_docente) - min(docentes_alunos$aluno_docente)

# Variância de 0.7739116
var(docentes_alunos$aluno_docente)

# Desvio padrão de 0.8797225
sd(docentes_alunos$aluno_docente)

# Coeficiente de variação de 14.55794
100*0.8797225/6.042907

# Para saber o município com maior número de alunos por docente e seu IDHM:

# Carregando dados do PNUD
install.packages("readxl")
library(readxl)
pnud_idh <- read_excel("atlas2013_dadosbrutos_pt.xlsx", "MUN 91-00-10")

# Salvando como data frame
pnud_idh <- as.data.frame(pnud_idh)

# Filtrando dados de 2010 e no estado de PE
pnud_idh_pe <- pnud_idh%>%filter(ANO == 2010 & UF == 26)
View(pnud_idh_pe)

#Identificando as variavies de interesse
names(pnud_idh_pe)

# Selecionando apenas as variaveis de interesse (IDHM, COdmun7)
docentes_alunos_idh <- pnud_idh_pe %>% select (IDHM, Codmun7)
View(docentes_alunos_idh)

# Juntando com a base de dados docentes_alunos atraves da variavel Codmun7 com CO_MUNICIPIO

docentes_alunos_idh2 <- docentes_alunos_idh %>% left_join(docentes_alunos, by = c( "Codmun7" = "CO_MUNICIPIO"))
View(docentes_alunos_idh2)

#salvando nova base 

save (docentes_alunos_idh2, file = "docentes_alunos_idh2_censo_pnud_pe.RData")

# Municipio com maior número de alunos por docente

max(docentes_alunos_idh2$aluno_docente)
# [1] 9.556772

# Ao buscar na base de dados o numero acima (9.556772) na variavel aluno_docente, 
# encontramos que o mesmo corresponde ao municipio de codigo 2615805.
View(docentes_alunos_idh2)
# Logo, o município que apresenta maior número de alunos por docente é Tupanatinga.
# Seu IDHM é 0.519.
# Toda a identificação foi feita através da visualização do data frama através do comando View().
docentes_alunos_idh2[177,]
#      IDHM   Codmun7    n_docentes  n_alunos  aluno_docente
# 177 0.519    2615805      731      6986      9.556772

# Teste do coeficiente de correlação linear de Pearson entre o número de
# alunos por docente nos municípios do Estado e o IDH-M


# Correlacao é de -0.5057435
cor(docentes_alunos_idh2$aluno_docente, docentes_alunos_idh2$IDHM)
# [1] -0.5057435

# Teste de correlacao
cor.test(docentes_alunos_idh2$aluno_docente, docentes_alunos_idh2$IDHM)
# Temos um p-valor de 2.092e-13

# Salvando a base de dados criada para o cálculo em formato .RData
save (docentes_alunos_idh2, file = "docentes_alunos_idh2_censo_pnud_pe.RData")

# Gráfico de dispersão entre numero de alunos por docente e e IDHM
ggplot(data = docentes_alunos_idh2, aes(x = aluno_docente, y = IDHM) ) +
  geom_point(color = "green", size = 2) +
  labs(x = "Número de alunos por docente", y = "IDHM")

# A partir do gráfico, vemos que existe uma concentracao de numero de alunos por 
# docente em IDHM com valores entre 0,5 e 0,7. Sugerindo que quanto menos alunos
# por docente, maior é o IDHM do município. Nos extremos verticais e horizontais 
# vemos valores mais discrepantes, com municipios com IDHM muito altos e numero 
# de alunos por docente baixo. Enquanto que existem tambem os casos de numero 
# de alunos por docente alto e um IDHM baixo.Ou seja, quanto menos alunos um 
# professor(a) tem, maior e o IDHM.