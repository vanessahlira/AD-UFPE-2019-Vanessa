# Lista 3

# Exercicio 1: https://github.com/vanessahlira/AD-UFPE-2019-Vanessa

# Exercicio 2:
getwd()
# x = idade 
# y = renda
x <- c(40, 30, 20)
y <- c(900, 800,700)
class(x)
class(y)
z <- sum(x,y)
# [1] 2490
class(z)
z <- z*08309405456
z
## Resposta: o resultado é [1] 2.069042e+13

# Exercicio 3: 
head(mtcars)
# Uma descricao dos tipos de variaveis da base
str(mtcars)
# O numero de dimensoes da base
dim(mtcars)
# Imprima a terceira coluna da base.
mtcars[,3]
# Imprima a segunda linha da base.
mtcars[2,]
# O quarto elemento presente na variavel cyl.
mtcars[4,2]
# Um resumo descritivo da base que apresente as principais informacoes descritivas das variaveis.
summary(mtcars)

# Exercicio 4:
require(ffbase)
getwd()
setwd("C:/Users/Dell/OneDrive/Acadêmico/Doutorado/2. Análise de Dados/R/AD-UFPE-2019-Vanessa")
turmas <- read.csv2.ffdf(file = "TURMAS.csv", sep = "|", first.rows=100000)

# Filtrando PE
turmas_pe <- subset(turmas, CO_UF == 26)
dim(turmas_pe)
names(turmas_pe)
head(turmas_pe)
# Verificando se só selecionou mesmo PE
turmas_pe$CO_UF
# transformando em data frame
turmas_pe <- as.data.frame(turmas_pe) # definindo diretorio
# definindo diretorio
setwd("C:/Users/Dell/OneDrive/Acadêmico/Doutorado/2. Análise de Dados/R/AD-UFPE-2019-Vanessa")
# salvando a base em formato RData
save(turmas_pe, file ="turmas_pe.RData")

# Exercicio 5:
# Carregando a base de dados
setwd("C:/Users/Dell/OneDrive/Acadêmico/Doutorado/2. Análise de Dados/R/AD-UFPE-2019-Vanessa")
load("turmas_pe.RData")
# Média do número de matrículas por turma.
names(turmas_pe)
mean(turmas_pe$NU_MATRICULAS)
## Resposta: a média é de 23.07089 matrículas por turma.

# Exercico 6:

# Carregando base de dados:
setwd("C:/Users/Dell/OneDrive/Acadêmico/Doutorado/2. Análise de Dados/R/AD-UFPE-2019-Vanessa")
require(ffbase)
docentes <- read.csv2.ffdf(file = "DOCENTES_NORDESTE.csv", sep = "|", first.rows=100000)

# salvando nova base
save(docentes, file ="DOCENTES_NORDESTE.RData")

# Filtrando PE
docentes_pe <- subset(docentes, CO_UF == 26)
# Verificando
dim(docentes_pe)
names(docentes_pe)
head(docentes_pe)

# Docentes de PE que nao declararam cor ou raca e que se declararam pretos ou pardos no Censo
range(docentes_pe$TP_COR_RACA)
# Com isso sabemos que os valores da variável "TP_COR_RACA" variam de 0 a 5.
table(docentes_pe$TP_COR_RACA)
# Sabemos que os valores dessa variável variam de 0 a 5, porém não sabemos qual categoria corrresponde a negros e pardos.
# Assim, consultamos o arquivo LEIA-ME e este indicou o anexo I para dicionario das variaveis
# Dessa forma, temos que:

table(docentes_pe$TP_COR_RACA)
## 0      1      2      3      4      5 
## 181573  98141  14710 114718   1419   2102

# Como 0 corresponde aos valores daqueles que não declararam, cor ou raça, temos 181573 docentes nessa categoria.
sum(docentes_pe$TP_COR_RACA)
# [1] 487901
181573*100/487901
# [1] 37.21513
## Resposta: 37,2% dos docentes não declaram cor e raca. 

# 129428 docentes se declararam pretos ou pardos. Como a categoria 2 corresponde a cor/raca preta e a categoria 3 a cor/raca parda, somando os valores de ambas temos:
14710+114718
# [1] 129428
129428*100/487901
# [1] 26.52751
## Resposta: 26,52% dos docentes se declararam pretos ou pardos.


