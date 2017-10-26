          # Curso Data Scientist
          # Módulo 2 - Linuagem R

# Criar um dataset com pelo menos cinco colunas numéricas e três categóricas.
# Use as funções de criação de distribuição aleatória, use pelo menos duas
# Grave seu dataset no disco
# Você deverá extrair do dataset:

          #EXTRAIR DO DATASET
# Somas e medias das colunas numéricas do dataset usando ??pply
# Usar split
# Mostra um gráfico
# Histograma

#Projeto para controle de Diabéticos
#Glicose normal em Jejum de 70 a 99 mg/dl

#10% dos Beneficiários Ativos
b <- 30000
#100% dos Beneficiários Ativos
B <- 300000

# Criar um dataset com pelo menos cinco colunas
Identificador_Beneficiario <- 1:3000
peso <- abs(round(rnorm(n,75,20)))
Altura <- abs(rnorm(n,1.7,0.2))
Sexo <- round(runif(n,min = 0, max = 1),0)
glicose <- rnorm(n,87,20)

# Criar um dataset com pelo menos três categóricas.
> cat.refri <- rep(c(0, 1, 2, 3, 4, 5, 6), p)
> set.seed(1234)
> refri.temp <- sample(cat.refri, n)
> refri <- factor(refri.temp,
+ levels = c(0,1,2,3,4,5,6),
+ labels = c("Nunca","1x Semana", "2x Semana", "3x Semana", "4x Semana",
+            "5x Semana", "6 ou Mais"), ordered = TRUE)
> rm(cat.refri, refri.temp)
> str(refri)
summary(refri)






