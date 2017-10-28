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

#Total de Beneficiários Ativos na Cooperativa
B <- 300000
#10% dos Beneficiários Ativos
b <- 30000


# Criar um dataset com pelo menos cinco colunas
Identificador_Beneficiario <- 1:3000
peso <- round(rnorm(b,75,20),digits=3)
Altura <- round(rnorm(b,1.7,0.2),digits=2)
Sexo <- round(runif(b,min = 0, max = 1),0)
glicose <- rnorm(b,87,20)

# Criar um dataset com pelo menos três categóricas.
cat.refri <- rep(c(0, 1, 2, 3, 4, 5, 6), B) #rep -> repete os numeros de 1 a 6 
set.seed(1234) #Seto um número fixo para minha semente
refri.temp <- sample(cat.refri, b) # Busco uma amostra de 3000 beneficiários
refri <- factor(refri.temp, levels = c(0,1,2,3,4,5,6), labels = c("Nunca","1x Semana", "2x Semana", "3x Semana", "4x Semana","5x Semana", "6 ou Mais"), ordered = TRUE) #Crio as categorias e seus label's
rm(cat.refri, refri.temp)
str(refri)
summary(refri)

set.seed(1234)
cat.PraticaEsporte.n <- rbinom(b, 1, 0.8)
praticaEsporte <- factor(cat.PraticaEsporte.n, levels = c(0, 1), labels = c("sim", "não"), ordered = TRUE)
str(praticaEsporte)
summary(praticaEsporte)

set.seed(1234)
catHistoricoFamiliar.n <- rbinom(b, 1, 0.9)
historicoFamiliar <- factor(catHistoricoFamiliar.n, levels = c(0, 1), labels = c("sim", "não"), ordered = TRUE)
str(historicoFamiliar) #Exibe o conteúdo de forma compactada
summary(historicoFamiliar) #Sumariza o resultado


# Montando o dataset
DiabeticosDF <- data.frame(Identificador_Beneficiario # Cria meu Data Frame
                           ,peso
                           ,Altura
                           ,Sexo
                           ,glicose
                           ,refri
                           ,praticaEsporte
                           ,historicoFamiliar
)
str(DiabeticosDF)

# Grave seu dataset no disco
write.table(DiabeticosDF, file = "ProjetoPacientesDiabeticos.csv", sep = ",", col.names = TRUE, fileEncoding = "UTF-8")

# Sumarizando os dados: Exibindo (Valor Mínimo, 1º Quartil, Meidana, Média, 3º Quartil e Valor Máximo)
sapply(DiabeticosDF[,c("Identificador_Beneficiario", "peso", "Altura", "Sexo", "glicose", "refri", "praticaEsporte","historicoFamiliar")],summary)

# Exibindo a Soma das colunas numéricas 
apply(DiabeticosDF[,c("Identificador_Beneficiario", "peso", "Altura", "Sexo", "glicose")],2,sum)

# Mostando gráfico histograma da Glicose
hist(DiabeticosDF$glicose,
     main = "Histograma da Glicose",
     xlab = "Glicose (em mg/dl)",
     ylab = "Freqüência",
     border="black", 
     col="pink")

# Mostrando Gráfico de Pontos para peso x glicose, usando a cor para identificar frequência de consumo de Refrigerante
ggplot(data=DiabeticosDF, aes(x=peso, y=glicose, color = refri)) + geom_point()

# Mostrando Gráfico de Área para glicose x altura
ggplot(data=DiabeticosDF, aes(x=glicose, y=Altura)) + geom_area()

# Usando Split

#Pegando as 6 primeiras linhas
head(DiabeticosDF)
# Atribuindo as 6 primeiras linhas à A
a <- head(DiabeticosDF)
# Fazendo o Split de A por Sexo
split(a, a$Sexo)
# Fazendo split por praticantes de esporte
split(a, a$praticaEsporte)

#Espero que tenha entendido e atendido todos os pontos da solicitação
#Aguardo críticas para melhoria na forma de programar, assim como na idealização dos projetos
#Fábio Junior Schneider Marcelino
