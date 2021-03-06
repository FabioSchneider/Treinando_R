# Curso Data Scientist
# M�dulo 2 - Linuagem R

# Criar um dataset com pelo menos cinco colunas num�ricas e tr�s categ�ricas.
# Use as fun��es de cria��o de distribui��o aleat�ria, use pelo menos duas
# Grave seu dataset no disco
# Voc� dever� extrair do dataset:

#EXTRAIR DO DATASET

# Somas e medias das colunas num�ricas do dataset usando ??pply
# Usar split
# Mostra um gr�fico
# Histograma

#Projeto para controle de Diab�ticos

#Glicose normal em Jejum de 70 a 99 mg/dl

B <- 3000 #Total de Benefici�rios Ativos na Cooperativa
b <- 300  #10% dos Benefici�rios Ativos

# Criar um dataset com pelo menos cinco colunas
Identificador_Beneficiario <- 1:3000
peso <- round(rnorm(b,75,20),digits=3)
Altura <- round(rnorm(b,1.7,0.2),digits=2)
Sexo <- round(runif(b,min = 0, max = 1),0)
glicose <- rnorm(b,87,20)
diabetico <- rnorm(b,0,0)

# Criar um dataset com pelo menos tr�s categ�ricas.
cat.refri <- rep(c(0, 1, 2, 3, 4, 5, 6), B) #rep -> repete os numeros de 1 a 6 
set.seed(1234) #Seto um n�mero fixo para minha semente
refri.temp <- sample(cat.refri, b) # Busco uma amostra de 3000 benefici�rios
refri <- factor(refri.temp, levels = c(0,1,2,3,4,5,6), labels = c("Nunca","1x Semana", "2x Semana", "3x Semana", "4x Semana","5x Semana", "6 ou Mais"), ordered = TRUE) #Crio as categorias e seus label's
rm(cat.refri, refri.temp)
str(refri)
summary(refri)

set.seed(1234)

cat.PraticaEsporte.n <- rbinom(b, 1, 0.8)
praticaEsporte <- factor(cat.PraticaEsporte.n, levels = c(0, 1), labels = c("sim", "n�o"), ordered = TRUE)
str(praticaEsporte)
summary(praticaEsporte)

set.seed(1234)

catHistoricoFamiliar.n <- rbinom(b, 1, 0.9)
historicoFamiliar <- factor(catHistoricoFamiliar.n, levels = c(0, 1), labels = c("sim", "n�o"), ordered = TRUE)
str(historicoFamiliar) #Exibe o conte�do de forma compactada
summary(historicoFamiliar) #Sumariza o resultado

#AULA 4
#EXPLORA��O DE DADOS
#PROJETO CLUSTERIZA��O

#Criando 2 variaveis independentes 
imc <- peso / Altura
n.amostras <- b

# Montando o dataset
for (n in 1:n.amostras)
{
  if (glicose[n] > 99)
  {diabetico[n] <- 1}
  else 
  {diabetico[n] <- 2}
}
DiabeticosDF <- data.frame(Identificador_Beneficiario # Cria meu Data Frame
                          ,peso
                          ,Altura
                          ,Sexo
                          ,glicose
                          ,refri
                          ,praticaEsporte
                          ,historicoFamiliar
                          ,diabetico
                          ,imc
                          ,diabetico
                          
)



str(DiabeticosDF)




# Grave seu dataset no disco
write.table(DiabeticosDF, file = "ProjetoPacientesDiabeticos.csv", sep = ",", col.names = TRUE, fileEncoding = "UTF-8")

# Sumarizando os dados: Exibindo (Valor M�nimo, 1� Quartil, Meidana, M�dia, 3� Quartil e Valor M�ximo)
sapply(DiabeticosDF[,c("Identificador_Beneficiario", "peso", "Altura", "Sexo", "glicose", "refri", "praticaEsporte","historicoFamiliar")],summary)

# Exibindo a Soma das colunas num�ricas 
apply(DiabeticosDF[,c("Identificador_Beneficiario", "peso", "Altura", "Sexo", "glicose")],2,sum)

# Mostando gr�fico histograma da Glicose
hist(DiabeticosDF$glicose,
  main = "Histograma da Glicose",
  xlab = "Glicose (em mg/dl)",
  ylab = "Freq��ncia",
  border="black", 
  col="pink")

# Mostrando Gr�fico de Pontos para peso x glicose, usando a cor para identificar frequ�ncia de consumo de Refrigerante
library(ggplot2)
ggplot(data=DiabeticosDF, aes(x=peso, y=glicose, color = refri)) + geom_point()

# Mostrando Gr�fico de �rea para glicose x altura
ggplot(data=DiabeticosDF, aes(x=glicose, y=Altura)) + geom_area()

# Usando Split
#Pegando as 6 primeiras linhas

head(DiabeticosDF)

# Atribuindo as 6 primeiras linhas � A
a <- head(DiabeticosDF)

# Fazendo o Split de A por Sexo
split(a, a$Sexo)

# Fazendo split por praticantes de esporte
split(a, a$praticaEsporte)


#AULA 4
#EXPLORA��O DE DADOS
#PROJETO CLUSTERIZA��O

#Sumarizando os dados
summary(DiabeticosDF[,-c(1,4,9)])

#Gerando Boxplot
boxplot(DiabeticosDF$glicose, col = "red")
boxplot(DiabeticosDF$imc, col = "blue")

#Gerando Histograma
hist(DiabeticosDF$glicose,
     main = "Histograma da Glicose",
     xlab = "Glicose (em mg/dl)",
     ylab = "Freq��ncia",
     border="black", 
     col="pink")
hist(DiabeticosDF$imc,
     main = "Histograma do IMC",
     xlab = "IMC",
     ylab = "Freq��ncia",
     border="black", 
     col="green")
rug(DiabeticosDF$imc)
abline(v=mean(DiabeticosDF$imc), col = "red")

#Gerando Barplot
#1 = Diabetico
#2 = N�o Diabetico
barplot(table(DiabeticosDF$diabetico), col = "pink", main = "Numero de Diab�ticos")


with(DiabeticosDF, plot(imc, peso, col=diabetico))

#Usando cluster
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
data <- transform(DiabeticosDF, diabetico = factor(diabetico))
sub1 <- subset(DiabeticosDF)
plot(sub1[, 2], col = sub1$diabetico, ylab = names(sub1)[2])
plot(sub1[, 10], col = sub1$diabetico, ylab = names(sub1)[10])

#Imprimindo Dendograma
#Exibindo apenas 6 por falta de hardware
distanceMatrix <- dist(head(sub1[,2,10]))
hclustering <- hclust(distanceMatrix)
plot(hclustering)

#Analisando a Distancia
dist(DiabeticosDF[,-c(1,4,6,7,8,9)])


#Espero que tenha entendido e atendido todos os pontos da solicita��o
#Aguardo cr�ticas para melhoria na forma de programar, assim como na idealiza��o dos projetos
#F�bio Junior Schneider Marcelino

#Tracando os pontos
dataFrame <- data.frame(x=head(DiabeticosDF$peso),y=head(DiabeticosDF$glicose))
dist(dataFrame)

#Gerando Dendograma
suppressMessages(library(fields))
rdistxy <- rdist(dataFrame)
diag(rdistxy) <- diag(rdistxy) + 1e5

ind <- which(rdistxy == min(rdistxy),arr.ind=TRUE)
par(mfrow=c(1,2),mar=rep(0.2,4))
plot(dataFrame$x ,dataFrame$y,col="blue",pch=19,cex=2)
text(dataFrame$x+0.05,dataFrame$y+0.05,labels=as.character(1:12))
points(dataFrame$x[ind[1,]],dataFrame$y[ind[1,]],col="orange",pch=19,cex=2)

# Encontrando os pontos de menor distancia
ind <- which(rdistxy == min(rdistxy),arr.ind=TRUE)
par(mar=rep(0.2,4))

plot(dataFrame$x,dataFrame$y,col="blue",pch=19,cex=2)
text(dataFrame$x+0.05,dataFrame$y+0.05,labels=as.character(1:12))
points(dataFrame$x[ind[1,]],dataFrame$y[ind[1,]],col="orange",pch=19,cex=2)
points(mean(dataFrame$x[ind[1,]]),mean(dataFrame$y[ind[1,]]),col="black",cex=3,lwd=3,pch=3)
points(mean(dataFrame$x[ind[1,]]),mean(dataFrame$y[ind[1,]]),col="orange",cex=5,lwd=3,pch=1)

#Exibindo o Cluster
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)

#Gerando HeatMap
dataMatrix <- as.matrix(dataFrame)[sample(1:6),]
heatmap(dataMatrix)

#Gerando kmeans
kmeansObj <- kmeans(dataFrame,centers=3)
names(kmeansObj)
kmeansObj$cluster

kmeansObj <- kmeans(dataFrame,centers=3)
names(kmeansObj)
kmeansObj$cluster

par(mar=rep(0.2,4))
plot(dataFrame$x ,dataFrame$y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)

#Gerando o Heatmaps
par(mfrow=c(1,2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[,nrow(dataMatrix):1],yaxt="n")
image(t(dataMatrix)[,order(kmeansObj$cluster)],yaxt="n")
