####################################################################################################################
#Loading to files about the company WEG:
# 3t20_dre.csv: file with the company results #https://ri.weg.net/informacoes-financeiras/planilhas/
# WEGE3SA.csv: file with stock market prices and volume
# https://br.financas.yahoo.com/quote/WEGE3.SA/history?period1=1262217600&period2=1611014400&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true
# I am trying to see how the stock prices react when the company results are presented.
# Everything I did here, I've learned in the course Formação Cientista de Dados, at DataScience Academy
# https://www.datascienceacademy.com.br/bundles?bundle_id=formacao-cientista-de-dados
# 
# Carlos Eduardo Carvalho
# carlos.e.carvalho@gmail.com
# Linkedin: https://www.linkedin.com/in/carlos-carvalho-93204b13/
#https://github.com/CarlosCarvalho1981/Analise_Weg.git
###################################################################################################################

#Working directory
setwd("D:/CIENTISTA_DADOS/WEG/Analise_Weg/ARQUIVOS")
getwd()

#Installing packages 

library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(reshape2)
library(plotrix)
library(moments)
library(hablar)
library(varhandle)
library(ggpubr)

#Loading the file
dre <- read_csv("3t20_dre.csv")
View(dre)

#There are some problems with the way the rows and columns are.
#Let's see the class of the object
class(dre)

#Let's remove the first column that won't help us in any way.
dre$X1 <- NULL

#Let's swap the rows and the columns.
inv_dre <- t(dre)
inv_dre

#Now it became a matrix
class(inv_dre)

#Let's turn it into a data frame
inv_dre <- as.data.frame(inv_dre)
class(inv_dre)
View(inv_dre)

#Since the name of some columns have characters that aren't recognized, let's rename everything.
colnames(inv_dre) <- c("Data", "Receita de Venda de Bens e/ou Servicos", "Custo dos Bens e/ou Servicos Vendidos", 
                       "Resultado Bruto" , "Despesas/Receitas Operacionais", "Despesas com Vendas", "Despesas Gerais e Administrativas" ,
                       "Honorarios dos Administradores", "Outras Despesas Administrativas", "Outras Receitas Operacionais", "Outras Despesas Operacionais",
                       "Resultado de Equivalencia Patrimonial", "Resultado Antes do Resultado Financeiro e dos Tributos",
                       "Resultado Financeiro", "Receitas Financeiras", "Despesas Financeiras", "Resultado Antes dos Tributos sobre o Lucro",
                       "Imposto de Renda e Contribuicao Social sobre o Lucro", "Corrente", "Diferido", "Resultado Liquido das Operacoes Continuadas",
                       "Lucro/Prejuizo Consolidado por Periodo", "Atribuido a Socios da Empresa Controladora", "Atribuido a Socios nao Controladores",
                       "Lucro por Acao - (Reais/Acao", "Lucro Basico por Acao", "ON", "Lucro Diluido por Acao", "ON")

#How does it look like?
View(inv_dre)


#We still have a row that is not usefull.  Let's remove it
inv_dre <- inv_dre[-c(1),]

#And also remove the last 2 columns
inv_dre <- inv_dre[,-c(30,31)]

#Now we have a good data frame
View(inv_dre)

#Let's take the columns Data and Lucro/Prejuizo Consolidado por Periodo
sub_dre <- inv_dre[,c("Data", "Lucro/Prejuizo Consolidado por Periodo")]

#How does this subset looks like?
View(sub_dre)

#Let's see the types of the columns
glimpse(sub_dre)

#Both variables are factors.  We're going to need the variable Data as a date format.
#Let's extract a vector to make the conversion
sub_dre1 <- sub_dre$Data

#Now we change the type and save it in a new object
Data2 <- as.Date(sub_dre1, format = "%m/%d/%Y")
glimpse(sub_dre1)

#And we put it back together
sub_dre <- cbind(sub_dre, Data2)
View(sub_dre)

#We have now columns with the same information.  Let's take off the one we don't need anymore.
sub_dre <- sub_dre[-c(1)]

#Let's look at some statistics
glimpse(sub_dre)

#Let's drop the first row
sub_dre <- sub_dre[-c(1),]
View(sub_dre)

#Saving it in a new file
write.csv(sub_dre, "Resultados.csv")

Resultados <- read_csv("Resultados.csv")
Resultados$`Lucro/Prejuizo Consolidado por Periodo` <- (Resultados$`Lucro/Prejuizo Consolidado por Periodo`)/1000
Resultados <- Resultados[,-1]
colnames(Resultados) <- c("Lucro_Prejuizo", "Data")
View(Resultados)

sub_dre <- cbind(sub_dre, Resultados$Lucro_Prejuizo)
View(sub_dre)
sub_dre <- sub_dre[,-1]
colnames(sub_dre) <- c("Data", "Lucro_Prejuizo")


#Quartis
quantile(sub_dre$Lucro_Prejuizo)

#Media
mean(sub_dre$Lucro_Prejuizo)

#Mediana
median(sub_dre$Lucro_Prejuizo)

#Desvia padrão
sd(sub_dre$Lucro_Prejuizo)

#Variância
var(sub_dre$Lucro_Prejuizo)

#Coeficiente de Variação
CV <- (sd(sub_dre$Lucro_Prejuizo)/mean(sub_dre$Lucro_Prejuizo)) * 100
CV

#Coeficiente de assimetria
skewness(sub_dre$Lucro_Prejuizo)

#Coeficiente de curtose
kurtosis(sub_dre$Lucro_Prejuizo)

#In order to make a plot, let's take only the last 10 rows.
sub_dre2 <- sub_dre[34:44,]
View(sub_dre2)

glimpse(sub_dre2)
#Scatter plot 
ggplot(data = sub_dre2, aes(x=Data, y = Lucro_Prejuizo, colour = Lucro_Prejuizo ))+
  geom_point(aes(size = 2)) + xlab("Data") + ylab("Lucro/Prejuizo por Periodo")

png("Grafico7.png", width = 1200, height = 500, res = 72)
hist(sub_dre2$Lucro_Prejuizo, main = "Resultados trimestrais", 
     xlab = "Lucro/prejuizo em Milhões")
dev.off()

png("Grafico8.png", width = 1200, height = 500, res = 72)
boxplot(sub_dre2$Lucro_Prejuizo)
dev.off()


#Now we're going to load the file with WEG stock prices
acoes <- read_csv("WEGE3SA.csv", col_types = list(
  Date = col_date(),
  Open = col_double(),
  High = col_double(),
  Low = col_double(),
  Close = col_double(),
  'Adj Close' = col_double(),
  Volume = col_integer()
))

#The file has some NULL rows that's why we've got those warnings.
#Let's see.
View(acoes)
#The NULL rows became NA.

glimpse(acoes)

#Let's drop those NA rows
acoes <- acoes[complete.cases(acoes), ]

#Let's look at some statistics
#Quartis
quantile(acoes$Close)

#Media
mean(acoes$Close)

#Mediana
median(acoes$Close)

#Desvia padrão
sd(acoes$Close)

#Variância
var(acoes$Close)

#Coeficiente de Variação
CVacoes <- (sd(acoes$Close)/mean(acoes$Close)) * 100
CVacoes

#Coeficiente de assimetria
skewness(acoes$Close)

#Coeficiente de curtose
kurtosis(acoes$Close)


hist(acoes$Close, main = "Preço de Fechamento das ações", 
     xlab = "Preço em Reais")

png("Grafico10.png", width = 1200, height = 500, res = 72)
boxplot(acoes$Close)
dev.off()

#Let's get a subset with the last 30 rows, just to see...
acoes30 <- acoes[661:691,]

#The last 2 are NA to.
acoes30 <- acoes30[complete.cases(acoes30), ]
View(acoes30)

#We are going to see a scatter plot of the stock prices with a nice linear regression.
ggplot(data = acoes30, aes(x = Date, y = Close))+
  geom_point() + xlab("Data") + ylab("Valor de Fechamento") + 
  geom_smooth(method = lm, color = "blue", se = FALSE)

#Now we're going to extract some vectors to work with.

#We are going to take the dates that are in both data frames: sub_dre2 and acoes. Or, at least, the next dates
acoes2 <- acoes[c(1,63,126,185,244,306,371,432,493,554,619),] #Had to do that manually 
View(acoes2)

#Extracting some vectors
Close2 <- acoes2$Close
Volume2 <- acoes2$Volume


#Let's have a column with the range of values instead of the real values
maxClose <- max(acoes$Close, na.rm = TRUE)
maxClose #90,28
minClose <- min(acoes$Close, na.rm = TRUE)
minClose #15,21

#Let's create the groups
groupClose <- function(fechamento){
  if(fechamento > 15.00 & fechamento <= 30.00){
    return(30)
  }else if(fechamento > 30.00 & fechamento <= 45.00){
    return(45)
  }else if(fechamento > 45.00 & fechamento <= 60.00){
    return(60)
  }else if(fechamento > 60.00 & fechamento <= 75.00){
    return(75)
  }else if(fechamento > 75.00 & fechamento <= 90.00){
    return(90)
  }else{
    return(95)
  }
}

acoes2$CloseGroup <- sapply(acoes2$Close, groupClose)
View(acoes2)

#Another vector
CloseGroup2 <- acoes2$CloseGroup

#And now we insert the vectors into the data frame
sub_dre2 <- cbind(sub_dre2, Close2)
sub_dre2 <- cbind(sub_dre2, Volume2)
sub_dre2 <- cbind(sub_dre2, CloseGroup2)
colnames(sub_dre2) <- c("Data", "Lucro_Prejuizo", "Close2", "Volume2", "CloseGroup2")
View(sub_dre2)

#We want to see if there is any relation between the closing price and the results
cov(sub_dre2$Lucro_Prejuizo, sub_dre2$Close2)

cor(sub_dre2$Lucro_Prejuizo, sub_dre2$Close2)

#Apparently there is a strong relation between the two.
png("Grafico11.png", width = 1200, height = 500, res = 72)
ggscatter(sub_dre2, x = "Lucro_Prejuizo", y = "Close2",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Lucro/Prejuizo", ylab = "Preço de Fechamento")
dev.off()


#Scatter plot 
#Let's see a scatter plot of  Lucro/Prejuizo Consolidado por Periodo, with the size of the dots as the
#closing stock prices and the color as the volume
ggplot(data = sub_dre2, aes(x=Data, y = Lucro_Prejuizo))+
  geom_point(aes(size = CloseGroup2, color = Volume2)) + xlab("Data") + ylab("Lucro/Prejuizo por Periodo")


#Plotting two line plots: Lucro/Prejuizo and Preco de Fechamento
par(mar = c(4,3,3,3), col.axis = "black", adj = 0.5)
plot(sub_dre2$Data, sub_dre2$Lucro_Prejuizo, type = "l", col = "red", bty = "o", 
     xlab = "Data", ylab = "Lucro/Prejuizo")
par(new = T)

plot(sub_dre2$Data, sub_dre2$Close2, type = "l", ann = F, axes = F, col = "darkblue", ylab = "Preco")
axis(side = 4)
legend("bottomright", pch = 1, col = c("blue", "red"), legend = c("Preco de Fechamento", "Lucro/Prejuizo"))
par(mfrow=c(1,1))
title(main = "Preco x Lucro/Prejuizo")

#Bar plot with volume of stocks buy/sold on the days that the results were presented.
ggplot(data = sub_dre2, aes(x = Data, y = Volume2 )) +
  geom_col(aes(fill = Volume2)) +
  ylab("Volume de Ações Negociadas") + xlab("Data de Divulgação dos Resultados")


#Now let's create another dataset, with dates next to the date when the results of the company were given

acoes3 <- acoes[c(1,2,3,62,63,64,125,126,127,184,185,186,243,244,245,305,306,307,370,371,372,431,432,433,492,493,494,553,554,555,618619,620),] #Had to do that manually 


#We've got some NA row, let's drop them
acoes3 <- acoes3[complete.cases(acoes3), ]
View(acoes3)

#Creating the last plot to see the stock prices near the days when the results came

plot(acoes3$Date, acoes3$Close, type = "l", col = "red", bty = "o", 
     xlab = "Data", ylab = "Preço de fechamento")
