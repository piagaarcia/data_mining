"
L'objectif de ce travail est de faire une analyse des données de l'app de rencontres OkCupid.
Pour le faire, nous nous sommes servis d'une base de données disponible sur le site Kaggle (https://www.kaggle.com/andrewmvd/okcupid-profiles)

Cette base contient 31 variables :




"





#librairies:

library(dplyr)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(tidyverse)


#Importer la base de données dans un dataframe nommée data

data<-read.csv("C:/Users/maria/OneDrive/Escritorio/PROJETS MoSEF 2021-2022/Data Mining/okcupid_profiles.csv", stringsAsFactors=FALSE)


#Regarder avec summary les stats principales de chaque variable. Pour les variables Numeriques on aura les valeurs MIN,MAX, la moyenne et les quartiles.
#Pour les variables categoriques on aura sa taille (length) class et mode.


#les valeurs manquantes sont represnetes par des espaces, on veut que ce soit NA pour que R puisse comprendre

data[data==""]<-NA


str(data) #info

colSums(is.na(data))  #combien de valeurs nulles

 
summary(data)   #résumé des stats



#diviser le dataset entre variables numeriques et qualitatives

numerique<-data %>%
  dplyr::select(where(is.numeric))


quali<-data[sapply(data,is.numeric)==F]
quali<-data %>%
  dplyr::select(!where(is.numeric))



#dropper les valeurs nulles de la var height, comme ça on a une base uniforme

num <-na.omit(numerique)


#Histogramme

for(x in seq(1,length(num),1)) hist(num[,x],xlab=names(num[x]),col=rainbow(10),main=names(num[x]))

#boxplot
for(x in seq(1,length(num),1)) boxplot(num[,x],col="pink",main=names(num[x]))


#probleme avec income et la notation scientifique

breaks=c(-1, 20000, 30000, 40000, 50000, 60000, 1200000)

hist(num$income, xlab = "Revenu", main = "Histogramme du Revenu",  breaks, col=rainbow(10))



#Ce qu'on retire des boxplot et histogrammes sur les variables numeriques:

#AGE: il y a des outliers au dessous du Q3. Il y a des valeurs extremement aberrants (MAX=110). fAUDRA L'Eliminer.
#on peut dire que la pluplart des gens sont des jeunes, plus de 20 ans mais moins de 40.

#HEIGHT : height in inches--> taille en pouces

#INCOME: problematique: la plupart des valeurs sont -1: les gens ne publient pas leur income. ON PEUT L'ELIMINER.
#Catégories : -1 (signifie que l’individu n’a pas voulu répondre), 20000, 30000, 40000, 50000, 60000 70000, 80000, 100000, 150000, 250000, 500000, 1000000.
"faire des bins pour voir les plots de INCOME"

#drop income de la base des numeriques

num2<-num[c("age","height")]





m<-cor(num2)
corrplot(m,method='ellipse')


#le corr plot, c'est vraiment necessaire?



pairs(num2)

#pairs a plus d sense, mais c'est une chose coherent




# apprentissage NON supervisé sur les variables numeriques
#je crois qu'on peut faire ça apres l'analyse exploratoire et le traitement des donnees

res.pca<-PCA(num2, graph=FALSE)

res.pca



eig.val<-get_eigenvalue(res.pca)

num_norm=scale(num2)


fviz_eig(res.pca,addlabels = TRUE,ylim=c(0,60))
fviz_pca_var(res.pca,col.var = "blue")
fviz_pca_var(res.pca,col.var = "blue",axes = c(1,2))



var<-get_pca_var(res.pca)



#ça n'a pas du sens de faire un ACP sur 2 variables numeriques...










#ANALYSE DES VARIABLES quant ENLEVER LES ESSAY C'EST POUR LE TEXT MINING.




quali2<-quali[c(1:18)]



colSums(is.na(quali2))


#body_type        diet      drinks       drugs   education   ethnicity         job




# offspring        pets    religion        sign      smokes      speaks 

35561/59946





#PROBLENE AVEC OFFSPRINF ET PETS

#Religion: remplacer avec "ne veut pas le dire" ou sim.

#Body_type: predire.


#education

#ethnicity

#job

#sign

#smokes

#speaks

#replace NA values in column SMOKES with "No answer"

quali2$smokes <- quali2$smokes %>% replace_na('No answer')


colSums(is.na(quali2))





#non supervise: segmentation, Multiple component analyses (objectif convertir donnes categor en numeriques)

#clustering
#expliquer les dimensions





#supervisé: predire qqchose



"analyse des sentiments selon essays

essay0- My self summary
essay1- What I’m doing with my life
essay2- I’m really good at
essay3- The first thing people usually notice about me
essay4- Favorite books, movies, show, music, and food
essay5- The six things I could never do without
essay6- I spend a lot of time thinking about
essay7- On a typical Friday night I am
essay8- The most private thing I am willing to admit
essay9- You should message me if...
"  



