"
L'objectif de ce travail est de faire une analyse des données de l'app de rencontres OkCupid.
Pour le faire, nous nous sommes servis d'une base de données disponible sur le site Kaggle (https://www.kaggle.com/andrewmvd/okcupid-profiles)

Cette base contient 31 variables.

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


#les valeurs manquantes sont representées par des espaces, on veut que ce soit NA pour que R puisse comprendre

data[data==""]<-NA


colSums(is.na(data))  #nombre de valeurs nulles

 
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

pairs(num2)

#PCA sur les variables numeriques

res.pca<-PCA(num2, graph=FALSE)

res.pca


#eigenvalues
eig.val<-get_eigenvalue(res.pca)

#graphs

fviz_eig(res.pca,addlabels = TRUE,ylim=c(0,60))
fviz_pca_var(res.pca,col.var = "blue")
fviz_pca_var(res.pca,col.var = "blue",axes = c(1,2))


var<-get_pca_var(res.pca)



#VARIABLES QUALITATIVES: TRAITEMENT DES VALEURS MANQUANTES


"si NA <=2997.3
alors remplacer par mode"



"drinks et speaks mode"




my_mode <- function(x) {                                     # Create mode function 
  unique_x <- unique(x)
  mode <- unique_x[which.max(tabulate(match(x, unique_x)))]
  mode
}

quali$drinks[is.na(quali$drinks)] <- my_mode(quali$drinks[!is.na(quali$drinks)])                #imputation mode drinks


quali$speaks[is.na(quali$speaks)] <- my_mode(quali$speaks[!is.na(quali$speaks)])                 # imputation mode speaks



quali$diet[is.na(quali$diet)] <- my_mode(quali$diet[!is.na(quali$diet)])                 # imputation mode diet

quali$drugs[is.na(quali$drugs)] <- my_mode(quali$drugs[!is.na(quali$drugs)])                 # imputation mode diet

quali$education[is.na(quali$education)] <- my_mode(quali$education[!is.na(quali$education)])                 # imputation mode diet

quali$offspring[is.na(quali$offspring)] <- my_mode(quali$offspring[!is.na(quali$offspring)])                 # imputation mode diet

quali$pets[is.na(quali$pets)] <- my_mode(quali$pets[!is.na(quali$pets)])                 # imputation mode diet
quali$sign[is.na(quali$sign)] <- my_mode(quali$sign[!is.na(quali$sign)])                 # imputation mode diet


quali$smokes <- quali$smokes %>% replace_na('No answer')   #imputation smokes "No Answer"


quali$body_type <- quali$body_type %>% replace_na('rather not say')   #imputation body_type "rather not say"

quali$job <- quali$job %>% replace_na('rather not say')   #imputation job "rather not say"

quali$ethnicity <- quali$ethnicity %>% replace_na('rather not say')   #imputation ethnicity "rather not say"

quali$religion <- quali$religion %>% replace_na('rather not say')   #imputation ethnicity "rather not say"

colSums(is.na(quali))



#essays sans essays


essays<-quali[c(19:28)]

quali<-quali[c(1:18)]


#non supervise: segmentation, Multiple component analyses (objectif convertir donnes categor en numeriques)

#clustering
#expliquer les dimensions



#supervisé: predire qqchose






