#Importer la base de données dans un dataframe nommée okcupid.


okcupid<-read.csv("C:/Users/maria/OneDrive/Escritorio/PROJETS MoSEF 2021-2022/Data Mining/okcupid_profiles.csv", stringsAsFactors=FALSE)

okcupid[okcupid==""]<-NA

#Regarder avec summary les stats principales de chaque variable. Pour les variables Numeriques on aura les valeurs MIN,MAX, la moyenne et les quartiles.
#Pour les variables categoriques on aura sa taille (length) class et mode.


okcupid[okcupid==""]<-NA



colSums(is.na(okcupid))

summary(okcupid)


#importer la librairie dplyr, necessaire pour separer le dataframe en NUM pour numerique et QUALI pour les qualitatives.

library(dplyr)

num<-okcupid %>%
  dplyr::select(where(is.numeric))


quali<-okcupid[sapply(okcupid,is.numeric)==F]
quali<-okcupid %>%
  dplyr::select(!where(is.numeric))



#Histogramme

for(x in seq(1,length(num),1)) hist(num[,x],xlab=names(num[x]),col=rainbow(10),main=names(num[x]))

#boxplot
for(x in seq(1,length(num),1)) boxplot(num[,x],col="pink",main=names(num[x]))



#Ce qu'on retire des boxplot et histogrammes sur les variables numeriques:

#AGE: il y a des outliers au dessous du Q3. Il y a des valeurs extremement aberrants (MAX=110). fAUDRA L'Eliminer.
#on peut dire que la pluplart des gens sont des jeunes, plus de 20 ans mais moins de 40.

#HEIGHT: idk le format utilisee...

#INCOME: problematique: la pluplart des valeurs sont -1: les gens ne publient pas leur income. ON PEUT L'ELIMINER.


#drop income de la base des numeriques

num2<-num[c("age","height")]


library(corrplot)


m<-cor(num2)
corrplot(m,method='ellipse')


#le corr plot, c'est vraiment necessaire?



pairs(num2)

#pairs a plus d sense, mais c'est une chose coherent


install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)


res.pca<-PCA(num2, graph=FALSE)

res.pca



eig.val<-get_eigenvalue(res.pca)

num_norm=scale(num2)


fviz_eig(res.pca,addlabels = TRUE,ylim=c(0,60))
fviz_pca_var(res.pca,col.var = "blue")
fviz_pca_var(res.pca,col.var = "blue",axes = c(1,2))



var<-get_pca_var(res.pca)



#ça n'a pas du sens de faire un ACP sur 2 variables numeriques...


#ANALYSE DES VARIABLES CAT ENLEVER LES ESSAY C'EST POUR LE TEXT MINING.




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
library(dplyr)
library(tidyverse)
#replace NA values in column SMOKES with "No answer"

quali2$smokes <- quali2$smokes %>% replace_na('No answer')


colSums(is.na(quali2))