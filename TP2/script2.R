library(readr)
wine = read.csv("wine.csv",header=TRUE)#Les données relatives aux vins de Loire

#Les données contiennent deux variables qualitatives (Appellation: "Label" = {Bourgueil,Chinon, Saumur} et Sol: "Soil" = {Env1, Env2, Env3=référence, Env4}) et p = 29 variables
#quantitatives décrivant diverses intensités sensorielles (odeur, arôme, goût, couleur etc).
#Les vins seront traduits en nuage dans l'espace des 29 variables quantitatives, ℝ29.
#On notera X la matrice dont les colonnes sont les 29 variables quantitatives centrées réduites, Y la matrice dont les colonnes sont les indicatrices d'appellations, et Z celle
#dont les colonnes sont les indicatrices de sols.
#On notera W =1/n \times In : la matrice des poids des individus et M = 1/p \times Ip :  celle des poids des variables. 
#Ici, tout est équipondéré.

