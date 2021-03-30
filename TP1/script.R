#Charger dans le logiciel les données relatives aux vins de Loire (wine.csv).
#Elles contiennent deux variables qualitatives (Appellation: "Label" = {Bourgueil, Chinon,Saumur} et Sol: "Soil" = {Env1, Env2, Env3=référence, Env4}) et 29 variables quantitatives
#décrivant diverses intensités sensorielles (odeur, arôme, goût, couleur etc).
#Les vins seront traduits en nuage dans l'espace des 29 variables quantitatives, ℝ^29.

library(readr)
wine = read.csv("wine.csv",header=TRUE)#Les données relatives aux vins de Loire 

#Question 1 

#Il faut Centrer-réduire les variables quantitatives.
#Dans un premier temps, nous allons nommer nos variables quantitatives V.quantitative et V.qualitative :
V.quantitative <- wine[4:32]
V.qualitative <- wine[2:3]

#Maintenant on introduit une fonction qui permet de centrer réduire.
V.quantitative <- wine[4:32]
V.qualitative <- wine[2:3]

#Maintenant on introduit une fonction qui permet de centrer réduire.

Centre.reduire <- function(x){
  Centre_reduire <- function(x){
    N = 29
    moyenne <- mean(x)
    ecart.type <- sqrt((N-1)/N*var(x))
    y <- (x-moyenne)/(ecart.type)
    return(y)
  }
  y <- as.data.frame((lapply(x,Centre_reduire)))
  return(y)
}
Variables.C_R <- Centre.reduire(V.quantitative)
print(Variables.C_R)

#Maintenant, il faut montrer que le barycentre du nuage se trouve à l'origine. 
#On sait que le barycentre est le point d'application de la résultante des actions de la pesanteur sur toute les parties d'un corps ; c'est le centre de gravité. 
BarycentreVariables.C_R <- colMeans(Variables.C_R)
print(BarycentreVariables.C_R)

dotchart(sort(BarycentreVariables.C_R))

distance_euclidienne <- sqrt(sum((BarycentreVariables.C_R^2)))
distance_euclidienne 

#On remarque que le barycentres est composé de valeurs qui sont toutes très proches de 0.
#On peut donc en conclure que le barycentre du nuages se trouve à l'origine.
#Maintenant, il faut montrer que l'inertie totale du nuage est égale au nombre de variables; c'est à dire 29.

Inertie = 0
for (i in 1:29){
  Inertie <- Inertie + var(Variables.C_R[,i])*(28/29)
}
print(Inertie)
#Nous trouvons donc bien que l'inertie totale du nuage vaut 29.

#Question 2

#Calculer les poids et les barycentres des trois appellations (Bourgueil, Chinon, Saumur).Puis, calculer les normes euclidiennes carrées de ces trois barycentres.
#Pour commencer, calculons les poids des variables qualitatives.Dans un premier temps, nous allons initialiser à 0 les variables "bourgueuil", "Chinon", "Saumur"
for (i in 1:21){
  if(wine[i,2] == "Bourgueuil")
    Bo <- Bo + 1
  if(wine[i,2] == "Chinon")
    Ch <- Ch + 1
  if(wine[i,2] == "Saumur")
    Sa <- Sa + 1
}
#Poids
Poids_Bo <- Bo/Effectif
Poids_Ch <- Ch/Effectif
Poids_Sa <- Sa/Effectif

print(Poids_Bo)
print(Poids_Ch)
print(Poids_Sa)

#On intoduit des matrices des variables des appellations :
Bourgueuil = Variables.C_R[wine$Label == "Bourgueuil",]
Chinon = Variables.C_R[wine$Label == "Chinon",]
Saumur = Variables.C_R[wine$Label == "Saumur",]
#barycentre
BarycentreBourgueuil = colMeans(Bourgueuil)
BarycentreChinon = colMeans(Chinon)
BarycentreSaumur = colMeans(Saumur)
#Norme
NormeBourgueuil <- crossprod(BarycentreBourgueuil)
NormeChinon <- crossprod(BarycentreChinon) 
NormeSaumur <- crossprod(BarycentreSaumur) 

Inertie_Inter_Appellations <- Poids_Bo*((NormeBourgueuil)) + Poids_Ch*((NormeChinon)) + Poids_Sa*((NormeSaumur))
R2 <- (Inertie_Inter_Appellations)/(Inertie)
R2.sensorielle <- numeric(29)

for (i in 1:29){
  R2.sensorielle[i]= R2.sensorielle[i]+Poids_Bo*((BarycentreBourgueuil[i])^2)+Poids_Ch*((BarycentreChinon[i])^2)+Poids_Sa*((BarycentreSaumur[i])^2)
}
print(R2.sensorielle)



















