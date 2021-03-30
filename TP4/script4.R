library(FactoMineR)
library(munsell)
library("factoextra")
library("corrplot")
library(farver)
library(labeling)
library(ade4)
library(readr)
library(knitr)

Villes <- read.table("villes.csv",header=T,sep = "\t")
Villes[49,2]<-0.113
#View(Villes)
#ACP grossière
VillesBrut <- data.frame(scale(Villes[,2:55]) * sqrt(54/53)) 
rownames(VillesBrut)<-Villes$Ville
PCA1Villes <- PCA(VillesBrut, scale.unit = F)

v<-PCA1Villes$var$cos2# CO2 des variables
#corrplot(PCA1Villes$var$cos2, is.corr=FALSE)
plot(PCA1Villes,choix="var",select="cos2 0.7")# selectionner les co2
fviz_pca_ind(PCA1Villes, col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(PCA1Villes, col.var = "cos2" ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )

dv=dist(VillesBrut, method="euclidean")
CAHV = hclust(d=dv, method = "ward.D2")
suppressPackageStartupMessages(library(dendextend))
d <- as.dendrogram(CAHV)
d <- d%>%color_branches(k=3)%>%color_labels(k=3)
plot(d,main = "Dendogramme", cex = 1.5, cex.lab = 1.7, cex.main = 2, xlab = "Villes", ylab = "Indice")

#ACP par rang
VillesRank <- apply(VillesBrut,2,rank)
PCAVillesRank <- PCA(VillesRank,scale.unit = T)
j<-PCAVillesRank$var$cos2# CO2 des variables
#corrplot(PCA1Villes$var$cos2, is.corr=FALSE)
plot(PCAVillesRank,choix="var",select="cos2 0.7")# selectionner les co2
fviz_pca_ind(PCAVillesRank,col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(PCAVillesRank, col.var = "cos2" ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )

#ACP thème économique
attach(VillesBrut)
Economie = cbind.data.frame(
  Chomage, 
  ChomageJeunes, 
  ChomageLong, 
  EvoluEmploiCree,
  Activite,
  EmploiFeminin,
  EmploiCommune,
  DefaillEntreprise,
  SalaireAnnuel,
  ImpotRevenu,
  ImpotFortune,
  Imposables,
  MetreCarreAncien,
  TaxeHabitation,
  FoncierBati,
  MetreCubeEau,
  EvolDemographique,
  Vieillissement,
  AttiranceGlobale,
  AttiranceActifs,
  Proprietaires,
  LogtSup4pieces,
  LogtInsalubre,
  LogtVacant,
  LogtConstruction
  
)
Eco = cbind.data.frame(Economie)
Eco <- data.frame(mapply(FUN = as.numeric,Eco))
rownames(Eco)<-Villes$Ville
PCAEco <- PCA(Eco)

v1<-PCAEco$var$cos2
#corrplot(PCAEco$var$cos2, is.corr=FALSE)
plot(PCAEco,choix="var",select="cos2 0.7")
fviz_pca_ind(PCAEco, col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(PCAEco, col.var = "cos2" ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )

#ACP thème risque
Risques = data.frame(
  Criminalite,
  EvolutionCrimes,
  SecuriteRoutiere,
  Inondations,
  TerrainsPollues,
  UsinesRisques,
  MortaliteInfantile,
  MortaliteCancerPoumon,
  MortaliteAlcool,
  DecesInfarctus,
  TauxSuicide,
  MortaliteGlobale,
  TailleClassesPrimaires,
  Retard6eme,
  Retard3eme,
  RetardTerminale
)

Ris = cbind.data.frame(Risques)
Ris <-data.frame(mapply(FUN = as.numeric,Ris))
rownames(Ris)<-Villes$Ville
PCARis <- PCA(Ris,scale.unit = F)

v2<-PCARis$var$cos2
#corrplot(PCARis$var$cos2, is.corr=FALSE)
plot(PCARis,choix="var",select="cos2 0.7")
fviz_pca_ind(PCARis, col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(PCARis, col.var = "cos2" ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )

#ACP thème nature
Nature = data.frame(
  Mer,
  Ski,
  Soleil,
  Pluie,
  Temperature,
  MarcheAPied
)

Nat = cbind.data.frame(Nature)
Nat <- data.frame(mapply(FUN = as.numeric,Nat))
rownames(Nat)<-Villes$Ville
PCANat <- PCA(Nat,scale.unit = F)

v3<-PCANat$var$cos2
#corrplot(PCANat$var$cos2, is.corr=FALSE)
plot(PCANat,choix="var",select="cos2 0.7")
fviz_pca_ind(PCANat, col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(PCANat, col.var = "cos2" ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )

#ACP thème culture
Culture = data.frame(
  Musees,
  Cinema,
  MonumHistoriques,
  PretLivres,
  RestaurDistingues,
  Presse,
  Etudiants
)

Cult = cbind.data.frame(Culture)
Cult <- data.frame(mapply(FUN = as.numeric,Cult))
rownames(Cult)<-Villes$Ville
PCACult <- PCA(Cult,scale.unit = F)

v4<-PCACult$var$cos2
#corrplot(PCACult$var$cos2, is.corr=FALSE)
plot(PCACult,choix="var",select="cos2 0.7")
fviz_pca_ind(PCACult, col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(PCACult, col.var = "cos2" ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )

#ACP de rang thème économie
EcoRank <- apply(Economie,2,rank)
PCAEcoRank <- PCA(EcoRank,scale.unit = T)
rownames(EcoRank)<-rownames(VillesBrut)
v5<-PCAEcoRank$var$cos2
#corrplot(PCACult$var$cos2, is.corr=FALSE)
plot(PCAEcoRank,choix="var",select="cos2 0.7")
fviz_pca_ind(PCAEcoRank, col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(PCAEcoRank, col.var = "cos2" ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )


#ACP de rang thème risques
RisRank <- apply(Risques,2,rank)
PCARisRank <- PCA(RisRank,scale.unit = T)
rownames(Risques)<-rownames(VillesBrut)
v6<-PCARisRank$var$cos2
#corrplot(PCACult$var$cos2, is.corr=FALSE)
plot(PCARisRank,choix="var",select="cos2 0.7")
fviz_pca_ind(PCARisRank, col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(PCARisRank, col.var = "cos2" ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )


#ACP de rang thème nature
NatRank <- apply(Nature,2,rank)
PCANatRank <- PCA(NatRank,scale.unit = T)
rownames(NatRank)<-rownames(Nat)
v7<-PCANatRank$var$cos2
#corrplot(PCACult$var$cos2, is.corr=FALSE)
plot(PCANatRank,choix="var",select="cos2 0.7")
fviz_pca_ind(PCANatRank, col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(PCANatRank, col.var = "cos2" ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )


#ACP de rang thème culture
CultRank <- apply(Culture,2,rank)
PCACultRank <- PCA(CultRank,scale.unit = T)
rownames(Cult)<-rownames(VillesBrut)
v7<-PCACultRank$var$cos2
#corrplot(PCACult$var$cos2, is.corr=FALSE)
plot(PCACultRank,choix="var",select="cos2 0.7")
fviz_pca_ind(PCACultRank, col.ind="contrib",gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
fviz_pca_var(PCACultRank, col.var = "cos2" ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE )












