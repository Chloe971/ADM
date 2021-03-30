#Importez le fichier villes.csv dans R (read.table("villes.csv",header=T))
VillesBrut <- read.table("villes.csv",header=T,sep = "\t")
#View(VillesBrut)

#Nous avons donc un tableau de données qui réunit des données décrivant 100 villes de France selon 54 variables représentant plusieurs indicateurs socio-économiques, écologiques,culturels.
#On désire mettre en oeuvre une classification thématique complétée par un treillis de Galois. Pour ce faire, on doit regrouper des thèmes afin de les rendre moins nombreux. On décide le
#regroupement suivant:
  # - Économie: Chomage, ChomageJeunes, ChomageLong, EvoluEmploiCree,Activite,EmploiFeminin, EmploiCommune, DefaillEntreprise,SalaireAnnuel, ImpotRevenu, ImpotFortune, Imposables,
#MetreCarreAncien, TaxeHabitation, FoncierBati,MetreCubeEau,EvolDemographique, Vieillissement,AttiranceGlobale,AttiranceActifs, Proprietaires,LogtSup4pieces, LogtInsalubre, LogtVacant, LogtConstruction

  # - Risques: Criminalite, EvolutionCrimes, SecuriteRoutiere, Inondations,TerrainsPollues, UsinesRisques,MortaliteInfantile, MortaliteCancerPoumon, MortaliteAlcool,DecesInfarctus,TauxSuicide, MortaliteGlobale,
#TailleClassesPrimaires, Retard6eme, Retard3eme,RetardTerminale

  # - Nature: Mer, Ski, Soleil, Pluie, Temperature, MarcheAPied

  # - Culture: Musees, Cinema, MonumHistoriques, PretLivres,RestaurDistingues, Presse, Etudiants

#Standardiser les variables. 
Villes = data.frame(scale(VillesBrut[,2:55]) * sqrt(54/53))

#CAH de l'ensemble des variables.
#a) création de la matrice des distances euclidiennes:
dv=dist(Villes, method="euclidean")

#b)CAH avec Ward (vous pouvez essayer plusieurs indices pour juger de la stabilité des résultats):
CAHV = hclust(d=dv, method = "ward.D")

#c)Dendrogramme de la hiérarchie indicée:
plot(CAHV)

#d)Coupure de l'arbre et fabrication de la variable de classe correspondant à la partition obtenue,par exemple k=2 classes:
PV2 = cutree(tree = CAHV, k=2)
print(PV2)

#e)Calcul du R2 des variables avec la variable de classe. On va stocker tous les R2 dans un seul vecteur: R2.
R2_PV2 = cbind(rep(0 , ncol(Villes)))
for (i in cbind(1:ncol(Villes))) {
  
  R2_PV2[i] =
    summary(lm(Villes[,i]~as.factor(PV2)))$r.squared
}
row.names(R2_PV2) = colnames(Villes)

#f)Calcul du R2 de la partition
R2G_PV2 = mean(R2_PV2)
print(R2G_PV2)

#h)Optimisation d'une partition avec les K-means, e.g. pour celle en 2 classes:
IC2Vil = data.frame(model.matrix(~as.factor(PV2)-1))
mIC2Vil = as.matrix(IC2Vil)
mVil = as.matrix(Villes)
CentresC2 = solve(t(mIC2Vil) %*% mIC2Vil) %*% t(mIC2Vil)%*% mVil
KMV2 = kmeans(Villes, CentresC2)
KMV2$cluster

#CAH par thèmes.
#Economie
Economie = data.frame(
  Villes$Chomage, 
  Villes$ChomageJeunes, 
  Villes$ChomageLong, 
  Villes$EvoluEmploiCree,
  Villes$Activite,
  Villes$EmploiFeminin,
  Villes$EmploiCommune,
  Villes$DefaillEntreprise,
  Villes$SalaireAnnuel,
  Villes$ImpotRevenu,
  Villes$ImpotFortune,
  Villes$Imposables,
  Villes$MetreCarreAncien,
  Villes$TaxeHabitation,
  Villes$FoncierBati,
  Villes$MetreCubeEau,
  Villes$EvolDemographique,
  Villes$Vieillissement,
  Villes$AttiranceGlobale,
  Villes$AttiranceActifs,
  Villes$Proprietaires,
  Villes$LogtSup4pieces,
  Villes$LogtInsalubre,
  Villes$LogtVacant,
  Villes$LogtConstruction
  
)

dvEco = dist(Economie, method="euclidean")
CAHVEco = hclust(d=dvEco, method = "ward.D")
plot(CAHVEco)
rect.hclust(CAHVEco,3)
PEco = cutree(tree = CAHVEco, k=3)
IC2Eco = data.frame(model.matrix(~as.factor(PEco)-1))
mIC2Eco = as.matrix(IC2Eco)
mEco = as.matrix(Economie)
CentresC2Eco = solve(t(mIC2Eco) %*% mIC2Eco) %*% t(mIC2Eco)%*% mEco
KMEco = kmeans(Economie, CentresC2Eco)
print(KMEco)

Eco <- cbind.data.frame(Villes[,1],Economie) 

Ceco1 <- vector()
Ceco2 <- vector()
Ceco3 <- vector()

a<-1
b<-1
c<-1

for (i in 1:100) {
  
  if (KMEco$cluster[i] == 1){
    Ceco1[a] <- as.matrix(Eco[i,1])
    a <- a+1
  }
  
  if (KMEco$cluster[i] == 2){
    Ceco2[b] <- as.matrix(Eco[i,1]) 
    b <- b+1
  }
  
  if (KMEco$cluster[i] == 3){
    Ceco3[c] <- as.matrix(Eco[i,1])
    c <- c+1
  }
}

#Risques
Risques = data.frame(
  Villes$Criminalite,
  Villes$EvolutionCrimes,
  Villes$SecuriteRoutiere,
  Villes$Inondations,
  Villes$TerrainsPollues,
  Villes$UsinesRisques,
  Villes$MortaliteInfantile,
  Villes$MortaliteCancerPoumon,
  Villes$MortaliteAlcool,
  Villes$DecesInfarctus,
  Villes$TauxSuicide,
  Villes$MortaliteGlobale,
  Villes$TailleClassesPrimaires,
  Villes$Retard6eme,
  Villes$Retard3eme,
  Villes$RetardTerminale
)

dvRis = dist(Risques, method="euclidean")
CAHVRis = hclust(d=dvRis, method = "ward.D")
plot(CAHVRis)
rect.hclust(CAHVRis,3)
PRis = cutree(tree = CAHVRis, k=3)
IC2Ris = data.frame(model.matrix(~as.factor(PRis)-1))
mIC2Ris = as.matrix(IC2Ris)
mRis = as.matrix(Risques)
CentresC2Ris = solve(t(mIC2Ris) %*% mIC2Ris) %*% t(mIC2Ris)%*% mRis
KMRis = kmeans(Risques, CentresC2Ris)
print(KMRis)

Ris <- cbind.data.frame(Villes[,1],Risques) 

CRIS1 <- vector()
CRIS2  <- vector()
CRIS3  <- vector()

a<-1
b<-1
c<-1

for (i in 1:100) {
  
  if (KMRis $cluster[i] == 1){
    CRIS1[a] <- as.matrix(Ris[i,1])
    a <- a+1
  }
  
  if (KMRis $cluster[i] == 2){
    CRIS2[b] <- as.matrix(Ris[i,1]) 
    b <- b+1
  }
  
  if (KMRis $cluster[i] == 3){
    CRIS3[c] <- as.matrix(Ris[i,1])
    c <- c+1
  }
}

#Nature 
Nature = data.frame(
  Villes$Mer,
  Villes$Ski,
  Villes$Soleil,
  Villes$Pluie,
  Villes$Temperature,
  Villes$MarcheAPied
)

dvNat = dist(Nature, method = "euclidean")
CAHVNat = hclust(d=dvNat, method = "ward.D")
plot(CAHVNat)
rect.hclust(CAHVNat,3)
PNat = cutree(tree = CAHVNat, k=3)
IC2Nat = data.frame(model.matrix(~as.factor(PNat)-1))
mIC2Nat = as.matrix(IC2Nat)
mNat = as.matrix(Nature)
CentresC2Nat = solve(t(mIC2Nat) %*% mIC2Nat) %*% t(mIC2Nat)%*% mNat
KMNat = kmeans(Nature, CentresC2Nat)
print(KMNat)

Nat <- cbind.data.frame(Villes[,1],Nature) 

CNat1 <- vector()
Cnat2  <- vector()
CNat3  <- vector()

a<-1
b<-1
c<-1

for (i in 1:100) {
  
  if (KMNat  $cluster[i] == 1){
    CNat1 [a] <- as.matrix(Nat[i,1])
    a <- a+1
  }
  
  if (KMNat  $cluster[i] == 2){
    Cnat2[b] <- as.matrix(Nat[i,1]) 
    b <- b+1
  }
  
  if (KMNat  $cluster[i] == 3){
    CNat3 [c] <- as.matrix(Nat[i,1])
    c <- c+1
  }
}

# Culture

Culture = data.frame(
  Villes$Musees,
  Villes$Cinema,
  Villes$MonumHistoriques,
  Villes$PretLivres,
  Villes$RestaurDistingues,
  Villes$Presse,
  Villes$Etudiants
)

dvCul = dist(Culture, method = "euclidean")
CAHVCul = hclust(d=dvCul, method = "ward.D")
plot(CAHVCul)
rect.hclust(CAHVCul,2)
PCul = cutree(tree = CAHVCul, k=3)
IC2Cul = data.frame(model.matrix(~as.factor(PCul)-1))
mIC2Cul = as.matrix(IC2Cul)
mCul = as.matrix(Culture)
CentresC2Cul = solve(t(mIC2Cul) %*% mIC2Cul) %*% t(mIC2Cul)%*% mCul
KMCul = kmeans(Culture, CentresC2Cul)
print(KMCul)

Cult <- cbind.data.frame(Villes[,1],Culture) 

Ccult1 <- vector()
Ccult2  <- vector()
Ccult3  <- vector()

a<-1
b<-1
c<-1

for (i in 1:100) {
  
  if (KMCul  $cluster[i] == 1){
    Ccult1  [a] <- as.matrix(Cult[i,1])
    a <- a+1
  }
  
  if (KMCul  $cluster[i] == 2){
    Ccult2 [b] <- as.matrix(Cult[i,1]) 
    b <- b+1
  }
  
  if (KMCul $cluster[i] == 3){
    Ccult3 [c] <- as.matrix(Cult[i,1])
    c <- c+1
  }
}

























