library("readxl")
library("tidyverse")

library("FactoMineR")
library("factoextra")
library("questionr")

library("ggplot2")

##----------------------------------------------------------------------------------------------------------------------------------##
##------------------------------------------------Préparation des données-----------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##

donnees = read_excel("BDD brute Confinement.xlsx")

vecSocioPro = donnees[,which(colnames(donnees) == "Votre catégorie socioprofessionnelle :")]

donneesTravailleur = donnees[-which(vecSocioPro == "Retraité" | vecSocioPro == "Etudiant" | vecSocioPro == "En recherche d'emploi" | vecSocioPro == "Autre sans activité professionnelle (personne au foyer...)"),]

listeVar = c()
listeVarCreer = c()

##Q7 - Situation familiale
`Situation familiale` = as.factor(donneesTravailleur$`Situation familiale`)
data = as.data.frame(`Situation familiale`)
listeVar = append(listeVar, "Situation familiale", after=length(listeVar))


##Q8 - Sexe du membre
`Sexe du membre` = as.factor(donneesTravailleur$`Sexe du membre`)
data = cbind(data, `Sexe du membre`)
listeVar = append(listeVar, "Sexe du membre", after=length(listeVar))


##Q9 - Nombre de personnes dans le foyer
`Nombre de personnes dans le foyer` = as.numeric(unlist(donneesTravailleur$`Nombre de personnes dans le foyer`))
`Tranche Nombre de personnes dans le foyer` = rep(NA, dim(donneesTravailleur)[1])
q1 = as.numeric(summary(`Nombre de personnes dans le foyer`)[2])
mediane = as.numeric(summary(`Nombre de personnes dans le foyer`)[3])
q3 = as.numeric(summary(`Nombre de personnes dans le foyer`)[5])
for(ind in 1:length(`Tranche Nombre de personnes dans le foyer`)){
  if(`Nombre de personnes dans le foyer`[ind] < q1){
    `Tranche Nombre de personnes dans le foyer`[ind] = paste("moins de",q1,sep=" ")
  }
  if(`Nombre de personnes dans le foyer`[ind] >= q1 & `Nombre de personnes dans le foyer`[ind] < mediane){
    `Tranche Nombre de personnes dans le foyer`[ind] = paste("entre",q1,"et",mediane,sep=" ")
  }
  if(`Nombre de personnes dans le foyer`[ind] >= mediane & `Nombre de personnes dans le foyer`[ind] < q3){
    `Tranche Nombre de personnes dans le foyer`[ind] = paste("entre",mediane,"et",q3,sep=" ")
  }
  if(`Nombre de personnes dans le foyer`[ind] >= q3){
    `Tranche Nombre de personnes dans le foyer`[ind] = paste("plus de",q3,sep=" ")
  }
}

`Tranche Nombre de personnes dans le foyer` = as.factor(`Tranche Nombre de personnes dans le foyer`)
data = cbind(data, `Tranche Nombre de personnes dans le foyer`)
listeVarCreer = append(listeVarCreer, "Tranche Nombre de personnes dans le foyer", after=length(listeVarCreer))


##Q10 - Nombre de 2rm conduits par le membre
`Nombre de 2rm conduits par le membre` = as.numeric(unlist(donneesTravailleur$`Nombre de 2rm conduits par le membre`))
`Tranche Nombre de 2rm conduits par le membre` = rep(NA, dim(donneesTravailleur)[1])

for(ind in 1:length(`Tranche Nombre de 2rm conduits par le membre`)){
  if(`Nombre de 2rm conduits par le membre`[ind] == 1){
    `Tranche Nombre de 2rm conduits par le membre`[ind] = "1"
  }
  if(`Nombre de 2rm conduits par le membre`[ind] == 2){
    `Tranche Nombre de 2rm conduits par le membre`[ind] = "2"
  }
  if(`Nombre de 2rm conduits par le membre`[ind] > 2){
    `Tranche Nombre de 2rm conduits par le membre`[ind] = "Plus de 2"
  }
}
`Tranche Nombre de 2rm conduits par le membre` = as.factor(`Tranche Nombre de 2rm conduits par le membre`)
data = cbind(data, `Tranche Nombre de 2rm conduits par le membre`)
listeVarCreer = append(listeVarCreer, "Tranche Nombre de 2rm conduits par le membre", after=length(listeVarCreer))


##Q13 - CSP du membre
`CSP du membre` = as.factor(donneesTravailleur$`CSP du membre`)
data = cbind(data, `CSP du membre`)
listeVar = append(listeVar, "CSP du membre", after=length(listeVar))

##Q14 : Q20 - variable nombre d'année du premier permis
tempTablePermis = donneesTravailleur[,which(colnames(donneesTravailleur) == "Année d'obtention Permis A" | colnames(donneesTravailleur) == "Année d'obtention Permis A1-AL" | colnames(donneesTravailleur) == "Année d'obtention Formation 125" | colnames(donneesTravailleur) == "Année d'obtention Permis A2" | colnames(donneesTravailleur) == "Année d'obtention Permis A3" | colnames(donneesTravailleur) == "Année d'obtention BSR cyclo" | colnames(donneesTravailleur) == "Année d'obtention BSR quad")]

tempTablePermis$`Année d'obtention Permis A`[is.na(tempTablePermis$`Année d'obtention Permis A`)] <- -1
tempTablePermis$`Année d'obtention Permis A1-AL`[is.na(tempTablePermis$`Année d'obtention Permis A1-AL`)] <- -1
tempTablePermis$`Année d'obtention Formation 125`[is.na(tempTablePermis$`Année d'obtention Formation 125`)] <- -1
tempTablePermis$`Année d'obtention Permis A2`[is.na(tempTablePermis$`Année d'obtention Permis A2`)] <- -1
tempTablePermis$`Année d'obtention Permis A3`[is.na(tempTablePermis$`Année d'obtention Permis A3`)] <- -1
tempTablePermis$`Année d'obtention BSR cyclo`[is.na(tempTablePermis$`Année d'obtention BSR cyclo`)] <- -1
tempTablePermis$`Année d'obtention BSR quad`[is.na(tempTablePermis$`Année d'obtention BSR quad`)] <- -1

`NB Année Premier Permis` = rep(2020, dim(tempTablePermis)[1])
`Tranche NB Année Premier Permis` = rep(NA, dim(tempTablePermis)[1])

for(ind in 1:length(`NB Année Premier Permis`)){
  min = 2020
  for(colonne in 1:dim(tempTablePermis)[2]){
    if(as.numeric(tempTablePermis[ind,colonne]) > 1920 & as.numeric(tempTablePermis[ind,colonne]) < min ){
      min = as.numeric(tempTablePermis[ind,colonne])
    }
  }
  `NB Année Premier Permis`[ind] = 2020 - min
  if(`NB Année Premier Permis`[ind] >= 0 & `NB Année Premier Permis`[ind] < 5){
    `Tranche NB Année Premier Permis`[ind] = "moin de 5ans"
  }
  if(`NB Année Premier Permis`[ind] >= 5 & `NB Année Premier Permis`[ind] < 10){
    `Tranche NB Année Premier Permis`[ind] = "entre 5 et 10 ans"
  }
  if(`NB Année Premier Permis`[ind] >= 10 & `NB Année Premier Permis`[ind] < 20){
    `Tranche NB Année Premier Permis`[ind] = "entre 10 et 20 ans"
  }
  if(`NB Année Premier Permis`[ind] >= 20){
    `Tranche NB Année Premier Permis`[ind] = "plus de 20 ans"
  }
}

`Tranche NB Année Premier Permis` = as.factor(`Tranche NB Année Premier Permis`)
data = cbind(data, `Tranche NB Année Premier Permis`)
listeVarCreer = append(listeVarCreer, "Tranche NB Année Premier Permis", after = length(listeVarCreer))


##Q22 - Age du membre
`Tranche âge` = as.factor(donneesTravailleur$Tranche_âge)
data = cbind(data, `Tranche âge`)
listeVar = append(listeVar, "Tranche âge",after = length(listeVar))


##Q38 : Q42 - variable associant la fréquence d'utilisation de 2RM façon hiérarchique sur les 5 2RM
tempTableFrequence = donneesTravailleur[,which(colnames(donneesTravailleur) == "Fréquence d'utilisation du 2rm 1" | colnames(donneesTravailleur) == "Fréquence d'utilisation du 2rm 2" | colnames(donneesTravailleur) == "Fréquence d'utilisation du 2rm 3" | colnames(donneesTravailleur) == "Fréquence d'utilisation du 2rm 4" | colnames(donneesTravailleur) == "Fréquence d'utilisation du 2rm 5")]
tempTableFrequence = mutate(tempTableFrequence,
                            frequence_2rm = case_when(
                              (`Fréquence d'utilisation du 2rm 1` =="Tous les jours")==TRUE | (`Fréquence d'utilisation du 2rm 2` =="Tous les jours")==TRUE | (`Fréquence d'utilisation du 2rm 3` =="Tous les jours") == TRUE | (`Fréquence d'utilisation du 2rm 4` =="Tous les jours")==TRUE ~ "Tous les jours", 
                              (`Fréquence d'utilisation du 2rm 1` =="Quelques jours par mois")==TRUE | (`Fréquence d'utilisation du 2rm 2` =="Quelques jours par mois")==TRUE | (`Fréquence d'utilisation du 2rm 3` =="Quelques jours par mois") == TRUE | (`Fréquence d'utilisation du 2rm 4` =="Quelques jours par mois")==TRUE ~ "Quelques jours par mois", 
                              (`Fréquence d'utilisation du 2rm 1` =="Quelques fois par semaine")==TRUE | (`Fréquence d'utilisation du 2rm 2` =="Quelques fois par semaine")==TRUE | (`Fréquence d'utilisation du 2rm 3` =="Quelques fois par semaine") == TRUE | (`Fréquence d'utilisation du 2rm 4` =="Quelques fois par semaine")==TRUE ~ "Quelques fois par semaine",
                              (`Fréquence d'utilisation du 2rm 1` =="Quelques fois par mois")==TRUE | (`Fréquence d'utilisation du 2rm 2` =="Quelques fois par mois")==TRUE | (`Fréquence d'utilisation du 2rm 3` =="Quelques fois par mois") == TRUE | (`Fréquence d'utilisation du 2rm 4` =="Quelques fois par mois")==TRUE ~ "Quelques fois par mois",
                              TRUE ~ "Quelques fois par an"
                            ),
                            frequence_2rm = as.factor(frequence_2rm)
)
`Fréquence d'utilisation de 2rm` = as.factor(tempTableFrequence$frequence_2rm)
data = cbind(data, `Fréquence d'utilisation de 2rm`)
listeVarCreer = append(listeVarCreer, "Fréquence d'utilisation de 2rm",after = length(listeVarCreer))


##Q53 - Nombre de kilomètres parcourus par an sur l'ensemble des 2RM
`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM` = donneesTravailleur[,"Nombre de kilomètres parcourus par an sur l'ensemble des 2RM"]
`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[is.na(`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`)] <- "0"
`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM` = as.numeric(unlist(`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`))
`Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM` = rep(NA, dim(donneesTravailleur)[1])
q1 = as.numeric(summary(`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`)[2])
mediane = as.numeric(summary(`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`)[3])
q3 = as.numeric(summary(`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`)[5])
for(ind in 1:length(`Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM`)){
  if(`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[ind] < q1){
    `Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[ind] = paste("moins de",q1,sep=" ")
  }
  if(`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[ind] >= q1 & `Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[ind] < mediane){
    `Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[ind] = paste("entre",q1,"et",mediane,sep=" ")
  }
  if(`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[ind] >= mediane & `Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[ind] < q3){
    `Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[ind] = paste("entre",mediane,"et",q3,sep=" ")
  }
  if(`Nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[ind] >= q3){
    `Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM`[ind] = paste("plus de",q3,sep=" ")
  }
}

`Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM` = as.factor(`Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM`)
data = cbind(data, `Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM`)
listeVarCreer = append(listeVarCreer, "Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM",after = length(listeVarCreer))


##Q69 : Q73 - préparation variable chaque segment en colonne
tempTableSegment = donneesTravailleur[,which(colnames(donneesTravailleur) == "Segment du 2rm 1" | colnames(donneesTravailleur) == "Segment du 2rm 2" | colnames(donneesTravailleur) == "Segment du 2rm 3" | colnames(donneesTravailleur) == "Segment du 2rm 4" | colnames(donneesTravailleur) == "Segment du 2rm 5")]
tempTableSegment$`Segment du 2rm 1`[is.na(tempTableSegment$`Segment du 2rm 1`)] <- ""
tempTableSegment$`Segment du 2rm 2`[is.na(tempTableSegment$`Segment du 2rm 2`)] <- ""
tempTableSegment$`Segment du 2rm 3`[is.na(tempTableSegment$`Segment du 2rm 3`)] <- ""
tempTableSegment$`Segment du 2rm 4`[is.na(tempTableSegment$`Segment du 2rm 4`)] <- ""
tempTableSegment$`Segment du 2rm 5`[is.na(tempTableSegment$`Segment du 2rm 5`)] <- ""
tempTableSegment = mutate(tempTableSegment,
                          `Possede Custom` = ifelse((tempTableSegment$`Segment du 2rm 1` == "Custom")
                                                    |(tempTableSegment$`Segment du 2rm 2` == "Custom")
                                                    |(tempTableSegment$`Segment du 2rm 3` == "Custom")
                                                    |(tempTableSegment$`Segment du 2rm 4` == "Custom")
                                                    |(tempTableSegment$`Segment du 2rm 5` == "Custom")
                                                    ,"Oui","Non"),
                          `Possede Quad` = ifelse((tempTableSegment$`Segment du 2rm 1` == "Quad")
                                                  |(tempTableSegment$`Segment du 2rm 2` == "Quad")
                                                  |(tempTableSegment$`Segment du 2rm 3` == "Quad")
                                                  |(tempTableSegment$`Segment du 2rm 4` == "Quad")
                                                  |(tempTableSegment$`Segment du 2rm 5` == "Quad")
                                                  ,"Oui","Non"),
                          `Possede Roadster` = ifelse((tempTableSegment$`Segment du 2rm 1` == "Roadster")
                                                      |(tempTableSegment$`Segment du 2rm 2` == "Roadster")
                                                      |(tempTableSegment$`Segment du 2rm 3` == "Roadster")
                                                      |(tempTableSegment$`Segment du 2rm 4` == "Roadster")
                                                      |(tempTableSegment$`Segment du 2rm 5` == "Roadster")
                                                      ,"Oui","Non"),
                          `Possede Routière / GT` = ifelse((tempTableSegment$`Segment du 2rm 1` == "Routière / GT")
                                                           |(tempTableSegment$`Segment du 2rm 2` == "Routière / GT")
                                                           |(tempTableSegment$`Segment du 2rm 3` == "Routière / GT")
                                                           |(tempTableSegment$`Segment du 2rm 4` == "Routière / GT")
                                                           |(tempTableSegment$`Segment du 2rm 5` == "Routière / GT")
                                                           ,"Oui","Non"),
                          `Possede Scooter` = ifelse((tempTableSegment$`Segment du 2rm 1` == "Scooter")
                                                     |(tempTableSegment$`Segment du 2rm 2` == "Scooter")
                                                     |(tempTableSegment$`Segment du 2rm 3` == "Scooter")
                                                     |(tempTableSegment$`Segment du 2rm 4` == "Scooter")
                                                     |(tempTableSegment$`Segment du 2rm 5` == "Scooter")
                                                     ,"Oui","Non"),
                          `Possede Side-car / Trike` = ifelse((tempTableSegment$`Segment du 2rm 1` == "Side-car / Trike")
                                                              |(tempTableSegment$`Segment du 2rm 2` == "Side-car / Trike")
                                                              |(tempTableSegment$`Segment du 2rm 3` == "Side-car / Trike")
                                                              |(tempTableSegment$`Segment du 2rm 4` == "Side-car / Trike")
                                                              |(tempTableSegment$`Segment du 2rm 5` == "Side-car / Trike")
                                                              ,"Oui","Non"),
                          `Possede Sportive` = ifelse((tempTableSegment$`Segment du 2rm 1` == "Sportive")
                                                      |(tempTableSegment$`Segment du 2rm 2` == "Sportive")
                                                      |(tempTableSegment$`Segment du 2rm 3` == "Sportive")
                                                      |(tempTableSegment$`Segment du 2rm 4` == "Sportive")
                                                      |(tempTableSegment$`Segment du 2rm 5` == "Sportive")
                                                      ,"Oui","Non"),
                          `Possede Tout terrain (Trial,  Enduro, Cross)` = ifelse((tempTableSegment$`Segment du 2rm 1` == "Tout terrain (Trial,  Enduro, Cross)")
                                                                                  |(tempTableSegment$`Segment du 2rm 2` == "Tout terrain (Trial,  Enduro, Cross)")
                                                                                  |(tempTableSegment$`Segment du 2rm 3` == "Tout terrain (Trial,  Enduro, Cross)")
                                                                                  |(tempTableSegment$`Segment du 2rm 4` == "Tout terrain (Trial,  Enduro, Cross)")
                                                                                  |(tempTableSegment$`Segment du 2rm 5` == "Tout terrain (Trial,  Enduro, Cross)")
                                                                                  ,"Oui","Non"),
                          `Possede Trail / Supermotard` = ifelse((tempTableSegment$`Segment du 2rm 1` == "Trail / Supermotard")
                                                                 |(tempTableSegment$`Segment du 2rm 2` == "Trail / Supermotard")
                                                                 |(tempTableSegment$`Segment du 2rm 3` == "Trail / Supermotard")
                                                                 |(tempTableSegment$`Segment du 2rm 4` == "Trail / Supermotard")
                                                                 |(tempTableSegment$`Segment du 2rm 5` == "Trail / Supermotard")
                                                                 ,"Oui","Non"),
                          `Possede autre segment` = ifelse((tempTableSegment$`Segment du 2rm 1` == "")
                                                           |(tempTableSegment$`Segment du 2rm 2` == "")
                                                           |(tempTableSegment$`Segment du 2rm 3` == "")
                                                           |(tempTableSegment$`Segment du 2rm 4` == "")
                                                           |(tempTableSegment$`Segment du 2rm 5` == "")
                                                           ,"Oui","Non")
)
`Possede Custom` = as.factor(tempTableSegment$`Possede Custom`)
data = cbind(data, `Possede Custom`)
listeVarCreer = append(listeVarCreer, "Possede Custom", after=length(listeVarCreer))

`Possede Quad` = as.factor(tempTableSegment$`Possede Quad`)
data = cbind(data, `Possede Quad`)
listeVarCreer = append(listeVarCreer, "Possede Quad", after=length(listeVarCreer))

`Possede Roadster` = as.factor(tempTableSegment$`Possede Roadster`)
data = cbind(data, `Possede Roadster`)
listeVarCreer = append(listeVarCreer, "Possede Roadster", after=length(listeVarCreer))

`Possede Routière / GT` = as.factor(tempTableSegment$`Possede Routière / GT`)
data = cbind(data, `Possede Routière / GT`)
listeVarCreer = append(listeVarCreer, "Possede Routière / GT", after=length(listeVarCreer))

`Possede Scooter` = as.factor(tempTableSegment$`Possede Scooter`)
data = cbind(data, `Possede Scooter`)
listeVarCreer = append(listeVarCreer, "Possede Scooter", after=length(listeVarCreer))

`Possede Side-car / Trike` = as.factor(tempTableSegment$`Possede Side-car / Trike`)
data = cbind(data, `Possede Side-car / Trike`)
listeVarCreer = append(listeVarCreer, "Possede Side-car / Trike", after=length(listeVarCreer))

`Possede Sportive` = as.factor(tempTableSegment$`Possede Sportive`)
data = cbind(data, `Possede Sportive`)
listeVarCreer = append(listeVarCreer, "Possede Sportive", after=length(listeVarCreer))

`Possede Tout terrain (Trial,  Enduro, Cross)` = as.factor(tempTableSegment$`Possede Tout terrain (Trial,  Enduro, Cross)`)
data = cbind(data, `Possede Tout terrain (Trial,  Enduro, Cross)`)
listeVarCreer = append(listeVarCreer, "Possede Tout terrain (Trial,  Enduro, Cross)", after=length(listeVarCreer))

`Possede Trail / Supermotard` = as.factor(tempTableSegment$`Possede Trail / Supermotard`)
data = cbind(data, `Possede Trail / Supermotard`)
listeVarCreer = append(listeVarCreer, "Possede Trail / Supermotard", after=length(listeVarCreer))

`Possede autre segment` = as.factor(tempTableSegment$`Possede autre segment`)
data = cbind(data, `Possede autre segment`)
listeVarCreer = append(listeVarCreer, "Possede autre segment", after=length(listeVarCreer))


##Q80 : Q84 - préparation des variables sur usage travaille
tempTableUsage = donneesTravailleur[,which(colnames(donneesTravailleur) == "Usage du 2rm 1" | colnames(donneesTravailleur) == "Usage du 2rm 2" | colnames(donneesTravailleur) == "Usage du 2rm 3" | colnames(donneesTravailleur) == "Usage du 2rm 4" | colnames(donneesTravailleur) == "Usage du 2rm 5")]
tempTableUsage$`Usage du 2rm 1`[is.na(tempTableUsage$`Usage du 2rm 1`)] <- ""
tempTableUsage$`Usage du 2rm 2`[is.na(tempTableUsage$`Usage du 2rm 2`)] <- ""
tempTableUsage$`Usage du 2rm 3`[is.na(tempTableUsage$`Usage du 2rm 3`)] <- ""
tempTableUsage$`Usage du 2rm 4`[is.na(tempTableUsage$`Usage du 2rm 4`)] <- ""
tempTableUsage$`Usage du 2rm 5`[is.na(tempTableUsage$`Usage du 2rm 5`)] <- ""
tempTableUsage = mutate(tempTableUsage,
                        `Uniquement usage travail` = ifelse((tempTableUsage$`Usage du 2rm 1` == "Pour les trajets domicile travail")
                                                            &((tempTableUsage$`Usage du 2rm 2` == "Pour les trajets domicile travail")|(tempTableUsage$`Usage du 2rm 2` == "Non conducteur de 2rm 2") | (tempTableUsage$`Usage du 2rm 2` == ""))
                                                            &((tempTableUsage$`Usage du 2rm 3` == "Pour les trajets domicile travail")|(tempTableUsage$`Usage du 2rm 3` == "Non conducteur de 2rm 3") | (tempTableUsage$`Usage du 2rm 3` == ""))
                                                            &((tempTableUsage$`Usage du 2rm 4` == "Pour les trajets domicile travail")|(tempTableUsage$`Usage du 2rm 4` == "Non conducteur de 2rm 4") | (tempTableUsage$`Usage du 2rm 4` == ""))
                                                            &((tempTableUsage$`Usage du 2rm 5` == "Pour les trajets domicile travail")|(tempTableUsage$`Usage du 2rm 5` == "Non conducteur de 2rm 5") | (tempTableUsage$`Usage du 2rm 5` == ""))
                                                            ,"Oui","Non"),
                        `Au moin un 2rm Pour Travail` = ifelse((tempTableUsage$`Usage du 2rm 1` == "Pour les trajets domicile travail")
                                                               |((tempTableUsage$`Usage du 2rm 2` == "Pour les trajets domicile travail"))
                                                               |((tempTableUsage$`Usage du 2rm 3` == "Pour les trajets domicile travail"))
                                                               |((tempTableUsage$`Usage du 2rm 4` == "Pour les trajets domicile travail"))
                                                               |((tempTableUsage$`Usage du 2rm 5` == "Pour les trajets domicile travail"))
                                                               ,"Oui","Non")
)
`Uniquement usage travail` = as.factor(tempTableUsage$`Uniquement usage travail`)
data = cbind(data, `Uniquement usage travail`)
listeVarCreer = append(listeVarCreer, "Uniquement usage travail", after=length(listeVarCreer))

`Au moin un 2rm Pour Travail` = as.factor(tempTableUsage$`Au moin un 2rm Pour Travail`)
data = cbind(data, `Au moin un 2rm Pour Travail`)
listeVarCreer = append(listeVarCreer, "Au moin un 2rm Pour Travail", after=length(listeVarCreer))


##Q89 - Comment vous rendez-vous, généralement, au travail ?
donneesTravailleur$`A 2-roues motorisé - Avant le confinement`[is.na(donneesTravailleur$`A 2-roues motorisé - Avant le confinement`)] <- "Non"
donneesTravailleur$`A 2-roues motorisé - Pendant le confinement`[is.na(donneesTravailleur$`A 2-roues motorisé - Pendant le confinement`)] <- "Non"
donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`[is.na(donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`)] <- "Non"

`A 2-roues motorisé - Avant le confinement` = as.factor(donneesTravailleur$`A 2-roues motorisé - Avant le confinement`)
data = cbind(data, `A 2-roues motorisé - Avant le confinement`)
listeVar = append(listeVar, "A 2-roues motorisé - Avant le confinement", after = length(listeVar))

`A 2-roues motorisé - Pendant le confinement` = as.factor(donneesTravailleur$`A 2-roues motorisé - Pendant le confinement`)
data = cbind(data, `A 2-roues motorisé - Pendant le confinement`)
listeVar = append(listeVar, "A 2-roues motorisé - Pendant le confinement", after = length(listeVar))

`A 2-roues motorisé - Dans les mois qui vont suivre` = as.factor(donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`)


##Q94 - Le 2-roues motorisé que vous utiliserez dans les mois à venir est-il un 2-roues de fonction ou de service ?
donneesTravailleur$`Le 2-roues motorisé que vous utiliserez dans les mois à venir est-il un 2-roues de fonction ou de service ?`[is.na(donneesTravailleur$`Le 2-roues motorisé que vous utiliserez dans les mois à venir est-il un 2-roues de fonction ou de service ?`)] <- "Non"

`Le 2-roues motorisé que vous utiliserez dans les mois à venir est-il un 2-roues de fonction ou de service ?` = as.factor(donneesTravailleur$`Le 2-roues motorisé que vous utiliserez dans les mois à venir est-il un 2-roues de fonction ou de service ?`)
data = cbind(data, `Le 2-roues motorisé que vous utiliserez dans les mois à venir est-il un 2-roues de fonction ou de service ?`)
listeVar = append(listeVar, "Le 2-roues motorisé que vous utiliserez dans les mois à venir est-il un 2-roues de fonction ou de service ?", after = length(listeVar))


##Q98 - Qu'est-ce qui vous inciterait à aller travailler à 2-roues motorisé ?
donneesTravailleur$`La facilité de stationnement...143`[is.na(donneesTravailleur$`La facilité de stationnement...143`)] <- "Non"
`La facilité de stationnement` = as.factor(donneesTravailleur$`La facilité de stationnement...143`)
data = cbind(data, `La facilité de stationnement`)
listeVar = append(listeVar, "La facilité de stationnement", after = length(listeVar))

donneesTravailleur$`La praticité...144`[is.na(donneesTravailleur$`La praticité...144`)] <- "Non"
`La praticité` = as.factor(donneesTravailleur$`La praticité...144`)
data = cbind(data, `La praticité`)
listeVar = append(listeVar, "La praticité", after = length(listeVar))

donneesTravailleur$`La liberté...145`[is.na(donneesTravailleur$`La liberté...145`)] <- "Non"
`La liberté` = as.factor(donneesTravailleur$`La liberté...145`)
data = cbind(data, `La liberté`)
listeVar = append(listeVar, "La liberté", after = length(listeVar))

donneesTravailleur$`Le gain d'autonomie...146`[is.na(donneesTravailleur$`Le gain d'autonomie...146`)] <- "Non"
`Le gain d'autonomie` = as.factor(donneesTravailleur$`Le gain d'autonomie...146`)
data = cbind(data, `Le gain d'autonomie`)
listeVar = append(listeVar, "Le gain d'autonomie", after = length(listeVar))

donneesTravailleur$`Le gain de temps...147`[is.na(donneesTravailleur$`Le gain de temps...147`)] <- "Non"
`Le gain de temps` = as.factor(donneesTravailleur$`Le gain de temps...147`)
data = cbind(data, `Le gain de temps`)
listeVar = append(listeVar, "Le gain de temps", after = length(listeVar))

donneesTravailleur$`Le plaisir de rouler...148`[is.na(donneesTravailleur$`Le plaisir de rouler...148`)] <- "Non"
`Le plaisir de rouler` = as.factor(donneesTravailleur$`Le plaisir de rouler...148`)
data = cbind(data, `Le plaisir de rouler`)
listeVar = append(listeVar, "Le plaisir de rouler", after = length(listeVar))

donneesTravailleur$`Le fait de ne pas polluer`[is.na(donneesTravailleur$`Le fait de ne pas polluer`)] <- "Non"
`Le fait de ne pas polluer` = as.factor(donneesTravailleur$`Le fait de ne pas polluer`)
data = cbind(data, `Le fait de ne pas polluer`)
listeVar = append(listeVar, "Le fait de ne pas polluer", after = length(listeVar))

donneesTravailleur$`Un gain financier...150`[is.na(donneesTravailleur$`Un gain financier...150`)] <- "Non"
`Un gain financier` = as.factor(donneesTravailleur$`Un gain financier...150`)
data = cbind(data, `Un gain financier`)
listeVar = append(listeVar, "Un gain financier", after = length(listeVar))

donneesTravailleur$`L'apport pour votre santé`[is.na(donneesTravailleur$`L'apport pour votre santé`)] <- "Non"
`L'apport pour votre santé` = as.factor(donneesTravailleur$`L'apport pour votre santé`)
data = cbind(data, `L'apport pour votre santé`)
listeVar = append(listeVar, "L'apport pour votre santé", after = length(listeVar))


##Q102 - Si votre employeur vous donnait accès à un 2-roues motorisé pour tout ou partie de vos déplacements professionnels, que diriez-vous ?
donneesTravailleur$`Si votre employeur vous donnait accès à un 2-roues motorisé pour tout ou partie de vos déplacements professionnels, que diriez-vous ?`[is.na(donneesTravailleur$`Si votre employeur vous donnait accès à un 2-roues motorisé pour tout ou partie de vos déplacements professionnels, que diriez-vous ?`)] <- "Autre"
`Si votre employeur vous donnait accès à un 2-roues motorisé pour tout ou partie de vos déplacements professionnels, que diriez-vous ?` = as.factor(donneesTravailleur$`Si votre employeur vous donnait accès à un 2-roues motorisé pour tout ou partie de vos déplacements professionnels, que diriez-vous ?`)
data = cbind(data, `Si votre employeur vous donnait accès à un 2-roues motorisé pour tout ou partie de vos déplacements professionnels, que diriez-vous ?`)
listeVar = append(listeVar, "Si votre employeur vous donnait accès à un 2-roues motorisé pour tout ou partie de vos déplacements professionnels, que diriez-vous ?", after = length(listeVar))


##Q104 - Dans votre cas, [format:U|quels que soient vos déplacements], pensez-vous que le 2-roues motorisé est :
`une alternative intéressante aux transports en commun` = as.factor(donneesTravailleur$`Dans votre cas, quels que soient vos déplacements, pensez-vous que le 2-roues motorisé est : - une alternative intéressante aux transports en commun`)
`une alternative intéressante aux transports en commun`[is.na(`une alternative intéressante aux transports en commun`)]<- "Non"
data = cbind(data, `une alternative intéressante aux transports en commun`)
listeVar = append(listeVar, "une alternative intéressante aux transports en commun", after = length(listeVar))

`une alternative intéressante à la voiture` = as.factor(donneesTravailleur$`Dans votre cas, quels que soient vos déplacements, pensez-vous que le 2-roues motorisé est : - une alternative intéressante à la voiture`)
`une alternative intéressante à la voiture`[is.na(`une alternative intéressante à la voiture`)]<- "Non"
data = cbind(data, `une alternative intéressante à la voiture`)
listeVar = append(listeVar, "une alternative intéressante à la voiture", after = length(listeVar))

`une alternative intéressante au vélo` = as.factor(donneesTravailleur$`Dans votre cas, quels que soient vos déplacements, pensez-vous que le 2-roues motorisé est : - une alternative intéressante au vélo`)
`une alternative intéressante au vélo`[is.na(`une alternative intéressante au vélo`)]<- "Non"
data = cbind(data, `une alternative intéressante au vélo`)
listeVar = append(listeVar, "une alternative intéressante au vélo", after = length(listeVar))

`un bon moyen de déstresser`  = as.factor(donneesTravailleur$`Dans votre cas, quels que soient vos déplacements, pensez-vous que le 2-roues motorisé est : - un bon moyen de déstresser`)
`un bon moyen de déstresser`[is.na(`un bon moyen de déstresser`)]<- "Non"
data = cbind(data, `un bon moyen de déstresser`)
listeVar = append(listeVar, "un bon moyen de déstresser", after = length(listeVar))

`une activité ludique`  = as.factor(donneesTravailleur$`Dans votre cas, quels que soient vos déplacements, pensez-vous que le 2-roues motorisé est : - une activité ludique`)
`une activité ludique`[is.na(`une activité ludique`)]<- "Non"
data = cbind(data, `une activité ludique`)
listeVar = append(listeVar, "une activité ludique", after = length(listeVar))

`un moyen de se vider la tête`  = as.factor(donneesTravailleur$`Dans votre cas, quels que soient vos déplacements, pensez-vous que le 2-roues motorisé est : - un moyen de se vider la tête`)
`un moyen de se vider la tête`[is.na(`un moyen de se vider la tête`)]<- "Non"
data = cbind(data, `un moyen de se vider la tête`)
listeVar = append(listeVar, "un moyen de se vider la tête", after = length(listeVar))

`un moyen de se protéger du risque sanitaire`  = as.factor(donneesTravailleur$`Dans votre cas, quels que soient vos déplacements, pensez-vous que le 2-roues motorisé est : - un moyen de se protéger du risque sanitaire`)
`un moyen de se protéger du risque sanitaire`[is.na(`un moyen de se protéger du risque sanitaire`)]<- "Non"
data = cbind(data, `un moyen de se protéger du risque sanitaire`)
listeVar = append(listeVar, "un moyen de se protéger du risque sanitaire", after = length(listeVar))

`un moyen de lutter contre le risque sanitaire`  = as.factor(donneesTravailleur$`Dans votre cas, quels que soient vos déplacements, pensez-vous que le 2-roues motorisé est : - un moyen de lutter contre le risque sanitaire`)
`un moyen de lutter contre le risque sanitaire`[is.na(`un moyen de lutter contre le risque sanitaire`)]<- "Non"
data = cbind(data, `un moyen de lutter contre le risque sanitaire`)
listeVar = append(listeVar, "un moyen de lutter contre le risque sanitaire", after = length(listeVar))

`un moyen d'éviter la circulation`  = as.factor(donneesTravailleur$`Dans votre cas, quels que soient vos déplacements, pensez-vous que le 2-roues motorisé est : - un moyen d'éviter la circulation`)
`un moyen d'éviter la circulation`[is.na(`un moyen d'éviter la circulation`)]<- "Non"
data = cbind(data, `un moyen d'éviter la circulation`)
listeVar = append(listeVar, "un moyen d'éviter la circulation", after = length(listeVar))


##Q105 - La crise sanitaire a-t-elle eu un impact sur les revenus de votre foyer ?
`La crise sanitaire a-t-elle eu un impact sur les revenus de votre foyer ?` = as.factor(donneesTravailleur$`La crise sanitaire a-t-elle eu un impact sur les revenus de votre foyer ?`)
data = cbind(data, `La crise sanitaire a-t-elle eu un impact sur les revenus de votre foyer ?`)
listeVar = append(listeVar, "La crise sanitaire a-t-elle eu un impact sur les revenus de votre foyer ?", after = length(listeVar))


##Q108 - Aviez-vous l'intention de changer de 2-roues motorisé en 2020 ?
`Aviez-vous l'intention de changer de 2-roues motorisé en 2020 ?` = as.factor(donneesTravailleur$`Aviez-vous l'intention de changer de 2-roues motorisé en 2020 ?`)
data = cbind(data, `Aviez-vous l'intention de changer de 2-roues motorisé en 2020 ?`)
listeVar = append(listeVar, "Aviez-vous l'intention de changer de 2-roues motorisé en 2020 ?", after = length(listeVar))


##Q114 - Combien dépensez-vous en moyenne par an pour votre 2-roues (hors carburant) - Budget agrégé ?
`Budget 2RM agrégé` = as.numeric(unlist(donneesTravailleur$`Budget 2RM agrégé`))
`Tranche Budget 2RM agrégé` = rep(NA, dim(donneesTravailleur)[1])
q1 = as.numeric(summary(`Budget 2RM agrégé`)[2])
mediane = as.numeric(summary(`Budget 2RM agrégé`)[3])
q3 = as.numeric(summary(`Budget 2RM agrégé`)[5])
for(ind in 1:length(`Tranche Budget 2RM agrégé`)){
  if(`Budget 2RM agrégé`[ind] < q1){
    `Tranche Budget 2RM agrégé`[ind] = paste("moins de",q1,sep=" ")
  }
  if(`Budget 2RM agrégé`[ind] >= q1 & `Budget 2RM agrégé`[ind] < mediane){
    `Tranche Budget 2RM agrégé`[ind] = paste("entre",q1,"et",mediane,sep=" ")
  }
  if(`Budget 2RM agrégé`[ind] >= mediane & `Budget 2RM agrégé`[ind] < q3){
    `Tranche Budget 2RM agrégé`[ind] = paste("entre",mediane,"et",q3,sep=" ")
  }
  if(`Budget 2RM agrégé`[ind] >= q3){
    `Tranche Budget 2RM agrégé`[ind] = paste("plus de",q3,sep=" ")
  }
}
`Tranche Budget 2RM agrégé` = as.factor(`Tranche Budget 2RM agrégé`)
data = cbind(data, `Tranche Budget 2RM agrégé`)
listeVarCreer = append(listeVarCreer, "Tranche Budget 2RM agrégé",after = length(listeVarCreer))


##Q117 - Pensez-vous arrêter le 2-roues motorisé d'ici la fin de l'année ?
`Pensez-vous arrêter le 2-roues motorisé d'ici la fin de l'année ?` = as.factor(donneesTravailleur$`Pensez-vous arrêter le 2-roues motorisé d'ici la fin de l'année ?`)
data = cbind(data, `Pensez-vous arrêter le 2-roues motorisé d'ici la fin de l'année ?`)
listeVar = append(listeVar, "Pensez-vous arrêter le 2-roues motorisé d'ici la fin de l'année ?", after = length(listeVar))


##Q119 - Pendant le confinement, vous avez :
`regardé des road-trips / voyages à 2-roues - Moins` = donneesTravailleur$`Pendant le confinement, vous avez : - regardé des road-trips / voyages à 2-roues - Moins`
`regardé des road-trips / voyages à 2-roues - Moins`[is.na(`regardé des road-trips / voyages à 2-roues - Moins`)]<-"Non"
`regardé des road-trips / voyages à 2-roues - Moins` = as.factor(`regardé des road-trips / voyages à 2-roues - Moins`)
data = cbind(data, `regardé des road-trips / voyages à 2-roues - Moins`)
listeVar = append(listeVar, "regardé des road-trips / voyages à 2-roues - Moins",after = length(listeVar))

`regardé des road-trips / voyages à 2-roues - Comme avant` = donneesTravailleur$`Pendant le confinement, vous avez : - regardé des road-trips / voyages à 2-roues - Comme avant`
`regardé des road-trips / voyages à 2-roues - Comme avant`[is.na(`regardé des road-trips / voyages à 2-roues - Comme avant`)]<-"Non"
`regardé des road-trips / voyages à 2-roues - Comme avant` = as.factor(`regardé des road-trips / voyages à 2-roues - Comme avant`)
data = cbind(data, `regardé des road-trips / voyages à 2-roues - Comme avant`)
listeVar = append(listeVar, "regardé des road-trips / voyages à 2-roues - Comme avant",after = length(listeVar))

`regardé des road-trips / voyages à 2-roues - Plus` = donneesTravailleur$`Pendant le confinement, vous avez : - regardé des road-trips / voyages à 2-roues - Plus`
`regardé des road-trips / voyages à 2-roues - Plus`[is.na(`regardé des road-trips / voyages à 2-roues - Plus`)]<-"Non"
`regardé des road-trips / voyages à 2-roues - Plus` = as.factor(`regardé des road-trips / voyages à 2-roues - Plus`)
data = cbind(data, `regardé des road-trips / voyages à 2-roues - Plus`)
listeVar = append(listeVar, "regardé des road-trips / voyages à 2-roues - Plus",after = length(listeVar))

`regardé des road-trips / voyages à 2-roues - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes` = donneesTravailleur$`Pendant le confinement, vous avez : - regardé des road-trips / voyages à 2-roues - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes :`
`regardé des road-trips / voyages à 2-roues - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`[is.na(`regardé des road-trips / voyages à 2-roues - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`)]<-"Non"
`regardé des road-trips / voyages à 2-roues - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes` = as.factor(`regardé des road-trips / voyages à 2-roues - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`)
data = cbind(data, `regardé des road-trips / voyages à 2-roues - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`)
listeVar = append(listeVar, "regardé des road-trips / voyages à 2-roues - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes",after = length(listeVar))

`regardé des tutoriels 2-roues motorisé - Moins` = donneesTravailleur$`Pendant le confinement, vous avez : - regardé des tutoriels 2-roues motorisé - Moins`
`regardé des tutoriels 2-roues motorisé - Moins`[is.na(`regardé des tutoriels 2-roues motorisé - Moins`)]<-"Non"
`regardé des tutoriels 2-roues motorisé - Moins` = as.factor(`regardé des tutoriels 2-roues motorisé - Moins`)
data = cbind(data, `regardé des tutoriels 2-roues motorisé - Moins`)
listeVar = append(listeVar, "regardé des tutoriels 2-roues motorisé - Moins",after = length(listeVar))

`regardé des tutoriels 2-roues motorisé - Comme avant` = donneesTravailleur$`Pendant le confinement, vous avez : - regardé des tutoriels 2-roues motorisé - Comme avant`
`regardé des tutoriels 2-roues motorisé - Comme avant`[is.na(`regardé des tutoriels 2-roues motorisé - Comme avant`)]<-"Non"
`regardé des tutoriels 2-roues motorisé - Comme avant` = as.factor(`regardé des tutoriels 2-roues motorisé - Comme avant`)
data = cbind(data, `regardé des tutoriels 2-roues motorisé - Comme avant`)
listeVar = append(listeVar, "regardé des tutoriels 2-roues motorisé - Comme avant",after = length(listeVar))

`regardé des tutoriels 2-roues motorisé - Plus` = donneesTravailleur$`Pendant le confinement, vous avez : - regardé des tutoriels 2-roues motorisé - Plus`
`regardé des tutoriels 2-roues motorisé - Plus`[is.na(`regardé des tutoriels 2-roues motorisé - Plus`)]<-"Non"
`regardé des tutoriels 2-roues motorisé - Plus` = as.factor(`regardé des tutoriels 2-roues motorisé - Plus`)
data = cbind(data, `regardé des tutoriels 2-roues motorisé - Plus`)
listeVar = append(listeVar, "regardé des tutoriels 2-roues motorisé - Plus",after = length(listeVar))

`regardé des tutoriels 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes` = donneesTravailleur$`Pendant le confinement, vous avez : - regardé des tutoriels 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes :`
`regardé des tutoriels 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`[is.na(`regardé des tutoriels 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`)]<-"Non"
`regardé des tutoriels 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes` = as.factor(`regardé des tutoriels 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`)
data = cbind(data, `regardé des tutoriels 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`)
listeVar = append(listeVar, "regardé des tutoriels 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes", after = length(listeVar))

`bricolé votre 2-roues motorisé - Moins` = donneesTravailleur$`Pendant le confinement, vous avez : - bricolé votre 2-roues motorisé - Moins`
`bricolé votre 2-roues motorisé - Moins`[is.na(`bricolé votre 2-roues motorisé - Moins`)]<-"Non"
`bricolé votre 2-roues motorisé - Moins` = as.factor(`bricolé votre 2-roues motorisé - Moins`)
data = cbind(data, `bricolé votre 2-roues motorisé - Moins`)
listeVar = append(listeVar, "bricolé votre 2-roues motorisé - Moins", after = length(listeVar))

`bricolé votre 2-roues motorisé - Comme avant` = donneesTravailleur$`Pendant le confinement, vous avez : - bricolé votre 2-roues motorisé - Comme avant`
`bricolé votre 2-roues motorisé - Comme avant`[is.na(`bricolé votre 2-roues motorisé - Comme avant`)]<-"Non"
`bricolé votre 2-roues motorisé - Comme avant` = as.factor(`bricolé votre 2-roues motorisé - Comme avant`)
data = cbind(data, `bricolé votre 2-roues motorisé - Comme avant`)
listeVar = append(listeVar, "bricolé votre 2-roues motorisé - Comme avant", after = length(listeVar))

`bricolé votre 2-roues motorisé - Plus` = donneesTravailleur$`Pendant le confinement, vous avez : - bricolé votre 2-roues motorisé - Plus`
`bricolé votre 2-roues motorisé - Plus`[is.na(`bricolé votre 2-roues motorisé - Plus`)]<-"Non"
`bricolé votre 2-roues motorisé - Plus` = as.factor(`bricolé votre 2-roues motorisé - Plus`)
data = cbind(data, `bricolé votre 2-roues motorisé - Plus`)
listeVar = append(listeVar, "bricolé votre 2-roues motorisé - Plus", after = length(listeVar))

`bricolé votre 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes` = donneesTravailleur$`Pendant le confinement, vous avez : - bricolé votre 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes :`
`bricolé votre 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`[is.na(`bricolé votre 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`)]<-"Non"
`bricolé votre 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes` = as.factor(`bricolé votre 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`)
data = cbind(data, `bricolé votre 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes`)
listeVar = append(listeVar, "bricolé votre 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes", after = length(listeVar))


##Q121 - Avez-vous découvert une activité en lien avec le 2-roues motorisé au cours du confinement ?
`Avez-vous découvert une activité en lien avec le 2-roues motorisé au cours du confinement ?` = donneesTravailleur$`Avez-vous découvert une activité en lien avec le 2-roues motorisé au cours du confinement ?`
`Avez-vous découvert une activité en lien avec le 2-roues motorisé au cours du confinement ?`[`Avez-vous découvert une activité en lien avec le 2-roues motorisé au cours du confinement ?` == "Oui. Précisez"] <- "Oui"
`Avez-vous découvert une activité en lien avec le 2-roues motorisé au cours du confinement ?` = as.factor(`Avez-vous découvert une activité en lien avec le 2-roues motorisé au cours du confinement ?`)
data = cbind(data, `Avez-vous découvert une activité en lien avec le 2-roues motorisé au cours du confinement ?`)
listeVar = append(listeVar, "Avez-vous découvert une activité en lien avec le 2-roues motorisé au cours du confinement ?", after = length(listeVar))


##Q125 - Avez-vous ressenti le besoin de faire du 2-roues motorisé pendant le confinement ?
`Avez-vous ressenti le besoin de faire du 2-roues motorisé pendant le confinement ?` = as.factor(donneesTravailleur$`Avez-vous ressenti le besoin de faire du 2-roues motorisé pendant le confinement ?`)
data = cbind(data, `Avez-vous ressenti le besoin de faire du 2-roues motorisé pendant le confinement ?`)
listeVar = append(listeVar, "Avez-vous ressenti le besoin de faire du 2-roues motorisé pendant le confinement ?", after = length(listeVar))


##Q126 - Avez-vous ressenti un sentiment de frustration de ne pas pouvoir pratiquer votre passion pendant le confinement ?
`Avez-vous ressenti un sentiment de frustration de ne pas pouvoir pratiquer votre passion pendant le confinement ?` = as.factor(donneesTravailleur$`Avez-vous ressenti un sentiment de frustration de ne pas pouvoir pratiquer votre passion pendant le confinement ?`)
data = cbind(data, `Avez-vous ressenti un sentiment de frustration de ne pas pouvoir pratiquer votre passion pendant le confinement ?`)
listeVar = append(listeVar, "Avez-vous ressenti un sentiment de frustration de ne pas pouvoir pratiquer votre passion pendant le confinement ?", after = length(listeVar))


##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
colnames(data)

## Analyse
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------------------------##

## Segment de moto

# pas utilisé encore
vecSegment = c("Segment du 2rm 1", "Usage du 2rm 1")
ACMSegChgmt = CA(table(donneesTravailleur[vecSegment]))

## est ce que la personne va déjà en moto au travail

vecUsage = c("A 2-roues motorisé - Avant le confinement", "A 2-roues motorisé - Dans les mois qui vont suivre", 
             "A 2-roues motorisé - Pendant le confinement")

ACMSegToday = MCA(donneesTravailleur[vecUsage])
fviz_mca_var(ACMSegToday, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)

## Impact financier

vecImpactFin = c("La crise sanitaire a-t-elle eu un impact sur les revenus de votre foyer ?")

segment_fin = cbind(data[vecImpactFin], 
                    donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`)

ACMSegImpactFin = MCA(segment_fin, graph = FALSE)

fviz_mca_var(ACMSegImpactFin, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)




## Activité en liens avec la moto
vecActivite = c("Avez-vous découvert une activité en lien avec le 2-roues motorisé au cours du confinement ?")

segment_activite = cbind(data[vecActivite], 
                                 donneesTravailleur['A 2-roues motorisé - Dans les mois qui vont suivre'])

ACMSegActivite = MCA(segment_activite, graph = FALSE)
fviz_mca_var(ACMSegActivite, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)

## Quels activité ?

data = rename.variable(data, 
                       "bricolé votre 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes",
                       "bricolé votre 2-roues motorisé")

data = rename.variable(data, 
                       "regardé des road-trips / voyages à 2-roues - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes",
                       "regardé des road-trips / voyages à 2-roues")

data = rename.variable(data, 
                       "regardé des tutoriels 2-roues motorisé - Ce comportement va perdurer, pour vous, dans les mois à venir pour les activités suivantes",
                       "regardé des tutoriels 2-roues motorisé")

vecActivites = c("bricolé votre 2-roues motorisé",
                "regardé des road-trips / voyages à 2-roues", 
                 "regardé des tutoriels 2-roues motorisé")

segment_activites = cbind(data[vecActivites], 
                          donneesTravailleur['A 2-roues motorisé - Dans les mois qui vont suivre'])

ACMSegActivites = MCA(segment_activites, graph = FALSE)
fviz_mca_var(ACMSegActivites, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)


## Frustration/ Besoin de rouler

data = rename.variable(data, 
                        "Avez-vous ressenti le besoin de faire du 2-roues motorisé pendant le confinement ?",
                        "Besoin")

data = rename.variable(data, 
                        "Avez-vous ressenti un sentiment de frustration de ne pas pouvoir pratiquer votre passion pendant le confinement ?",
                       "Frustration")
vecBesoin = c("Besoin", "Frustration")

segment_besoin_frustration = cbind(data[vecBesoin], 
                     donneesTravailleur['A 2-roues motorisé - Dans les mois qui vont suivre'])


ACMSegBesoin = MCA(segment_besoin_frustration, graph = FALSE)
fviz_mca_var(ACMSegBesoin, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)

## Véhicule de service
# pas utilisé

vecUsage = c("Le 2-roues motorisé que vous utiliserez dans les mois à venir est-il un 2-roues de fonction ou de service ?")

segemnt_avant_apres_pendant = cbind(data[vecUsage], 
                                    donneesTravailleur['A 2-roues motorisé - Dans les mois qui vont suivre'])

ACMSegAvantApres = MCA(segemnt_avant_apres_pendant, graph = FALSE)
fviz_mca_var(ACMSegAvantApres, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)

## incitatif
vecIncitatif = c("La facilité de stationnement", "La liberté", "La praticité", 
                 "Le gain de temps", "Le plaisir de rouler", "Le gain d'autonomie")

segment_incitatif = cbind(data[vecIncitatif], 
                          donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`)

segment_incitatif = rename.variable(segment_incitatif, 
                    "donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`", 
                    "A 2-roues motorisé - Dans les mois qui vont suivre")

ACMIncitatif = MCA(segment_incitatif, graph = FALSE)
fviz_mca_var(ACMIncitatif, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)

## l'apport pour la santé variable sur incitatif à se rendre au travail à vélo
## pas utilisé
vecIncitatif = c("L'apport pour votre santé")

segment_incitatif = cbind(data[vecIncitatif], 
                          donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`)

ACMIncitatif = MCA(segment_incitatif, graph = FALSE)
fviz_mca_var(ACMIncitatif, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)

## Pensez vous que la moto est ?

vecRaison = c("un moyen de se protéger du risque sanitaire", "un moyen de lutter contre le risque sanitaire", 
              "un bon moyen de déstresser", "une alternative intéressante aux transports en commun",
              "une alternative intéressante à la voiture", "une alternative intéressante à la voiture", 
              "un moyen d'éviter la circulation")

segment_raison = cbind(data[vecRaison], 
                          donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`)

segment_raison = rename.variable(segment_raison, 
                                "donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`", 
                                "A 2-roues motorisé - Dans les mois qui vont suivre")

ACMRaison = MCA(segment_raison, graph = FALSE)
fviz_mca_var(ACMRaison, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)

## Arrêt 2-roues

vecArret = c("Arrêt 2RM")

segment_arret = cbind(data[vecArret], 
                       donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`)

segment_arret = rename.variable(segment_arret, 
  "donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`", 
                     "A 2-roues motorisé - Dans les mois qui vont suivre")

ACMArret = MCA(segment_arret, graph = FALSE)
fviz_mca_var(ACMArret, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)


## fréq / km


vecFreqKM = c("Fréquence d'utilisation de 2rm", "Tranche nombre de kilomètres parcourus par an sur l'ensemble des 2RM")

segment_freq_km = cbind(data[vecFreqKM], 
                      donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`)

segment_freq_km = rename.variable(segment_freq_km, 
                                "donneesTravailleur$`A 2-roues motorisé - Dans les mois qui vont suivre`", 
                                "A 2-roues motorisé - Dans les mois qui vont suivre")

ACMKMFreq = MCA(segment_freq_km, graph = FALSE)
fviz_mca_var(ACMKMFreq, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             invisible = "ind",
             repel = TRUE)
