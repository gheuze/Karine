# ANALYSE-RETRAIT DES DOUBLONS -- REVELA -- script 3  -----------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)

source("I:\\DIRECTION ACTION TERRITORIALE DE SANTE\\VSSE\\CIRE\\Revela\\fonctions.R")

setwd("\\\\sd\\ars13dfs\\ORGANISATION\\DSPE\\DVSS\\SCIRE\\COMMUN\\REVELA13\\Voozanoo\\Fichiers_Guillaume")

# IMPORT DES DONNEES ------------------------------------------------------
# données DIM
donnees_DIM <- read.csv2("sorties/donnees_sans_doublon.csv")
donnees_DIM$prenom <- as.character(donnees_DIM$prenom)
donnees_DIM$date_naiss <- ymd(donnees_DIM$date_naiss)
donnees_DIM$nom_jfille[donnees_DIM$nom_jfille == ""] <- NA

# données Vooz
donnees_Vooz <- read.csv2("donnees/REVELA_urinaires_tot.csv")

donnees_Vooz$sexe <- factor(donnees_Vooz$sexe)
levels(donnees_Vooz$sexe) <- c("M","F") # pour comparaison

donnees_Vooz$nom_jfille[donnees_Vooz$nom_jfille == ""] <- NA
donnees_Vooz$nom_jfille <- as.character(modif(donnees_Vooz$nom_jfille))
donnees_Vooz$nom <- as.character(modif(donnees_Vooz$nom))
donnees_Vooz$prenom <- as.character(modif(donnees_Vooz$prenom))

donnees_Vooz$date_naiss <- dmy(donnees_Vooz$date_naiss)

donnees_Vooz$nom_jfille[donnees_Vooz$nom == donnees_Vooz$nom_jfille] <- ""

# creation d'une variable pour comparaison rapide pour trouver les lignes communes sans erreur
donnees_Vooz$ident <- paste(donnees_Vooz$nom, 
                            donnees_Vooz$nom_jfille, 
                            donnees_Vooz$prenom, 
                            donnees_Vooz$sexe, 
                            donnees_Vooz$date_naiss, 
                            sep = "_")

# identification des patients communs ---------------------------------------------------------

liste_ident <- intersect(donnees_DIM$ident,donnees_Vooz$ident) # identifiant global pour patient pour comparaison
sortie_exact_meme_patient <- donnees_DIM[which(donnees_DIM$ident %in% liste_ident),]

# sortie des patients identiques entre DIM et Vooz
write.csv2(sortie_exact_meme_patient, "sorties/result-final/exact_meme_patient_DIM_Vooz.csv", row.names = F)

# df avec seulement les patients qui ne sont pas les memes (nouveaux + erreurs de saisie)
ifelse(
  dim(array(which(donnees_DIM$ident %in% liste_ident))) == 0, # si c'est vide, on passe par la, pas reussi a faire autrement
  BDD_nouveaux <- donnees_DIM,
  BDD_nouveaux <- donnees_DIM[-which(donnees_DIM$ident %in% liste_ident),]
)

# RECHERCHE DES PATIENTS AVEC ERREURS DE SAISIES ----------------------------------------------

# pour exclusion des erreurs de saisie, meme procedure que script 1, d'ou source(...) en intro
mat_score <- matrix(NA, dim(BDD_nouveaux)[1], dim(donnees_Vooz)[1])

system.time(
for (i in 1:dim(mat_score)[1]){
  for (j in 1:dim(mat_score)[2]){
    mat_score[i,j] <- calcul_score(BDD_nouveaux, donnees_Vooz, i, j)
  }
})


# on récupère les scores de 6/7
pour_verif <- which(mat_score == 6, arr.ind = T)
sortie <- cbind(
  BDD_nouveaux[pour_verif[,1],c(1,3:7)],
  donnees_Vooz[pour_verif[,2],c(2:4,6,5, 7)]
)

# creation du fichier de sortie pour comparaison ----------------------------------------------
# on rajoute, pour info, le score en 1re colonne et un "NA" a la fin, pour modification en 0 ou 1
sortie <- cbind(
  rep(6, dim(pour_verif)[1]), # on passe par la, pour gerer les cas ou il y a des lignes de vides
  # car sortie_5 <- cbind(5, donnees[pour_verif_5[,1],3:7], donnees[pour_verif_5[,2],3:7], 0) ne fonctionne pas dans ce cas
  sortie,
  rep(NA, dim(pour_verif)[1])
)

names(sortie) <- c("score-pour_info","etab_DIM", "nom_DIM","prenom_DIM","nom_JF_DIM","sexe_DIM","ddn_DIM",
                   "nom_Vooz","prenom_Vooz","nom_JF_Vooz","sexe_Vooz","ddn_Vooz", "etab_vooz",
                   "meme_personne_0-1")

write.csv2(sortie,"sorties/pour_verif_2_le_retour.csv", row.names = F)

save.image("rep_travail/extraction_donnees_pseudo-doublon.RData")

# THAT'S ALL FOLKS !!!!!!!!! ----------------------------------------------