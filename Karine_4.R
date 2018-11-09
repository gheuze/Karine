# ANALYSE-RETRAIT DES DOUBLONS -- REVELA -- script 4  -----------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)

setwd("\\\\sd\\ars13dfs\\ORGANISATION\\DSPE\\DVSS\\SCIRE\\COMMUN\\REVELA13\\Voozanoo\\Fichiers_Guillaume")

load("rep_travail/extraction_donnees_pseudo-doublon.RData")
# IMPORT DES DONNEES ------------------------------------------------------

fichier_valide <- read.csv2("sorties/pour_verif_2_le_retour.csv")
fichier_valide$meme_personne_0.1 <- as.logical(fichier_valide$meme_personne_0.1)

pour_verif <- rbind(pour_verif_5,pour_verif_6)
pour_verif <- matrix(pour_verif[fichier_valide$meme_personne_0.1,],ncol = ncol(pour_verif)) # on ne garde que les lignes validées

ligne_a_supprimer <- pour_verif[,1]

Vooz_doublon <- BDD_nouveaux[ligne_a_supprimer,]
BDD_nouveaux <- BDD_nouveaux[-ligne_a_supprimer,]

write.csv2(BDD_nouveaux, "sorties/result-final/nouveaux_patients_DIM.csv", row.names = F)
write.csv2(Vooz_doublon, "sorties/result-final/Vooz_doublon.csv", row.names = F)


# THAT'S ALL FOLKS !!!!!!!!! ----------------------------------------------