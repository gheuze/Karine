
# ANALYSE-RETRAIT DES DOUBLONS -- REVELA -- script 2  -----------------------------------------------------

# install.packages("lubridate")
library(tidyverse)
library(lubridate)
library(readxl)

setwd("\\\\sd\\ars13dfs\\ORGANISATION\\DSPE\\DVSS\\SCIRE\\COMMUN\\REVELA13\\Voozanoo\\Fichiers_Guillaume")

# IMPORT DES DONNEES ------------------------------------------------------
load("rep_travail/extraction_donnees_pseudo-doublon.RData")

fichier_valide <- read.csv2("sorties/pour_verif.csv")
# fichier_valide$meme_personne_0.1.2 <- factor(fichier_valide$meme_personne_0.1.2, levels = 0:2)


# pour_verif <- rbind(pour_verif_5,pour_verif_6)

pour_verif <- matrix(pour_verif[fichier_valide$meme_personne_0.1.2 %in% 1:2,], ncol = ncol(pour_verif)) # on ne garde que les lignes validees
  # on passe par matrix, en lui imposant le nb de colonnes, pour eviter d'avoir un vecteur s'il n'y a qu'une personne qui ressort
fichier_valide <- fichier_valide[fichier_valide$meme_personne_0.1.2 != 0,] # idem, on ne garde que les lignes validees 


totalement_meme_patient <- which(mat_score == 7, arr.ind = T) # arr.ind pour qu'il donne les indices des lignes/colonnes

liste <- NULL # initialisation
for (i in 1:dim(pour_verif)[1]){
  liste <- c(liste, pour_verif[i,(fichier_valide$meme_personne_0.1.2 %% 2 + 1)[i]]) # on passe par un modulo pour inverser 1 et 2, c'est a dire pour enlever la colonne non voulue 
  # si on a repondu "1", c'est que l'on veut enlever "2"
} 
        
ligne_a_supprimer <- c(liste,totalement_meme_patient[,2])

cas_doublon <- donnees[ligne_a_supprimer,]
donnees <- donnees[-ligne_a_supprimer,]

write.csv2(donnees, "sorties/donnees_sans_doublon.csv", row.names = F)
write.csv2(cas_doublon, "sorties/tableau_doublon_intra_DIM.csv", row.names = F)

# THAT'S ALL FOLKS !!!!!!!!! ----------------------------------------------