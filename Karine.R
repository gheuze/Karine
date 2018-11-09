
# ANALYSE-RETRAIT DES DOUBLONS -- REVELA  -----------------------------------------------------

library(tidyverse)
library(lubridate)
library(readxl)
library(data.table)


# rapatriement des fonctions ------------------------------------------------------------------

source("I:\\DIRECTION ACTION TERRITORIALE DE SANTE\\VSSE\\CIRE\\Revela\\fonctions.R")


# IMPORT DES DONNEES ------------------------------------------------------

setwd("\\\\sd\\ars13dfs\\ORGANISATION\\DSPE\\DVSS\\SCIRE\\COMMUN\\REVELA13\\Voozanoo\\Fichiers_Guillaume")
donnees <- read.csv2("donnees/URO_TOUSDIM_2015_2016.csv")

donnees$Sexe <- factor(modif(donnees$Sexe)) 
donnees$Date.naissance <- dmy(donnees$Date.naissance)
donnees$NOM.Jeune.fille <- as.character(modif(donnees$NOM.Jeune.fille))
donnees$Nom <- as.character(modif(donnees$Nom))
donnees$PRENOM <- modif(donnees$PRENOM)

setnames(donnees, old=c("Nom","PRENOM","NOM.Jeune.fille","Date.naissance","Sexe"),
         new=c("nom","prenom","nom_jfille","date_naiss","sexe"))

# retrait des nomJF si ce sont les memes que les noms
donnees$nom_jfille[donnees$nom == donnees$nom_jfille] <- ""

# creation d'un identifiant
donnees$ident <- paste(donnees$nom, 
                           donnees$nom_jfille, 
                           donnees$prenom, 
                           donnees$sexe, 
                           donnees$date_naiss, 
                           sep = "_")

# donnees <- donnees[c(1:10,5,7),] # pour tests

n <- dim(donnees)[1]

# boucle de remplissage de la matrice de correspondance -------------------

mat_score <- matrix(NA, n, n)

system.time(for (i in 1:(n-1)){       # system.time() sert à connaître le temps mis, non indispensable
  for (j in (i+1):n){
    mat_score[i,j] <- calcul_score(donnees, donnees, i,j)
  }
  
})


# traitement des données pour sortie  ---------------------------------------------------------

pour_verif <- which(mat_score == 6, arr.ind = T)

sortie <- cbind(
  donnees[pour_verif[,1],c(1,3:7)],
  donnees[pour_verif[,2],c(1,3:7)]
)

# creation du fichier de sortie pour comparaison ----------------------------------------------
# on rajoute, pour info, le score en 1re colonne et un "NA" la fin, pour modification au besoin en 1 ou 2 ensuite

sortie <- cbind(
  c(rep(6, dim(pour_verif)[1])), # on passe par la, pour gerer les cas ou il y a des lignes de vides
  # car sortie_5 <- cbind(5, donnees[pour_verif_5[,1],3:7], donnees[pour_verif_5[,2],3:7], 0) ne fonctionne pas dans ce cas
  sortie,
  rep(NA, dim(pour_verif)[1])
)

names(sortie) <- c("score-pour_info", "etab_1", "nom_1", "prenom_1", "nom_JF_1", "sexe_1", "ddn_1",
                   "etab_2", "nom_2", "prenom_2", "nom_JF_2", "sexe_2", "ddn_2",
                   "meme_personne_0-1-2")
write.csv2(sortie,"sorties/pour_verif.csv", row.names = F)

save.image("rep_travail/extraction_donnees_pseudo-doublon.RData")

# THAT'S ALL FOLKS !!!!!!!!! ----------------------------------------------