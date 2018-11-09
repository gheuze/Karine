
# création de la fonction prenom --------------------------------------------------------------

prenom <- function(prenom1, prenom2) {
  ifelse(nchar(prenom1) < nchar(prenom2), 
         {A <- prenom1 ; B <- prenom2}, # dans A, on met le prénom le plus court
         {B <- prenom1 ; A <- prenom2} # et dans B le plus long. Si même longueur, on s'en fiche
         )
  
  somme <- str_split(A, "", simplify = T) %>% # on découpe A en caractères, on a un vecteur en résultat (simplify = T)
    str_detect(B,.) %>% # on regarde si on retrouve les différents caractères dans le 2e vecteur, vecteur logique en résultat
    sum(.) # on somme pour comparer par rapport à la longueur
  
  return(somme %in% (nchar(A)-1):(nchar(A))) # si on retrouve toutes les lettres sauf 1 => T, sinon, F
  
}


# creation de la fonction de calcul du score ------------------------------

# permet de gérer les erreurs de saisies entre les différentes sources pour une même personne. On compare un à un le sexe, le prénom, le nom, les jours
# mois et année de naissance et nom de jeune fille. Plus on s'approche de 7,
# plus les personnes ont des chances d'être les mêmes. 7 => exactement la même. On sortira un tableau des 5 et 6, c'est à dire avec 2 dissemblances


calcul_score <- function(dfA, dfB, i,j) {
  score <- 0
  
  # les plus simples
  
  if (dfA$sexe[i] == dfB$sexe[j]) score <- score + 1
  if (day(dfA$date_naiss[i]) == day(dfB$date_naiss[j])) score <- score + 1
  if (month(dfA$date_naiss[i]) == month(dfB$date_naiss[j])) score <- score + 1
  if (year(dfA$date_naiss[i]) == year(dfB$date_naiss[j])) score <- score + 1
  
  # gestion du prénom
  
  score <- score + prenom(dfA$prenom[i], dfB$prenom[j]) # on ajoute à score, 0 si les prénoms sont trop différents et 1 sinon
                                                        # voir fonction prenom()
  
  # maintenant, la gestion des noms et nom_JF avec possibilite d'inversion et de non remplissage
  
  # s'il y a des nJF pour les 2
  if ((!is.na(dfA$nom_jfille[i])) & (!is.na(dfB$nom_jfille[j]))) {
    if (dfA$nom[i] %in% c(dfB$nom[j], dfB$nom_jfille[j]))
      score <- score + 1  
    if (dfA$nom_jfille[i] %in% c(dfB$nom[j], dfB$nom_jfille[j]))
      score <- score + 1
    
    # si tout correspond avec une simple inversion entre nom et nJF,
    # il ne faut quand meme pas qu'ils sortent a 7, afin de les voir apparaître dans le fichier de choix
    # donc score défalqué de 1, sauf si nomA=nomB=nomJFA=nomJFB
  #   if ((dfA$nom[i] == dfB$nom_jfille[j] & dfA$nom_jfille[i] == dfB$nom[j]) &
  #       (dfA$nom[i] != dfB$nom[j] & dfA$nom_jfille[i] != dfB$nom_jfille[j])
  #       )
  #     score <- score - 1 
  #      # tout est commenté, car soustraction à mettre à la fin du script principal
  } 
  
  # s'il y a de nom de JF pour aucun
  if (is.na(dfA$nom_jfille[i]) & is.na(dfA$nom_jfille[j])) {
    score <- score + 1 # pour ne pas penaliser, car alors, score sur 6
    if (dfA$nom[i] == dfB$nom[j])
      score <- score + 1
  }
  
  # si l'un ou (exclusif) l'autre n'a pas de nom de jeune fille
  if(xor(is.na(dfA$nom_jfille[i]), is.na(dfA$nom_jfille[j]))) {
    
    score <- score + 1 # pour ne pas penaliser, car alors, score sur 6
    
    # pour retrouver l'indice sans nom_JF et l'affecter à x, l'autre à y
    ifelse(is.na(dfA$nom_jfille[i]),
           {x <- i ; y <- j},
           {x <- j ; y <- i}
           )
    
    # si le nom est present, on incremente
    if(dfA$nom[x] %in% c(dfB$nom[y], dfB$nom_jfille[y])) score <- score + 1
  }
  
    # soustraction pour les scores à 7 et des cas non identiques, pour vérification humaine
    if (score == 7 & dfA$ident[i] != dfB$ident[j]) score <- 6
  
  return(score)
}


# fonction pour retirer les espaces de debut et fin, les accents et remplacer les tirets par des espaces---------------------------------
modif <- function(texte){
  
  # retrait des accents
  texte <- gsub("['`^~\"]", " ", texte)
  texte <- iconv(texte, to="ASCII//TRANSLIT//IGNORE")
  texte <- gsub("['`^~\"]", "", texte)
  
  # échange des tirets par des espaces ---------------------------------------------------
  texte <- gsub("-", " ", texte)
  
  # suppression des espaces en debut ou fin, et les multiples au milieu
  texte <- gsub("\\s+", " ", gsub("^\\s+|\\s+$", "", texte))
  
  return(toupper(texte))
  
}





