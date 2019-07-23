### Ouverture library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("stringr")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("tcltk")}
if (!require("rlang")) {install.packages("tcltk")}

AURELHYtoRDATA <- function (repTAB=T){
  if(isFALSE(repTAB)) {repTAB <- tk_choose.dir(caption = "Choisir le dossier contenant les données AURELHY")}
  if(is_empty(repTAB)){
    warning("Pas de répertoire >> Traitement annulé")
  } else {
    # Création d'une liste des fichiers
    ListeFich <- list.files(repTAB, "*.TAB") # Création d'une liste de fichier .TAB
    list_TAB  <- list() # Création list vierge
    n <- 1 # Création du numéro de fichier
    if(length(ListeFich > 0)){ # Si la liste contient au moins un fichiers
      cat(paste0("Le dossier choisi contient ",length(ListeFich)," fichiers.\n"))
      for (i in ListeFich) { # Pour chaque fichier .TAB de la liste
        TAB <- st_read(paste(repTAB,i, sep="/"), quiet=T) # Chargement du fichier .TAB
        #assign(str_sub(i,gregexpr("_",i)[[1]][1]+1,gregexpr("_",i)[[1]][2]-1),TAB,envir=globalenv())
        list_TAB[[n]] <- TAB # Ajout du fichier à la liste à la position n
        cat(paste0("Le fichier n°",n," : ", i," a été ajouté à la liste \n"))
        n <- n + 1
      }
      AURELHY_list <- list_TAB
      
      # Enrigistrement du tout sous un .Rdata
      save(AURELHY_list, file = "AURELHY.Rdata")
      load("AURELHY.Rdata")
      cat(paste0("Les données AURELHY.Rdata ont été exportées dans", dirname(repTAB), "\n"))
    }
  }
}