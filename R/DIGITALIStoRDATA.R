# Ouverture library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("stringr")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("tcltk")}
if (!require("rlang")) {install.packages("tcltk")}
if (!require("raster")) {install.packages("raster")}
if (!require("rgdal")) {install.packages("rgdal")}
if (!require("XML")) {install.packages("XML")}
if (!require("installr")) {install.packages("installr")}

DIGITALIStoRDATA <- function (repDIG=T){
  if(isFALSE(repDIG)) {repDIG <- tk_choose.dir(caption = "Choisir le dossier où télécharger DIGITALIS")}
  if(is_empty(repDIG)){
    warning("Pas de répertoire >> Traitement annulé")
  } else {

    # Téléchargement des données

    URL <- "https://silvae.agroparistech.fr/home/?page_id=2683"
    HTML <- paste(readLines(url), collapse="\n")
    LIST_URL <- str_match_all(html, "<a href=\"(.*?)\"")[[1]][,2]
    LIST_URL <- LIST_URL[16:39]

    for (a in 1:length(LIST_URL)){
      TF = tempfile(repDIG, fileext=".7z") # fichier temporaire
      download.file(LIST_URL[[a]], TF, method="libcurl") # Téléchargement du fichier URL sous le nom TF dans TD
    }

    # Flute
    message("Désolé ma belle mais ce pauvre R ne gère pas les .7z file. \n",
            "Veuillez extraire les archives mannuellements ! \n")

    RES <- dlgMessage(message="Pret a continuer ?", type = "ok")$res

    # Création d'une liste des fichiers
    ListeFich <- list.files(repDIG, "*.tif$", recursive = T)

    if(!length(ListeFich)){
      Warning("Pas de fichiers extraits > Traitement annulé")
    } else {
      DIGITALIS_list <- list()
      DIGITALIS_listname <- list()
      if(length(ListeFich) > 0){ # Si la liste contient au moins un fichiers
        for (i in 1:length(ListeFich)) { # Boucle de lecture
          name <- str_sub(ListeFich[i], 0, str_locate_all(ListeFich[i],"/")[[1]][1,1]-1)
          DIGITALIS_listname[i] <- name
          r <- raster(paste(repDIG, ListeFich[i], sep="/"))
          DIGITALIS_list[[i]] <- r
          cat(paste0("Le fichier",name," a été ajouté à la liste \n"))
        } # Fin boucle de lecture

        # Enrigistrement du tout sous un .Rdata
        setwd(dirname(repDIG))
        save(DIGITALIS_list, DIGITALIS_listname, file = "DIGITALIS.Rdata")
        load("DIGITALIS.Rdata")
        cat(paste0("Les données DIGITALIS.Rdata ont été exportées dans", dirname(repDIG), "\n"))
    }
    }
  }
}
