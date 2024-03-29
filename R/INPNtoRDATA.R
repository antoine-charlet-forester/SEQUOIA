# Lancement des library
if (!require("sf")) {install.packages("sf")}
if (!require("stringr")) {install.packages("stringr")}

INPNtoRDATA <- function(rep){

# Adresses de téléchargement et de destination
  INPN_URL <- "https://inpn.mnhn.fr/docs/Shape"
  LISTE_ZONE <- c("znieff2", "znieff1", "zico", "sic", "zps", "apb", "aspim", "bpm","pn", "ripn", "pnr", "rb", "bios", "rnn", "rncfs", "rnr", "cen")
  TD = tempdir() # répertoire temporaire

# Téléchargement des données .shp depuis INPN
  for (a in LISTE_ZONE) {
    URL <- paste(INPN_URL, paste0(a,".zip"), sep="/")

    TF = tempfile(tmpdir=TD, fileext=".zip") # fichier temporaire
    download.file(URL, TF, method="libcurl") # Téléchargement du fichier URL sous le nom TF dans TD
    unzip(zipfile = TF, exdir=TD) # Extraction de TF dans TD
    cat("Le fichier", a, "a été téléchargé", "\n")
  }

# Détection des . shp et création des listes vierges
  list_dirs <- list.dirs(TD)
  cat(length(list_dirs), "dossiers ont été détectés \n")

  list_INPN     <- list()
  list_INPN_NOM <- list()
  d <- 1 # Variable d'indexation dans les listes

# Lecture des .shp dans les dossiers
  for (b in list_dirs) {
    list_shp <- list.files(b, "*.shp$")

    for (c in list_shp) {
      SHP <- sf::st_read(paste(b, c, sep="/"), options = "ENCODING=UTF-8", quiet=T) # Lecture du fichier .shp
      SHP <- sf::st_transform(SHP, 2154)

      SUPR <- c(".shp","N_ENP_","_S_000","1712","2013","2013_09")
      NAME <- c
      for (e in 1:length(SUPR)) {
        NAME <- stringr::str_replace(NAME, SUPR[e],"")
      }
      NOM <- paste0("INPN_", toupper(NAME),"_polygone")

      list_INPN[[d]] <- SHP # Ajout du .shp à la liste
      list_INPN_NOM[[d]] <- NOM # Ajout du nom à la liste

      d=d+1 # Avancé dans l'index

      cat("Le fichier", NOM, "a été ajouté à l'archive", "\n", "\n")
    }
  }

# Enrigistrement des liste sous un .Rdata
  save(list_INPN, list_INPN_NOM, file=paste(rep,"INPN.Rdata",sep="/"))
  cat("Les", d-1, "fichiers téléchargés ont été sauvegardés dans ", rep, "\n")
  assign("list_INPN",list_INPN,envir=globalenv())
  assign("list_INPN_NOM",list_INPN_NOM,envir=globalenv())

}



