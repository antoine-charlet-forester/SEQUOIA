# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("googledrive")) {install.packages("googledrive")}

BDTOPO_HYDROtoSHP <- function(repBD = T, sub = NULL){
  if(isFALSE(repBD)) {repRdata <- tk_choose.dir(caption = "Choisir le dossier de téléchargement de l'IGN (c) BD TOPO (r) Hydrographie")}
  if(!is.null(sub)){
    repBD <- paste(repBD, sub, sep="/")
  }

  # Téléchargement des données METEOFRANCE
  URL <- "https://drive.google.com/file/d/17ln_TH416HzwNKS9m-HTFcmZ0E7B-d8s/view?usp=sharing"
  temp <- tempfile(fileext = ".zip")
  message("Si un message 'sélection' apparrait, tapez 1 \n")
  dl <- drive_download(as_id(URL), path = temp, overwrite = TRUE)
  out <- unzip(temp, exdir = repBD)
  cat("L'IGN (c) BD TOPO (r) Hydrographie a été téléchargé et extraite dans ", repBD, "\n")
}
