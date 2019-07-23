# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("stringr")) {install.packages("stringr")}

UAtoTOPO <- function(rep=T) {
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp contenant le parcellaire cadastral")
  }

  # Préparation de la sortie du fichier
  NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_UA')[1,1]-1)
  assign("NAME", NAME, envir=globalenv())

  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  assign("repout2", repout2, envir=globalenv())
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  assign("repout3", repout3, envir=globalenv())

  # Création des contours
  SEQUOIA::MNTonSHP(rep, paste0(NAME,"_TOPO_line"))

}
