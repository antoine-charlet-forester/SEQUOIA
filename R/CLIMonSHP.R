# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("rlang")) {install.packages("rlang")}

CLIMonSHP <- function(repshp = T, repRdata = T){
  if(isFALSE(repshp)) {repshp <- tk_choose.files(caption = "Choisir le fichier .shp")}
  if(isFALSE(repRdata)) {repRdata <- tk_choose.dir(caption = "Choisir le dossier contenant les données .Rdata")}
  if (is_empty(repshp) || is_empty(repRdata)){
    warning("Pas de répertoire >> Traitement annulé")
  } else {

    # Lecture du fichier .shp
    SHP <- st_read(repshp, options = "ENCODING=UTF-8", quiet=T, agr='constant')

    # Téléchargement des données IFN
    RFN_sf_polygon <- SEQUOIA:::IFNtoSHP('RFN')
    SER_sf_polygon <- SEQUOIA:::IFNtoSHP('SER')

    METEOFRANCE_df <- SEQUOIA:::METEOFRANCEonSHP(repshp)
    assign("METEOFRANCE_df", METEOFRANCE_df, envir=globalenv())

    AURELHY_df     <- SEQUOIA:::AURELHYonSHP(repshp, repRdata)
    assign("AURELHY_df", AURELHY_df, envir=globalenv())

    DIGITALIS_df   <- SEQUOIA:::DIGITALISonSHP(repshp, repRdata)
    assign("DIGITALIS_df", DIGITALIS_df, envir=globalenv())

    # Détermination de la SER & de la RFN
    JOIN <- function(a, b, t){
      a <- st_transform(a, st_crs(b))       # Changement de système de coordonnées
      a[t] <- t                             # Création d'un champs spécial
      if(t=="SER"){n <- c(2,3)} else {n <- c(6,7)}
      df <- unique(as.data.frame(st_join(a[t], b))[,n]) # Union par attribut
      names(df) <- c(paste0(t,"_CODE"),paste0(t,"_NOM"))# Changement des intitulés de colonnes
      return(df)
    }

    SER_df <- JOIN(SHP, SER_sf_polygon, "SER")
    RFN_df <- JOIN(SHP, RFN_sf_polygon, "RFN")
    assign("SER_df", SER_df, envir=globalenv())
    assign("RFN_df", RFN_df, envir=globalenv())
  }
}
