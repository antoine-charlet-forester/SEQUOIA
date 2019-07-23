# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("raster")) {install.packages("raster")}
if (!require("rgdal")) {install.packages("rgdal")}
if (!require("sp")) {install.packages("sp")}

DIGITALISonSHP <- function(repshp = T, repRdata = T){
  if(isFALSE(repshp)) {repshp <- tk_choose.files(caption = "Choisir le fichier .shp")}
  if(isFALSE(repRdata)) {repRdata <- tk_choose.dir(caption = "Choisir le dossier contenant les données CLIM.Rdata")}
  if (is_empty(repshp) || is_empty(repRdata)){
    warning("Pas de répertoire >> Traitement annulé")
  } else {
    # Lecture des fichiers
    SHP <- st_read(repshp, options = "ENCODING=UTF-8", quiet=T, agr='constant')
    load(paste(repRdata, "DIGITALIS.Rdata", sep="/"))

    # Extraction des données
    DIGITALIS_df <- data.frame()
    for (i in 1:length(DIGITALIS_listname)){
      name <- DIGITALIS_listname[[i]][1]

      r <- DIGITALIS_list[[i]]
      v <- sp::spTransform(as(st_buffer(st_combine(SHP), 2500, nQuadSegs = 30), 'Spatial'), raster::crs(r))
      r.vals <- raster::extract(r, v)
      r.mean <- lapply(r.vals, FUN=mean)[[1]][1]

      df <- data.frame(VAR=name, VAL=r.mean)
      cat("La variable ", name, "a été calculé \n")

      DIGITALIS_df <- rbind(DIGITALIS_df, df)
    }

    # Sortie du tableur
    cat("Le tableur DIGITALIS_df a été exporté \n")
    return(DIGITALIS_df)
  }
}




