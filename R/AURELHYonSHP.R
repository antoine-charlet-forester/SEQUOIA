# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("rlang")) {install.packages("tcltk")}

AURELHYonSHP <- function(repshp=T, repRdata=T){
  if(isFALSE(repshp)) {repshp <- tk_choose.files(caption = "Choisir le fichier .shp")}
  if(isFALSE(repRdata)) {repRdata <- tk_choose.dir(caption = "Choisir le dossier contenant les données AURELHY.Rdata")}
  if (is_empty(repshp) || is_empty(repRdata)){
    warning("Pas de répertoire >> Traitement annulé")
  } else {
    
    # Lecture des fichiers
    SHP <- st_read(repshp, options = "ENCODING=UTF-8", quiet=T, agr='constant')
    load(paste(repRdata, "AURELHY.Rdata", sep="/"))
    
    # Création de l'emprise
    EMPRISE <- st_sf(st_as_sfc(st_bbox(st_buffer(st_combine(SHP), 2500, nQuadSegs = 30))))
    EMPRISE$AURELHY <- as.character("AURELHY")
    
    # Jointure avec les données AURELHY
    AU_df <- data.frame()
    for (i in 1:length(AURELHY_list)) { # Pour chaque fichier .html de la liste
      pts <-st_sf(AURELHY_list[[i]], agr='constant')
      emp <- st_sf(st_transform(EMPRISE[,"AURELHY"],st_crs(pts)), agr='constant')
      df  <- st_intersection(pts, emp)
      
      if (nrow(df)<1) {
        cat(paste("Aucune correspondance entre le .shp et le fichier n°",i),"\n")
      } else {
        AU_df <- rbind(AU_df, as.data.frame(df))
        cat(paste("Correspondances entre le .shp et le fichier Aurelhy n°",i),"\n")
      }
    }
    
    # Sortie du tableur
    AU_df <- as.data.frame(AU_df)[,-length(AU_df)]
    cat("Le tableur AURELHY_df a été exporté \n")
    return(AU_df)
  }
}