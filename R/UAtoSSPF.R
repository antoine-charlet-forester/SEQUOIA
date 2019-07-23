# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("svDialogs")) {install.packages("svDialogs")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("lwgeom")) {install.packages("lwgeom")}

UAtoSSPF <- function(rep=T) {
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)")

    NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_UA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())

    repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
    assign("repout2", repout2, envir=globalenv())
    repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
    assign("repout3", repout3, envir=globalenv())
  }

# Import des données .shp
  SHP <- st_read(rep,options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  cat("Le fichier .shp a été chargé avec succès  \n")

# Répertoire de sorties
  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")

# Recherche du nom
  NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_UA')[1,1]-1)

# Création du .shp "PF_polygon"
  PF_POLY <- aggregate(x = SHP[, "SURF_COR"], by = list(SHP$N_PARFOR),
                         FUN = sum, na.rm = TRUE)
  names(PF_POLY) <- c("N_PARFOR","SURF_COR","geometry")


  SEQUOIA:::WRITE(PF_POLY, repout2, paste(NAME,"PF_polygon.shp",sep="_"))

# Création du .shp "PF_line"
  PF_PTS <- st_combine(st_cast(PF_POLY[1],'MULTIPOINT', warn = F)) # Récupère les noeuds de polygones

  PF_LINE <- st_cast(PF_POLY[1],'MULTILINESTRING') # Récupère les contours de polygones
  PF_LINE <- st_split(PF_LINE, PF_PTS) # Segmentation : Decoupe les contours de polygones par les noeuds
  PF_LINE <- st_collection_extract(PF_LINE,"LINESTRING") # Sélectionne les objets de classe LINESTRING
  PF_LINE$LENGTH <- st_length(PF_LINE) # Ajouter un champ longueur
  PF_LINE <- PF_LINE[!duplicated(PF_LINE$LENGTH),] # Supprime les doublons sur le champs longueur
  st_crs(PF_LINE) <- "+init=epsg:2154"

  PROP_LINE <- st_read(paste(dirname(rep),paste(NAME,"PROP_line.shp",sep="_"),sep="/"),options = "ENCODING=windows-1252", quiet=T)
  st_crs(PROP_LINE) <- "+init=epsg:2154"
  PROP_POLY <- st_buffer(st_union(PROP_LINE),0.0005)
  PROP_PLOY_LINE <- st_sf(st_cast(PROP_POLY,'MULTILINESTRING'))

  st_agr(PF_LINE)="constant"
  PF_LINE <- st_difference(PF_LINE, PROP_POLY[1])

  LIST_SHP <- list.files(dirname(rep), "*.shp")
  setwd(dirname(rep))

  if (paste(NAME,"PF_line.shp",sep="_") %in% LIST_SHP) {
    RES <- winDialog(type = "yesno","Voulez-vous remplacer PF_line ?")
    if (RES=="YES") {
      cat("Remplacement de PF_line","\n")
      SEQUOIA:::WRITE(PF_LINE, repout2, paste(NAME,"PF_line.shp",sep="_"))
    } else {
      cat("Pas de remplacement de PF_line","\n")
    }
  } else {
    SEQUOIA:::WRITE(PF_LINE, repout2, paste(NAME,"PF_line.shp",sep="_"))
  }

# Création du .shp "SSPF_polygon"
  SSPF_POLY <- aggregate(x = SHP[, "SURF_COR"], by = list(SHP$PARFOR),
                         FUN = sum, na.rm = TRUE)
  names(SSPF_POLY) <- c("PARFOR","SURF_COR","geometry")
  SSPF_POLY <- merge(x=SSPF_POLY, y=unique(as.data.frame(SHP[, c("PARFOR","PLT_TYPE","PLT_COMS")])[,1:3]), by="PARFOR")

  SEQUOIA:::WRITE(SSPF_POLY, repout2, paste(NAME,"SSPF_polygon.shp",sep="_"))

# Actualisation de SSPF_line
  if (paste(NAME,"SSPF_line.shp",sep="_") %in% LIST_SHP) {
    RES <- winDialog(type = "yesno","Voulez-vous actualisez SSPF_line ?")
    if (RES=="YES") {
      cat("Actualisation de SSPF_line","\n")

      SSPF_LINE <- st_read(paste(dirname(rep),paste(NAME,"SSPF_line.shp",sep="_"),sep="/"),options = "ENCODING=UTF-8", quiet=T)

      SSPF_TEMPO <- merge(x = SSPF_LINE, y = as.data.frame(SSPF_POLY)[,1:4], by = "PARFOR")
      SSPF_TEMPO <- SSPF_TEMPO %>%
        mutate(PLT_TYPE=PLT_TYPE.y,
                PLT_COMS=PLT_COMS.y,
                SURF_COR=SURF_COR.y) %>%
        dplyr::select(PARFOR, PLT_TYPE, PLT_COMS, SURF_COR, LblField:LblAShow)

      SEQUOIA:::WRITE(SSPF_TEMPO, repout2, paste(NAME,"SSPF_line.shp",sep="_"))
    } else {
      cat("Pas d'actualisation de SSPF_line","\n")
    }
  }
}
