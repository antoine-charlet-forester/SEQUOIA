# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("svDialogs")) {install.packages("svDialogs")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("lwgeom")) {install.packages("lwgeom")}
if (!require("R.utils")) {install.packages("R.utils")}
if (!require("osmdata")) {install.packages("osmdata")}
if (!require("rvest")) {install.packages("rvest")}
if (!require("xml2")) {install.packages("xml2")}

PARCAtoUA <- function(rep=T) {
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp contenant le parcellaire cadastral")

    NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_PARCA')[1,1]-1)

    repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
    repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
    }

# Import des données .shp
    PARCA <- st_read(rep, options = "ENCODING=UTF-8", agr = "constant", crs=2154, quiet=T)  # Lecture du shapefile
    cat(" \n", "Le fichier .shp a été chargé avec succès  \n")

# Recherche du nom
    NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_PARCA')[1,1]-1)

# Création du .shp "UA_polygon"
    UA_polygon <- PARCA %>%
      mutate(PARCA      = paste0(SECTION, ' ', N_PARCA),
             PARFOR     = as.character(NA),
             N_PARFOR   = as.integer(NA),
             N_SSPARFOR = as.integer(NA),
             GEOL_TYPE  = as.character(NA),
             PLT_TYPE   = as.character(NA),
             PLT_COMS   = as.character(NA),
             AME_TYPE   = as.character(NA),
             SURF_SIG   = as.double(NA),
             SURF_COR   = as.double(NA)) %>%
      dplyr::select(REG_CODE:ADRESSE,IDU,PREFIXE:N_PARCA, PARCA, LIEUDIT,PARFOR,N_PARFOR,N_SSPARFOR,GEOL_TYPE, OCCUP_SOL,PLT_TYPE,PLT_COMS,AME_TYPE,REV_CA,SURF_CA,SURF_SIG,SURF_COR,geometry)
    UA_polygon <- unique(UA_polygon) # Simplification du shapefile

    SEQUOIA:::WRITE(UA_polygon, repout2, paste(NAME,"UA_polygon.shp",sep="_"))

# Création du .shp "PROP_polygon"
    PROP_polygon <- aggregate(x = PARCA[, "SURF_CA"], by = list(PARCA$PROP),
                           FUN = sum, na.rm = TRUE)
    names(PROP_polygon) <- c("PROP","SURF_CA","geometry")

    SEQUOIA:::WRITE(PROP_polygon, repout2, paste(NAME,"PROP_polygon.shp",sep="_"))

# Création du .shp "PROP_line"
    PROP_point <- st_combine(st_cast(PROP_polygon,'MULTIPOINT')) # Récupère les noeuds de polygones

    PROP_line <- st_cast(PROP_polygon[,-2],'MULTILINESTRING') # Récupère les contours de polygones
    PROP_line <- st_split(PROP_line, PROP_point) # Segmentation : Decoupe les contours de polygones par les noeuds
    PROP_line <- st_collection_extract(PROP_line,"LINESTRING") # Sélectionne les objets de classe LINESTRING
    PROP_line$LENGTH <- st_length(PROP_line) # Ajouter un champ longueur
    PROP_line <- PROP_line[!duplicated(PROP_line$LENGTH),] # Supprime les doublons sur le champs longueur
    st_crs(PROP_line) <-2154

    SEQUOIA:::WRITE(PROP_line, repout2, paste(NAME,"PROP_line.shp",sep="_"))

# Création du .shp "PROP_point"
    PROP_point <- st_centroid(PROP_polygon, of_largest_polygon=F) # Création du centroid

    SEQUOIA:::WRITE(PROP_point, repout2, paste(NAME,"PROP_point.shp",sep="_"))
}
