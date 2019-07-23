### Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("svDialogs")) {install.packages("svDialogs")}
if (!require("stringr")) {install.packages("stringr")}

# repshp   <- tk_choose.files(caption = "Choisir le fichier .shp")
# repRdata <- tk_choose.files(caption = "Choisir le fichier .Rdata")

INPNonPARCA <- function(repshp, repRdata){

# Import des données .shp
  SHP <- st_read(repshp, options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  cat("Le fichier .shp a été chargé avec succès \n")

# Recherche du nom
  NAME <- str_sub(repshp, str_locate_all(repshp,'/')[[1]][nrow(str_locate_all(repshp,'/')[[1]]),1]+1,str_locate(repshp,'_PARCA')[1,1]-1)
  if(is.na(NAME)) {NAME <- str_sub(repshp, str_locate_all(repshp,'/')[[1]][nrow(str_locate_all(repshp,'/')[[1]]),1]+1,str_locate(repshp,'_UA')[1,1]-1)}

# Import des données .Rdata
  load(repRdata)
  cat("L'archive .Rdata a été chargé avec succès \n")

# Intersection
  for (a in 1:length(list_INPN)) {
    SHP <- st_transform(SHP,st_crs(list_INPN[[a]]))
    SHP["INPN"] <- "INPN"

    INPN_POLY   <- st_join(list_INPN[[a]],SHP["INPN"])
    INPN_POLY   <- INPN_POLY %>% filter(!is.na(INPN))
    INPN_POLY   <- unique(INPN_POLY)

    NOM <- list_INPN_NOM[[a]]

    if (nrow(INPN_POLY)>=1) {
      SORTIE  <- str_replace(paste(NAME, NOM, sep="_"),".shp","") # Création du nom de sortie
      st_write(INPN_POLY, dsn= dirname(repshp), layer= SORTIE, update= TRUE, delete_layer= TRUE, driver= "ESRI Shapefile", quiet= T)
      cat(paste("Le fichier", paste0(SORTIE,".shp"), "a été exporté dans",dirname(repshp)), "\n")
      SEQUOIA:::MAKE_PRJ(dirname(repshp), SORTIE)
    } else {
      cat(paste("Aucune correspondance entre le .shp et ", NOM), "\n")
    }
  }
}


