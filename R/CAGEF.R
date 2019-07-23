# Lancement des library
if (!require("sf")) {install.packages("sf")}
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("stringr")) {install.packages("stringr")}

CAGEF <- function(PARCA=T, CODECA){
  if(isFALSE(PARCA)) {
    rep<-tk_choose.files(caption = "Choisir le fichier .shp du parcellaire cadastral (PARCA)")
    PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
    assign("PARCA", PARCA, envir=globalenv())

    NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_PARCA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())

    repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
    assign("repout2", repout2, envir=globalenv())
    repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
    assign("repout3", repout3, envir=globalenv())
  }

# Téléchargement des données OSM
  message("Téléchargement des données OSM")
  SEQUOIA:::OSM(PARCA)

# Téléchargement des données BDTOPO_HYDRO
  message("Téléchargement des données IGN (C) BDTOPO (r) Hydrologie")
  if(!exists("repBDTOPO")) {repBDTOPO <- tk_choose.dir(default= getwd(), caption = "Choisir le répertoire du dossier'IGN BD TOPO HYDRO'")}
  SEQUOIA:::BDTOPO_HYDRO(PARCA, repBDTOPO)

# Pré-création de INFRA_polygon & INFRA_line
  if(nrow(HYDRO_polygon)>0){ INFRA_polygon <- rbind(INFRA_polygon, HYDRO_polygon)}

  if(nrow(HYDRO_line)>0){
    if(nrow(INFRA_line)>0){INFRA_line <- rbind(INFRA_line, HYDRO_line)
    }else{INFRA_line <- HYDRO_line}}

# Import de la végétation
  RES <- winDialog(type = "yesno","Voulez-vous importer la végétation depuis l'IGN (c) ?")
  if (RES=="YES") {
    message("Import de la végétation depuis IGN (c) BDTOPO (r) ou OCS_GE (r)")
    SEQUOIA:::IGN_VEG(PARCA, repVEG=F)
  } else {
    cat("Pas d'import de végétation","\n")
  }

# Modification et sortie de INFRA_polygon & INFRA_line
  if (RES=="YES") {
    if(nrow(VEG_polygon)>0){INFRA_polygon <- rbind(INFRA_polygon, VEG_polygon)}
    if(nrow(VEG_line)>0){INFRA_line <- rbind(INFRA_line, VEG_line)}
  }
  SEQUOIA:::WRITE(INFRA_polygon, repout2, paste(NAME,"INFRA_polygon.shp",sep="_"))
  SEQUOIA:::WRITE(INFRA_line, repout2, paste(NAME,"INFRA_line.shp",sep="_"))

# Création de INFRA_point
  INFRA_point <- st_sf(st_sfc(st_point())) %>%
    mutate(TYPE = as.character(NA),
           NOM  = as.character(NA),
           ROT  = as.integer(NA)) %>%
    dplyr::select(TYPE, NOM, ROT)
  st_crs(INFRA_point) <- st_crs(2154)
  if(nrow(HYDRO_point)>0){ INFRA_point <- rbind(INFRA_point, HYDRO_point)}
  if (RES=="YES") {
    if(nrow(VEG_point)>0){INFRA_point <- rbind(INFRA_point, VEG_point)}
  }
  SEQUOIA:::WRITE(INFRA_point, repout2, paste(NAME, "INFRA_point.shp", sep="_"))

# Création des données cadastrales
  if (CODECA==1){
    message("Téléchargement des données ETALAB")
    SEQUOIA:::ETALAB(PARCA)
  }
  if (CODECA==2){
    message("Chargement et export des données BD_PARCELLAIRE")
    if(!exists("repBDPARCA")) {repBDPARCA <- tk_choose.dir(default= getwd(), caption = "Choisir le répertoire du dossier'IGN BD PARCA'")}
    SEQUOIA:::BD_PARCA(PARCA, repBDPARCA)
  }
  if (CODECA==3){
    message("Chargement et export des données EDIGEO")
    SEQUOIA:::EDIGEO(PARCA)
  }
# Fin de programme
  message("Fin de téléchargement")
}
