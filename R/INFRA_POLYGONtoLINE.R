# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("lwgeom")) {install.packages("lwgeom")}
if (!require("stringr")) {install.packages("stringr")}

# rep  <- tk_choose.files(caption = "Choisir le fichier .shp INFRA_polygone")

INFRA_POLYGONtoLINE <- function(rep) {

# Import des données .shp
  SHP <- st_read(rep,options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  cat("Le fichier .shp a été chargé avec succès  \n")

# Recherche du nom
  NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_INFRA')[1,1]-1)

# Conversion INFRA_POLYGON to INFRA_LINE"
  INFRA_PTS  <- st_combine(st_cast(SHP,'MULTIPOINT', warn = F)) # Récupère les noeuds de polygones

  INFRA_LINE <- st_cast(SHP,'MULTILINESTRING', warn = F) # Récupère les contours de polygones

  INFRA_LINE <- st_split(INFRA_LINE, INFRA_PTS)
  INFRA_LINE <- st_collection_extract(INFRA_LINE,'LINESTRING')
  INFRA_LINE$LENGTH <- st_length(INFRA_LINE) # Ajouter un champ longueur
  INFRA_LINE <- INFRA_LINE[!(duplicated(INFRA_LINE$LENGTH) | duplicated(INFRA_LINE$LENGTH, fromLast = TRUE)), ]

# Création du .shp "INFRA_line"
  CONV_POLY <- st_convex_hull(SHP)

  t=2
  CONV_POLY <- st_simplify(CONV_POLY, preserveTopology = T, dTolerance = t)
  CONV_PTS  <- st_combine(st_cast(CONV_POLY,'MULTIPOINT', warn = F))
  CONV_LINE <- st_cast(CONV_POLY,'LINESTRING', warn = F)
  CONV_LINE <- st_split(CONV_LINE, CONV_PTS)
  CONV_LINE <- st_collection_extract(CONV_LINE,'LINESTRING')
  CONV_LINE$LENGTH <- st_length(CONV_LINE)

  d=10
  units(d) = "m"
  CONV_LINE <- CONV_LINE %>%
    filter(LENGTH <= d)

  INFRA_LINE <- INFRA_LINE %>%
    filter(!(INFRA_LINE$LENGTH %in% CONV_LINE$LENGTH))

# Mise en forme de la table attributaire du .shp "INFRA_ligne"
  INFRA_LINE <- INFRA_LINE %>%
    mutate(TYPE = TYPE,
           DOMAINE = as.character(NA),
           NOM  = as.character(NA),
           DECA  = as.character(NA),
           LENGTH  = LENGTH)%>%
    dplyr::select(TYPE, DOMAINE, NOM, DECA, LENGTH)

  st_write(INFRA_LINE, dsn=dirname(rep),layer = paste(NAME,"INFRA_line.shp",sep="_"), update=TRUE, delete_layer = TRUE, driver = "ESRI Shapefile", layer_options = "ENCODING=UTF-8", quiet =T)
  cat(" \n", paste("Le fichier",paste(NAME,"INFRA_line.shp",sep="_"),"a été exporté dans",dirname(rep))," \n")
  SEQUOIA:::MAKE_PRJ(dirname(rep),paste(NAME,"INFRA_line",sep="_"))
}
