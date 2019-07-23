# Lancement des library
if (!require("sf")) {install.packages("sf")}
if (!require("plyr")) {install.packages("plyr")}
if (!require("dplyr")) {install.packages("dplyr")}


# De multipoint à points
mc_un_multipoint = function(x) {
  g = st_geometry(x)
  i = rep(seq_len(nrow(x)), sapply(g, nrow))
  x = x[i,]
  st_geometry(x) = st_sfc(do.call(c,
                                  lapply(g, function(geom) lapply(1:nrow(geom), function(i) st_point(geom[i,])))))
  x$original_geom_id = i
  x
}

# De polygones à lignes épurées
mc_linearize = function(polygon) {
  options(warn=-1) # Désactivation des warnings

  point <- st_cast(polygon,'MULTIPOINT')               # Récupère les noeuds de polygones
  point <- mc_un_multipoint(point) %>%                 # Fonction précédente
    mutate(VALUE=as.character(geometry))
  df    <- plyr::ddply(point,.(VALUE),nrow)            # Détection du nombre de duplication par noeuds
  point <- merge(point, df, by = "VALUE") %>%          # Sélection des noeuds dupliqués
    filter(V1 > 1)

  line  <- st_cast(polygon,'MULTILINESTRING')           # Récupère les contours de polygones
  line  <- st_split(line, st_combine(point))            # Segmentation des contours de polygones par les noeuds
  line  <- st_collection_extract(line,"LINESTRING")     # Extraction des objets de classe LINESTRING
  line <- distinct(line, geometry, .keep_all= TRUE)     # Filtre supprimant les lignes dupliquées

  return(line)
  options(warn=1) # Activation des warnings
}
