# Lancement des library
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("osmdata")) {install.packages("osmdata")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("tcltk")) {install.packages("tcltk")}

OSM <- function(PARCA=T){
  options(warn=-1) # Désactivation des warnings
  if(isFALSE(PARCA)) {
    rep<-tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)")
    PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
    NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_PARCA')[1,1]-1)

    repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
    repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  }

# Préparation des tempoms
  BUF2CONV <- function(x, T) { #Fonction de conversion des tempons en enveloppe
    SHP <- st_sf(st_cast(st_union(st_buffer(x, T, 30)), 'POLYGON'))
    CONVEX_ALL <- st_sf(st_sfc(crs=st_crs(SHP)))
    for (a in 1:nrow(SHP)){
      CONVEX <- st_convex_hull(SHP[a,])
      CONVEX_ALL <- st_make_valid(rbind(CONVEX_ALL, CONVEX))
    }
    return(CONVEX_ALL)
  }

  T = 500
  TEMPON1 <- BUF2CONV(PARCA, T)
  TEMPON2 <- BUF2CONV(PARCA, T-1)
  TEMPON3 <- BUF2CONV(PARCA, T-2)
  TEMPON4 <- BUF2CONV(PARCA, T*2)
  TEMPON5 <- BUF2CONV(PARCA, T*4)

# Adresse du serveur
  #new_url <- "https://overpass.kumi.systems/api/interpreter"
  #new_url <- "http://overpass.openstreetmap.ie/api/interpreter"
  #new_url <- "https://lz4.overpass-api.de/api/interpreter"
  new_url <- "http://www.overpass-api.de/api/interpreter"
  set_overpass_url(new_url)
  options(warn=-1)

# Création du .shp "COMS_polygon"
  DATA <- opq(bbox=st_bbox(st_transform(TEMPON5, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'admin_level', value = '8') %>%
    osmdata_xml(filename = 'OSM_COMS')

  COMS_OSM_POLY <- st_transform(st_read('OSM_COMS', layer = 'multipolygons', quiet=T, stringsAsFactors = FALSE), 2154)
  SEQUOIA:::WRITE(COMS_OSM_POLY, repout2, paste(NAME,"COMS_polygon.shp",sep="_"))

# Création du .shp "COMS_line"
  COMS_OSM_LINE <- st_transform(st_read('OSM_COMS', layer = 'lines', quiet=T, stringsAsFactors = FALSE), 2154)
  SEQUOIA:::WRITE(COMS_OSM_LINE, repout2, paste(NAME,"COMS_line.shp",sep="_"))

  COMS_OSM_LINE <- st_intersection(COMS_OSM_LINE, TEMPON1)
  SEQUOIA:::WRITE(COMS_OSM_LINE, repout2, paste(NAME,"COM_line.shp",sep="_"))

# Création du .shp "COMS_point"
  COMS_OSM_PTS  <- st_transform(st_read('OSM_COMS', layer = 'points', quiet=T, stringsAsFactors = FALSE), 2154)
  SEQUOIA:::WRITE(COMS_OSM_PTS, repout2, paste(NAME,"COMS_point.shp",sep="_"))

  COMS_OSM_PTS  <- st_centroid(st_intersection(COMS_OSM_POLY[,"name"], TEMPON1))
  SEQUOIA:::WRITE(COMS_OSM_PTS, repout2, paste(NAME,"COM_point.shp",sep="_"))

# Création de BT_polygon (BATI)
  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'building') %>%
    osmdata_xml(filename = 'OSM_BATI')

  BT_polygon <- st_read('OSM_BATI', layer = 'multipolygons', quiet=T, stringsAsFactors = FALSE)
  if(nrow(BT_polygon)>0){
    BT_polygon <- st_transform(BT_polygon, 2154)
    BT_polygon$TYPE <- "BT"
    BT_polygon <- BT_polygon[,"TYPE"]
  }

# Création de SP_polygon (SPORT)
  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'leisure') %>%
    osmdata_xml(filename = 'OSM_SPORT')

  SP_polygon <- st_read('OSM_SPORT', layer = 'multipolygons', quiet=T, stringsAsFactors = FALSE)
  if(nrow(SP_polygon)>0){
    SP_polygon <- st_transform(SP_polygon, 2154)
    SP_polygon$TYPE <- "SP"
    SP_polygon <- SP_polygon[,"TYPE"]
  }

# Création de INFRA_polygon
  if(nrow(BT_polygon)>0){
    INFRA_polygon <- BT_polygon
    if(nrow(SP_polygon)>0){
      INFRA_polygon <- rbind(INFRA_polygon, SP_polygon)
    }
  }
  # SEQUOIA:::WRITE(INFRA_polygon, repout2, paste(NAME,"INFRA_polygon.shp",sep="_"))
  assign("INFRA_polygon", INFRA_polygon, envir=globalenv())
  cat("L'object sf INFRA_polygon a été ajouté à l'environnement \n")

# Création de VF_line (VOIES FERREES)
  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'railway') %>%
    osmdata_xml(filename = 'OSM_RAIL')

  VF_line <- st_read('OSM_RAIL', layer = 'lines', quiet=T, stringsAsFactors = FALSE)
  if(nrow(VF_line)>0){
    VF_line <- st_transform(VF_line, 2154)
    VF_line$TYPE <- "VF"
    VF_line$NAME <- as.character("")
    VF_line <- VF_line[,c("TYPE", "NAME")]
  }

# Création de LE_line (LIGNE ELECTRIQUE)
  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'power') %>%
    osmdata_xml(filename = 'OSM_POWER')

  LE_line <- st_read('OSM_POWER', layer = 'lines', quiet=T, stringsAsFactors = FALSE)
  if(nrow(LE_line)>0){
    LE_line <- st_transform(LE_line, 2154)
    LE_line$TYPE <- "LE"
    LE_line$NAME <- as.character("")
    LE_line <- LE_line[,c("TYPE", "NAME")]
  }

# Création de INFRA_line
  INFRA_line <- st_sf(st_sfc(),crs=2154)
  if(nrow(VF_line)>0){
    INFRA_line <- VF_line}
  if(nrow(INFRA_line)>0){
    if(nrow(LE_line)>0){rbind(INFRA_line, LE_line)}
    } else {INFRA_line <- LE_line}

  assign("INFRA_line", INFRA_line, envir=globalenv())
  cat("L'object sf INFRA_line a été ajouté à l'environnement \n")

# Création du .shp "ROUTE_line"
  DATA <- opq(bbox=st_bbox(st_transform(TEMPON4, "+init=epsg:4326"))) %>%
    add_osm_feature(key = 'highway') %>%
    osmdata_xml(filename = 'OSM_INFRA')

  ROUTE_line <- st_read('OSM_INFRA', layer = 'lines', quiet=T, stringsAsFactors = FALSE)
  if(nrow(ROUTE_line)>0){
    ROUTE_line <- st_transform(ROUTE_line, 2154, layer_options = "ENCODING=UTF-8")
    ROUTE_line$TYPE <- ROUTE_line$highway
    ROUTE_line$NAME <- as.character("")

    for(a in 1:nrow(ROUTE_line)){
      TAG <- as.character(ROUTE_line[a,"other_tags"])[1]
      N <- str_locate(TAG,'"ref\"=>\"')[,2]
      if (!is.na(N)){
        N2 <- str_locate_all(TAG,'\"')[[1]][,1]
        b=1
        while (N2[b]<N){
          b=b+1
        }
        REF <- str_sub(TAG, N+1, N2[b+1]-1)
        ROUTE_line[a, c("NAME")] <- REF
      } else {ROUTE_line[a, c("NAME")] <- ROUTE_line[a, c("name")]}
    }

    ROUTE_line <- ROUTE_line[,c("TYPE", "NAME")]
  }

  SEQUOIA:::WRITE(ROUTE_line, repout2, paste(NAME,"ROAD_line.shp",sep="_"))
  options(warn=1) # Activation des warnings
}
