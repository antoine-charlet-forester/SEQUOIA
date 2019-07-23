# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("lwgeom")) {install.packages("lwgeom")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("svDialogs")) {install.packages("svDialogs")}

ROADtoROUTE <- function(repROAD=T){
  if(isFALSE(repROAD)) {
    repROAD<-tk_choose.files(caption = "Choisir le fichier.shp ROAD_ line ou polygon")
  }
  options(warn=-1) #désactivation des warnings

# Import de la source de donnée
  ROAD <- st_read(repROAD, options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  assign("ROAD", ROAD, envir=globalenv())

  rep <- repROAD
  NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_ROAD')[1,1]-1)
  assign("NAME", NAME, envir=globalenv())

  repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
  assign("repout2", repout2, envir=globalenv())
  repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
  assign("repout3", repout3, envir=globalenv())

# Chargement 1. Cadastre
  if (grepl('polygon', repROAD)){ # Boucle 1. Cadastre
    # Lecture du fichier
    ROAD_polygon <- ROAD

    # Conversion de ROAD_polygon vers ROUTE_line
    ROUTE_line <- st_cast(ROAD_polygon, 'MULTILINESTRING') # Linéarisation du polygon
    ROUTE_line <- st_difference(ROUTE_line, st_buffer(st_union(ROAD_polygon), -0.0001)) # Suppression des recouvrements

    # Pré-découpage des bordures
    CONV_polygon <- st_simplify(st_convex_hull(ROAD_polygon), preserveTopology = T, dTolerance = 2)
    CONV_point  <- st_combine(st_cast(CONV_polygon,'MULTIPOINT', warn = F))
    ROUTE_line <- st_split(st_cast(ROUTE_line,'MULTILINESTRING'), CONV_point)
    ROUTE_line <- st_collection_extract(ROUTE_line,'LINESTRING')

    # Nettoyage de la table
    ROUTE_line$LENGTH <- st_length(ROUTE_line) # Ajouter un champ longueur

    d=0.1
    units(d) = "m"
    ROUTE_line<- ROUTE_line%>%
      filter(!(ROUTE_line$LENGTH < d))

    SEQUOIA:::WRITE(ROUTE_line, repout2, paste(NAME,"ROUTE_line.shp",sep="_"))
    SEQUOIA:::WRITE(ROAD_polygon, repout2, paste(NAME,"ROUTE_polygon.shp",sep="_"))
  } # Fin Boucle 1. Cadastre

# Chargement 2. OSM
  if (grepl('line', repROAD)){ # Boucle 2. OSM
    # Lecture du fichier
    ROAD_line <- ROAD

    # Fonction line2polygon
    line2polygon <- function(type, B){
      line <- st_zm(ROAD_line %>% filter(TYPE %in% type), drop = TRUE, what = "ZM")
      polygon <- st_buffer(line, B, 5, endCapStyle='ROUND')
      return(polygon)
    }

    # Création de RD_polygon, RC_polygon, RF_polygon
    B1 <- as.numeric(dlgInput("Tempon RD:",default = "")$res)
    RD_buffer <- line2polygon(type="RD", B1)[, ]

    RN_buffer <- line2polygon(type="RN", B1*2.5)[, ]
    RN_polygon <- RN_buffer

    B2 <- as.numeric(dlgInput("Tempon RC:",default = "")$res)
    RC_buffer <- line2polygon(type="RC", B2)[, ]
    RF_buffer <- line2polygon(type="RF", B2)[, ]

    ROUTE_buffer <- rbind(RN_buffer, RD_buffer, RC_buffer, RF_buffer)

    # Linéarisation des buffers
    ROUTE_line <- st_cast(ROUTE_buffer, 'MULTILINESTRING') # Linéarisation du polygon
    ROUTE_line <- rbind(ROUTE_line, ROAD_line %>% filter(TYPE %in% "PN"))
    ROUTE_line <- st_difference(ROUTE_line, st_buffer(st_buffer(st_union(ROUTE_buffer), 0.0001),-0.0002)) # Suppression des recouvrements

    # Création du buffer de découpe
    BUFFER_line <- ROAD_line %>% filter(TYPE %in% c("RN", "RD", "RC", "RF", "CR", "CC"))
    BUFFER1_polygon <- st_buffer(st_union(BUFFER_line), B1+0.1, nQuadSegs = 0, endCapStyle = 'FLAT',
                                 joinStyle = "ROUND")
    BUFFER_point <- st_cast(st_node(BUFFER_line[3]), "POINT")
    BUFFER_point$duplicates <- duplicated(BUFFER_point)
    BUFFER2_polygon <- st_buffer(BUFFER_point %>% filter(duplicates %in% TRUE), B1+0.1, nQuadSegs = 0)

    BUFFER_polygon <- st_union(st_union(BUFFER1_polygon , BUFFER2_polygon ))

    # Création de ROUTE_polygon
    ROUTE_polygon <- st_intersection(ROUTE_buffer, BUFFER_polygon)

    # Création de ROUTE_line
    ROUTE_line <- st_intersection(ROUTE_line, BUFFER_polygon)
    ROUTE_buffer_line <- st_cast(BUFFER_polygon , 'MULTILINESTRING')
    ROUTE_line <- st_difference(ROUTE_line, ROUTE_buffer_line)

    # Sorties des fichiers
    # LIST_SHP <- list.files(repout2, "*.shp") A voir pour traitement de fusion
    SEQUOIA:::WRITE(ROUTE_polygon, repout2, paste(NAME,"ROUTE_polygon.shp",sep="_"))
    SEQUOIA:::WRITE(ROUTE_line, repout2, paste(NAME,"ROUTE_line.shp",sep="_"))
  } # Fin Boucle 2. OSM

# Suppression débordement INFRA_line
  repINFRA_line <- str_replace(repROAD,"ROAD","INFRA")
  repINFRA_line <- str_replace(repINFRA_line,"polygon","line")
  INFRA_line <- st_read(repINFRA_line, options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  INFRA_line <- st_difference(INFRA_line, st_make_valid(st_combine(ROAD_polygon)))
  SEQUOIA:::WRITE(INFRA_line, repout2, paste(NAME,"INFRA_line.shp",sep="_")) # Export du shapefile

  options(warn=1) # Activation des warnings
} # Fin fonction
