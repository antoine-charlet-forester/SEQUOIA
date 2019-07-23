# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("lwgeom")) {install.packages("lwgeom")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("svDialogs")) {install.packages("svDialogs")}
if (!require("units")) {install.packages("units")}

IGN_VEG <- function(PARCA=T, repVEG=T) {
  options(warn=-1) # Désactivation des warnings
  if(isFALSE(PARCA)) {
    rep<-tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)")
    PARCA <- st_read(rep ,options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
    assign("PARCA", PARCA, envir=globalenv())

    NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_PARCA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())

    repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
    assign("repout2", repout2, envir=globalenv())
    repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
    assign("repout3", repout3, envir=globalenv())
  }
  if(isFALSE(repVEG)) {
    repVEG <- tk_choose.dir(default= getwd(), caption = "Choisir le répertoire de la source VEG")
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
  TEMPON6 <- BUF2CONV(PARCA, 0.5)

# Utilisation de IGN (c) BD TOPO (r) Végétation
  if (grepl('IGN BD TOPO', repVEG)){ # Boucle IGN BD TOPO

    DEP_CODE <- unique(as.data.frame(PARCA[,"DEP_CODE"])[1])

    SHPS <- st_sf(st_sfc()) # Cration objet de concaténation
    for (a in 1:length(DEP_CODE)){ # Lecture de chaque BD
      rep <- paste(repVEG, paste0('BDT_2-2_SHP_LAMB93_D0', as.character(DEP_CODE[a,1])), 'F_VEGETATION', sep="/")
      if (dir.exists(rep)){
        SHP <- st_read(paste(rep, "ZONE_VEGETATION.shp", sep="/"), quiet=T)
        st_crs(SHPS) <- st_crs(SHP)
        SHPS <- rbind(SHPS, SHP)
      } else {
        message('L IGN BD TOPO ', DEP_CODE[a,1], ' n est pas disponible')
      }
    }
    if(nrow(SHPS)>0){ # Boucle sur SHPS
      list <- c('Bois','Forêt fermée de feuillus','Forêt fermée mixte', 'Forêt fermée de conifères')
      SHPS <- SHPS %>% # Sélection des occupations boisées
        filter(SHP$NATURE %in% list)

      SHPS<- st_make_valid(st_transform(SHPS, 2154))
      SHP <- st_intersection(SHPS, TEMPON4)

      if(nrow(SHP)>0){
        SHP$SURF = as.vector(st_area(SHP)) # Calcul de surface

        VEG_polygon <- SHP %>% # Sélection des entités > 0,5 ha
          filter(SHP$SURF > 5000) %>%
          mutate(TYPE = as.character(NATURE)) %>%
          dplyr::select(TYPE)

        VEG_line <- st_difference(SEQUOIA:::mc_linearize(VEG_polygon), TEMPON6) %>%
          mutate(TYPE = as.character("VEG"),
                 NAME = as.character(NA)) %>%
          dplyr::select(TYPE, NAME)
        VEG_line <- st_difference(VEG_line, st_buffer(st_cast(TEMPON4, 'MULTILINESTRING'),1))

        VEG_point <- st_sf(st_sample(VEG_polygon, as.vector(sum(round(SHP$SURF/50000))), type = "hexagonal"))
        VEG_point <- st_join(VEG_point, VEG_polygon, join = st_intersects)
        VEG_point <- VEG_point %>%
          mutate(TYPE = as.character(TYPE),
                 NOM  = as.character(NA),
                 ROT  = as.integer(NA))
        for (b in 1:nrow(VEG_point)){
          if(grepl('Forêt fermée de feuillus', as.data.frame(VEG_point[b,"TYPE"])[1,1])){VEG_point[b,"TYPE"]="FEV"}
          if(grepl('Forêt fermée de conifères', as.data.frame(VEG_point[b,"TYPE"])[1,1])){VEG_point[b,"TYPE"]="REV"}
          if(grepl('Forêt fermée mixte', as.data.frame(VEG_point[b,"TYPE"])[1,1])){VEG_point[b,"TYPE"]="FREV"}
        }
        VEG_point <- st_difference(VEG_point, TEMPON6) %>%
          dplyr::select(TYPE, NOM, ROT)

        VEG_polygon <- st_difference(VEG_polygon, TEMPON6) %>%
          dplyr::select(TYPE)
      }
    } # Fin boucle sur SHPS
  } # Fin boucle IGN BD TOPO

# Utilisation de IGN (c) OCS (r)
  options(warn=-1) #désactivation des warnings

  if (grepl('IGN OCS', repVEG)){ # Boucle IGN BD TOPO

    DEP_CODE <- unique(as.data.frame(PARCA[,"DEP_CODE"])[1])

    SHPS <- st_sf(st_sfc()) # Cration objet de concaténation
    for (a in 1:length(DEP_CODE)){ # Lecture de chaque BD
      rep <- paste(repVEG, paste0('OCS_GE_1-1_2013_SHP_LAMB93_D0', as.character(DEP_CODE[a,1])), 'OCS_GE', '1_DONNEES_LIVRAISON', paste0('OCSGE_1-1_SHP_LAMB93_D',as.character(DEP_CODE[a,1]),'-2013'), sep="/")
      if (dir.exists(rep)){
        SHP <- st_read(paste(rep, "OCCUPATION_SOL.shp", sep="/"), quiet=T)
        st_crs(SHPS) <- st_crs(SHP)
        SHPS <- rbind(SHPS, SHP)
      } else {
        message('L IGN OCS ', DEP_CODE[a,1], ' n est pas disponible')
      }
    }
    if(nrow(SHPS)>0){
      list <- c('CS2.1.1.1','CS2.1.1.2','CS2.1.1.3')
      SHPS <- SHPS %>% # Sélection des occupations boisées
        filter(SHP$CODE_CS %in% list)

      SHPS<- st_make_valid(st_transform(SHPS, 2154))
      SHP <- st_intersection(SHPS, TEMPON4)

      if(nrow(SHP)>0){
        SHP$SURF = as.vector(st_area(SHP)) # Calcul de surface

        VEG_polygon <- SHP %>% # Sélection des entités > 0,5 ha
          filter(SHP$SURF > 5000) %>%
          mutate(TYPE = as.character(CODE_CS)) %>%
          dplyr::select(TYPE)

        VEG_line <- st_difference(SEQUOIA:::mc_linearize(VEG_polygon), TEMPON6) %>%
          mutate(TYPE = as.character("VEG"),
                 NAME = as.character(NA)) %>%
          dplyr::select(TYPE, NAME)
        VEG_line <- st_difference(VEG_line, st_buffer(st_cast(TEMPON4, 'MULTILINESTRING'),1))

        VEG_point <- st_sf(st_sample(VEG_polygon, as.vector(sum(round(SHP$SURF/50000))), type = "hexagonal"))
        VEG_point <- st_join(VEG_point, VEG_polygon, join = st_intersects)
        VEG_point <- VEG_point %>%
          mutate(TYPE = as.character(TYPE),
                 NOM  = as.character(NA),
                 ROT  = as.integer(NA))
        for (b in 1:nrow(VEG_point)){
          if(grepl('CS2.1.1.1', as.data.frame(VEG_point[b,"TYPE"])[1,1])){VEG_point[b,"TYPE"]="FEV"}
          if(grepl('CS2.1.1.2', as.data.frame(VEG_point[b,"TYPE"])[1,1])){VEG_point[b,"TYPE"]="REV"}
          if(grepl('CS2.1.1.3', as.data.frame(VEG_point[b,"TYPE"])[1,1])){VEG_point[b,"TYPE"]="FREV"}
        }
        VEG_point <- st_difference(VEG_point, TEMPON6) %>%
          dplyr::select(TYPE, NOM, ROT)

        VEG_polygon <- st_difference(VEG_polygon, TEMPON6) %>%
          dplyr::select(TYPE)
      }
    }
  } # Boucle IGN OCS

# Sorties des fichiers
  if ((exists("VEG_polygon"))&(nrow(VEG_polygon)>0)){

    if ((exists("INFRA_polygon"))&(nrow(INFRA_polygon)>0)) {
      VEG_polygon <- st_difference(VEG_polygon, st_combine(INFRA_polygon)) %>%
        dplyr::select(TYPE)
    }
    assign("VEG_polygon", VEG_polygon, envir=globalenv())
    cat("L'object sf VEG_polygon a été ajouté à l'environnement \n")
    #SEQUOIA:::WRITE(VEG_polygon, repout2, paste(NAME,"VEG_polygon.shp",sep="_"))

    if ((exists("INFRA_line"))&(nrow(INFRA_line)>0)) {
      VEG_line <- st_difference(VEG_line, st_buffer(st_combine(INFRA_line), 2)) %>%
        dplyr::select(TYPE, NAME)
    }
    if ((exists("INFRA_polygon"))&(nrow(INFRA_polygon)>0)) {
      VEG_line <- st_difference(VEG_line, st_combine(INFRA_polygon)) %>%
        dplyr::select(TYPE, NAME)
    }
    assign("VEG_line", VEG_line, envir=globalenv())
    cat("L'object sf VEG_line a été ajouté à l'environnement \n")
    #SEQUOIA:::WRITE(VEG_line, repout2, paste(NAME,"VEG_line.shp",sep="_"))

    assign("VEG_point", VEG_point, envir=globalenv())
    cat("L'object sf VEG_point a été ajouté à l'environnement \n")
    #SEQUOIA:::WRITE(VEG_point, repout2, paste(NAME,"VEG_point.shp",sep="_"))
  }
  options(warn=1) # Activation des warnings
} # Fin fonction

