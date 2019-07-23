# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("elevatr")) {install.packages("elevatr")}
if (!require("raster")) {install.packages("raster")}
if (!require("svDialogs")) {install.packages("svDialogs")}
if (!require("smoothr")) {install.packages("smoothr")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("gdalUtils")) {install.packages("gdalUtils")}
if (!require("rgdal")) {install.packages("rgdal")}

MNTonSHP <- function(REP_SHP=T, NAME=NULL, TEMP=NULL){ # Function

# Sélection du fichier .shp
  if(isFALSE(REP_SHP)){
    REP_SHP  <- tk_choose.files(caption = "Choisir le fichier .shp contenant le parcellaire cadastral")
  }
  if(!length(REP_SHP)){
    warning("Aucun fichier sélectionné >> Traitement annulé")
  } else { # Boucle REP_SHP

    # Lecture du fichier .shp
    SHP <- st_read(REP_SHP, options = "ENCODING=UTF-8", agr="constant", quiet=T)  # Lecture du shapefile
    cat("Le fichier .shp a été chargé avec succès \n")

    # Création d'une emprise
    convex <- function(shp, buffer){
      comb <- st_buffer(st_combine(shp), buffer, nQuadSegs = 30)
      comb <- st_sf(st_cast(comb, 'POLYGON'))
      conv_all <- st_sf(st_sfc())

      for (a in 1:nrow(comb)){
        conv <- st_convex_hull(comb[a,])
        st_crs(conv_all) <- st_crs(conv)
        conv_all <- rbind(conv_all, conv)
      }
      return(conv_all)
    }
    if(!is.null(TEMP)){buffer = TEMP}else{buffer = 200}
    EMPRISE <- convex(SHP, buffer)

    # Choix de la source de données
    form <- c("IGN (C) BD ALTI (R)",
              "Amazon Web Services")
    RES <- dlgList(form, multiple = T, title = "Choix du MNT à utiliser", gui = .GUI)$res
    if(!length(RES)){
      warning("Aucun fichier sélectionné >> Traitement annulé")
    }

    # Récupération du MNT > IGN (C) BD ALTI (R)

    if(RES=="IGN (C) BD ALTI (R)") { # Boucle IGN (C) BD ALTI (R)

      REP_MNT  <- dlg_dir(default = getwd(), title="Choisir le dossier contenant les couches RASTER")$res

      if (!length(REP_MNT)) {
        warning("Aucun fichier sélectionné >> Traitement annulé")
      } else { # Boucle REP_MNT

        LISTE_ASC <- list.files(REP_MNT, "*.asc$") # Détection des fichiers .asc dans le répertoire sélectionné
        IDU <- str_sub(LISTE_ASC[1], 1, 12) # Détection du MNT IGN (C) BD ALTI (R)
        cat("Le MNT utilisé est",IDU,"\n")

        REP_ASC <- list()
        # Boucle en cas de chevauchement sur plusieurs dalles
        a=1
        for (x in 1:2){ # x min et x max
          for(y in 3:4) { # y min et y max
            options(scipen=999) # Evite la notation scientifique
            if (str_sub(IDU,10,11)=="75") { # Récupération de la dalle pour la IGN (C) BD ALTI 75 m (R)
              if(extent(SHP)[x]<round(extent(SHP)[x]/75000)*75000) {
                XMIN <- str_pad(str_sub(as.character(round(extent(SHP)[x]/75000)*75000-75000),1,3), 4, "left", pad = "0")
              } else {
                XMIN <- str_pad(str_sub(as.character(round(extent(SHP)[x]/75000)*75000),1,3), 4, "left", pad = "0")
              }

              if(extent(SHP)[y]>round(extent(SHP)[y]/75000)*75000) {
                YMIN <- str_sub(as.character(round(extent(SHP)[y]/75000)*75000+75000),1,4)
              } else {
                YMIN <- str_sub(as.character(round(extent(SHP)[y]/75000)*75000),1,4)
              }
            } else {
              if (str_sub(IDU,10,11)=="25") { # Récupération de la dalle pour la IGN (C) BD ALTI 25 m (R)
                if(extent(SHP)[x]<round(extent(SHP)[x]/25000)*25000) {
                  XMIN <- str_pad(str_sub(as.character(round(extent(SHP)[x]/25000)*25000-25000),1,3), 4, "left", pad = "0")
                } else {
                  XMIN <- str_pad(str_sub(as.character(round(extent(SHP)[x]/25000)*25000),1,3), 4, "left", pad = "0")
                }

                if(extent(SHP)[y]>round(extent(SHP)[y]/25000)*25000) {
                  YMIN <- str_sub(as.character(round(extent(SHP)[y]/25000)*25000+25000),1,4)
                } else {
                  YMIN <- str_sub(as.character(round(extent(SHP)[y]/25000)*25000),1,4)
                }
              }
            }
            REP_ASC[[a]] <- paste(REP_MNT, paste0(paste(IDU,"FXX", XMIN, YMIN, "MNT_LAMB93_IGN69", sep="_"), ".asc"), sep="/") # Récupération du répertoire de la dalle IGN
            a = a+1
          } # Fin y
        } # Fin x

        REP_ASC <- unique(REP_ASC) # Suppression du doublons en cas de dalle unique

        if (length(REP_ASC)>1){ # Merge des dalles
          MNT <- raster::raster(REP_ASC[[1]][1], crs="+init=epsg:2154")
          for (a in 2:length(REP_ASC)){
            ASC <- raster::raster(REP_ASC[[a]][1], crs="+init=epsg:2154")
            MNT <- raster::merge(MNT, ASC)
          }
        } else {
          MNT <- raster::raster(REP_ASC[[1]][1], crs="+init=epsg:2154")
        }

        MNT <- raster::crop(MNT, as(st_transform(EMPRISE, 2154), "Spatial"))
        writeRaster(MNT, paste0(dirname(REP_SHP),"/","MNT.tif"), overwrite=TRUE) # Créer ton raster MNT au format tiff, spécifie le chemin de sauvegarde, lisible ensuite sans problème sur QGis

        cat("Le MNT a été récupéré avec succès \n")

      } # Fin boucle REP_MNT

    } # Fin boucle IGN (C) BD ALTI (R)

    # Récupération du MNT > Amazon Web Services

    if(RES=="Amazon Web Services") { # Boucle Amazon Web Services

      SiteMnt <- function(shp, zoom=14, epsg=NULL) {             # Fonction SiteMnt, shp=polygone de périmètre de la forêt, epsg qui par défaut est nul mais qui peut être précisé
        if (is.numeric(zoom) & zoom < 15) {            # Test pour être sur de pouvoir utiliser les variables d'entrée
          zoom = max(zoom, 9)                                               # Règle la précision
          x <- get_elev_raster(as(shp, "Spatial"), z = zoom, src = "aws")   # Transformation de l'objet sf en objet sp puis récupération du MNS
          if (!is.null(epsg)) {
            x <- projectRaster(x, crs=CRS(paste0('+init=EPSG:',epsg)))      # Si système de projection demandé, l'attribuer au MNT
          }
          return(x)
        } else {
          warning("buffer et zoom doivent etre des entiers.")
        }
      }

      MNT <- SiteMnt(EMPRISE, epsg=2154) # Création du MNT par appel de la fonction de récupération du MNT sur emprise + reprojection suivant l'EPSG souhaité
      MNT <- crop(MNT, as(st_transform(EMPRISE, 2154), "Spatial"))

      writeRaster(MNT, paste0(dirname(REP_SHP),"/","MNT.tif"), overwrite=TRUE) # Créer ton raster MNT au format tiff, spécifie le chemin de sauvegarde, lisible ensuite sans problème sur QGis

      cat("Le MNT a été généré avec succès \n")

    } # Fin boucle Amazon Web Services

    if(!length(MNT)){
      warning("Une erreur est survenue dans la création du MNT")
    } else { # Boucle MNT
      # Création du .shp TOPO_line non lissé
      REP_TOPO_line <- paste(dirname(REP_SHP), "TOPO_line.shp", sep="/") # Répertoire de sortie du fichier
      DEM <- paste0(dirname(REP_SHP),"/","MNT.tif")

      line <- gdalUtils::gdal_contour(src_filename = DEM,
                   dst_filename = REP_TOPO_line,
                   b = 1,
                   a = "ELEVATION",
                   i = 10.0,
                   f = "ESRI Shapefile",
                   output_Vector=TRUE)

      cat("Les contours ont été générés avec succès \n")

      # Export définitif du .shp TOPO_line
      TOPO_line <- st_as_sf(line)
      st_crs(TOPO_line) <-2154
      TOPO_line <- st_intersection(TOPO_line, EMPRISE) # Intersection du .shp TOPO_line lissé avec le sf EMPRISE

      if(RES=="IGN (C) BD ALTI (R)") {
        TOPO_line <- smooth(TOPO_line, method = "ksmooth") # Lissage du .shp TOPO_line
        cat("Les contours ont été liséés avec succès \n")
      }

      if(!is.null(NAME)){ NAME <- paste0(NAME,"_TOPO_line.shp")
      } else {NAME <- "TOPO_line.shp"}

      st_write(TOPO_line, dsn= dirname(REP_SHP), layer = NAME, update=TRUE, delete_layer = TRUE,
               driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")

      cat(paste0("Le fichier ", NAME, " a été exporté dans", dirname(REP_SHP), "\n"))

      return(TOPO_line)

    } # Fin boucle MNT
  } # Fin boucle REP_SHP
} # Fin function
