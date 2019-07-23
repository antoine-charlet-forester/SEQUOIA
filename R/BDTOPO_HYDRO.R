# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("lwgeom")) {install.packages("lwgeom")}
if (!require("svDialogs")) {install.packages("svDialogs")}

BDTOPO_HYDRO <- function(PARCA=T, repBDTOPO=T){
  options(warn=-1) # Désactivation des warnings
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
  if(isFALSE(repBDTOPO)) {
    repBDTOPO <- tk_choose.dir(default= getwd(), caption = "Choisir le répertoire de l'IGN (c) BD TOPO (r) Hydrologie")
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

# Fonction de lecture
  REG_CODE <- unique(as.data.frame(PARCA[,"REG_CODE"])[1])

  LISTE_SHP <- c("CANALISATION_EAU",
                 "HYDRONYME",
                 "POINT_EAU",
                 "RESERVOIR_EAU",
                 "SURFACE_EAU",
                 "TRONCON_EAU")

  HYDRO <- function(REG_CODE, N){ # Début fonction HYDRO
    SHPS <- st_sf(st_sfc())

    for (a in 1:length(REG_CODE)){
      rep <- paste(repBDTOPO, as.character(REG_CODE[a,1]), sep="/")
      SHP <- st_read(paste(rep, paste0(paste(LISTE_SHP[N], as.character(REG_CODE[a,1]), sep="_"),".shp"), sep="/"), quiet=T)
      st_crs(SHPS) <- st_crs(SHP)
      SHPS <- rbind(SHPS, SHP)
    }

    if(nrow(SHPS)>0){
      SHPS <- st_make_valid(st_transform(SHPS, 2154))
      SHP <- st_intersection(SHPS, TEMPON4)
      return(SHP)
    }
  } # Fin fonction hydro

# Création de l'objet sf HYDRO_polygon
  HYDRO_polygon <- st_sf(st_sfc())
  st_crs(HYDRO_polygon) <- st_crs(2154)

  RESERVOIR_EAU <- HYDRO(REG_CODE, N=4)
  if(nrow(RESERVOIR_EAU)>0){
    RESERVOIR_EAU$TYPE = "RESO"
    HYDRO_polygon <- rbind(HYDRO_polygon, RESERVOIR_EAU[, "TYPE"])
  }

  SURFACE_EAU <- HYDRO(REG_CODE, N=5)
  if(nrow(SURFACE_EAU)>0){
    PERMANENT <- SURFACE_EAU %>% filter(REGIME %in% "Permanent")
    if(nrow(PERMANENT)>0){PERMANENT$TYPE = "SURFO"
    PERMANENT <- PERMANENT[, "TYPE"]}

    INTERMITENT <- SURFACE_EAU %>% filter(REGIME %in% "Intermittent")
    if(nrow(INTERMITENT)>0){INTERMITENT$TYPE = "SURFOi"
    INTERMITENT <- INTERMITENT[, "TYPE"]}

    SURFACE_EAU <- rbind(PERMANENT, INTERMITENT)

    HYDRO_polygon <- rbind(HYDRO_polygon, SURFACE_EAU)
  }

  assign("HYDRO_polygon", HYDRO_polygon, envir=globalenv())
  cat("L'object sf HYDRO_polygon a été ajouté à l'environnement \n")

# Création de l'objet sf HYDRO_line
  HYDRO_line <- st_sf(st_sfc())
  st_crs(HYDRO_line) <- st_crs(2154)

  CANALISATION_EAU <- HYDRO(REG_CODE, N=1)
  if(nrow(CANALISATION_EAU)>0){
    CANALISATION_EAU$TYPE = "CANO"
    HYDRO_line <- rbind(HYDRO_line, CANALISATION_EAU[, "TYPE"])
  }

  TRONCON_EAU <- HYDRO(REG_CODE, N=6)
  if(nrow(TRONCON_EAU)>0){ # Boucle Tronçon_Eau
    PERMANENT <- TRONCON_EAU %>% filter(REGIME %in% "Permanent")
    if(nrow(PERMANENT)>0){
      PERMANENT$TYPE = "RU"
      PERMANENT$NAME <- PERMANENT$NOM
      PERMANENT <- PERMANENT[, c("TYPE","NAME")]}

    INTERMITENT <- TRONCON_EAU %>% filter(REGIME %in% "Intermittent")
    if(nrow(INTERMITENT)>0){
      INTERMITENT$TYPE = "RUi"
      INTERMITENT$NAME <- INTERMITENT$NOM
      INTERMITENT <- INTERMITENT[, c("TYPE","NAME")]}

    TRONCON_EAU2 <- rbind(PERMANENT, INTERMITENT)

    HYDRO_line <- rbind(HYDRO_line, TRONCON_EAU2)
    for (b in 1:nrow(HYDRO_line)) { # Suppression des 'NR'
      VALUE <- as.data.frame(HYDRO_line[b,"NAME"])[1,1]
      if (!is.na(VALUE)){
        if (VALUE=='NR'){HYDRO_line[b,"NAME"]=as.character(NA)}
      }
    }
    SHP <- st_make_valid(st_combine(HYDRO_polygon))
    HYDRO_line <- st_difference(HYDRO_line, SHP)
  } # Fin de boucle Tronçon_Eau

  assign("HYDRO_line", HYDRO_line, envir=globalenv())
  cat("L'object sf HYDRO_line a été ajouté à l'environnement \n")

# Création de l'objet sf HYDRO_point
  HYDRO_point <- st_sf(st_sfc())
  st_crs(HYDRO_point ) <- st_crs(2154)

  HYDRONYME <- HYDRO(REG_CODE, N=2)
  if(nrow(HYDRONYME)>0){
    HYDRONYME$TYPE = "NOMO"
    HYDRONYME$NOM = HYDRONYME$NOM
    HYDRONYME$ROT = as.integer(NA)
    HYDRO_point <- rbind(HYDRO_point, HYDRONYME[, c("TYPE","NOM","ROT")])
  }

  POINT_EAU <- HYDRO(REG_CODE, N=3)
  if(nrow(HYDRONYME)>0){
    POINT_EAU$TYPE = "PTSO"
    POINT_EAU$NOM = as.character(NA)
    POINT_EAU$ROT = as.integer(NA)
    HYDRO_point <- rbind(HYDRO_point, POINT_EAU[, c("TYPE","NOM","ROT")])
  }

  assign("HYDRO_point", HYDRO_point, envir=globalenv())
  cat("L'object sf HYDRO_point a été ajouté à l'environnement \n")
  options(warn=1) # Activation des warnings
}

