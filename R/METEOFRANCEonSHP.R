# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("nngeo")) {install.packages("nngeo")}
if (!require("rlang")) {install.packages("rlang")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("units")) {install.packages("units")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("googledrive")) {install.packages("googledrive")}

METEOFRANCEonSHP <- function(repshp = T){
  if(isFALSE(repshp)) {repshp <- tk_choose.files(caption = "Choisir le fichier .shp")}
  if (is_empty(repshp)){
    warning("Pas de fichier sélectionné >> Traitement annulé")
  } else {
    # Lecture du fichier .shp
    SHP <- st_read(repshp, options = "ENCODING=UTF-8", quiet=T, agr='constant')

    # Téléchargement des données METEOFRANCE
    URL <- "https://drive.google.com/file/d/1lHiVhkmdRdJ_IIoKb-5u2qmQGizNg2q7/view?usp=sharing"
    temp <- tempfile(fileext = ".zip")
    message("Si un message 'sélection' apparrait, tapez 1 \n")
    dl <- drive_download(as_id(URL), path = temp, overwrite = TRUE)
    out <- unzip(temp, exdir = tempdir())
    METEOFRANCE_sf_point <- st_read(out, quiet=T) # Lecture du fichier sf

    # Détermination de la station Météo France la plus proche
    CENTROID_point <- st_transform(st_centroid(st_combine(SHP)),2154)
    METEOFRANCE_sf_point <- st_transform(METEOFRANCE_sf_point, 2154)
    pts <- st_nn(CENTROID_point, METEOFRANCE_sf_point, sparse = TRUE, k = 1, maxdist = Inf,
                 returnDist = FALSE, progress = TRUE)[[1]][1] # renvoie la ligne de la station la plus proche
    DISTANCE <- round(as.numeric(st_distance(CENTROID_point, METEOFRANCE_sf_point[pts,])[1,1])/1000,2)

    # Récupération des données Météo France
    ID <- as.data.frame(METEOFRANCE_sf_point[pts,"ID"])[,-2] # ID de la station
    STATION  <- as.data.frame(METEOFRANCE_sf_point[pts,"STATION"])[,-2] # Nom de la station
    ALTITUDE <- as.data.frame(METEOFRANCE_sf_point[pts,"ALTITUDE"])[,-2] # Nom de la station

    FICHE_URL <- "https://donneespubliques.meteofrance.fr/FichesClim/FICHECLIM_"
    if(ID > 9999999){ b <- as.character("") } else  { b <- "0" }
    URL <- paste0(FICHE_URL, b, ID, ".data")
    TD = tempdir() # répertoire temporaire
    TF = tempfile(tmpdir=TD, fileext=".data") # fichier temporaire
    download.file(URL, TF, method="libcurl")

    CSV <- read.csv(TF, sep = ";", dec=".", quote="", fill=TRUE, nrows=4, encoding="UTF-8")

    # Création du tableau et mise en forme
    MF_df <- read.csv(TF, header=FALSE, sep = ";", skip=7, dec=".", quote=";",
                           fill=TRUE, nrows=231, encoding="UTF-8") # Lecture du fichier CSV
    for (i in 1:nrow(MF_df)){if(MF_df[i,2]=="     Janv."){r <- i-1}}
    MF_df <- MF_df[-c(1:r),]

    data <- MF_df[,2:14]
    data <- dplyr::na_if(data, "")
    data <- data[rowSums(is.na(data)) == 0,]
    data <- dplyr::na_if(data, "         .")
    data <- dplyr::na_if(data, "         -")

    col <- as.character(MF_df[,1])
    col <- dplyr::na_if(col, "            ")
    col <- data.frame(col)
    col <- col[rowSums(is.na(col)) == 0,]
    col <- data.frame(col)
    col$b <- ""
    for (i in 1:nrow(col)){
      if(grepl("Statistiques établies", as.character(col[i,1]))){
        col<-col[-i,]
      }
    }
    for (i in 1:nrow(col)){
      if(str_sub(as.character(col[i,1]),1,8)=="(Records"){
        col[i-1,2]<-as.character(col[i,1])
        col[i,]<-NA
      }
      if(isTRUE(grepl("=",as.character(col[i,1])))){
        col[i,2] <- as.character(col[i,1])
        col[i,1] <- as.character(col[i-1,1])
      }
    }
    for (i in 1:nrow(col)){
      if(!is.na(col[i,1])){
        if((as.character(col[i,2])=="") & (grepl("Nombre moyen de jours avec", as.character(col[i,1])))){
          col[i,]<-NA
        }
      }
    }
    for (i in 1:nrow(col)){
      if(!is.na(col[i,1])){
        if((grepl("58 km/h", as.character(col[i,2])))&(as.character(col[i,1])=="Nombre moyen de jours avec rafales")){
          col[i,]<-NA
        }
      }
    }
    for (i in 1:nrow(col)){
      if(!is.na(col[i,1])){
        if(as.character(col[i,1])=="Rr : Hauteur quotidienne de précipitations"){
          col[i,]<-NA
        }
      }
    }
    for (i in 1:nrow(col)){
      if(!is.na(col[i,1])){
        if(as.character(col[i,1])=="Données non disponibles"){
          col[i,]<-NA
          col[i-1,]<-NA
        }
      }
    }
    for (i in 1:nrow(col)){
      if(!is.na(col[i,1])){
        if(as.character(col[i,1])=="- : donnée manquante"){
          col[i,]<-NA
        }
      }
    }
    for (i in 1:nrow(col)){
      if(!is.na(col[i,1])){
        if(as.character(col[i,2])=="(Tn=Température minimale. Tx=Température maximale)"){
          col[i,]<-NA
        }
      }
    }
    col <- col[rowSums(is.na(col)) == 0,]
    STAT <- as.character(col[nrow(col),1])
    col <- col[-nrow(col),]

    METEOFRANCE_df <- cbind(col, data[-1,])

    MOIS <- c("VARIABLE", "DETAIL", str_replace_all(colnames(data[1,1:13]) <- as.character(unlist(data[1,1:13][1,])),"     ",""))
    colnames(METEOFRANCE_df) <- MOIS

    METEOFRANCE_df <- METEOFRANCE_df %>%
      mutate(VARIABLE=as.character(VARIABLE),
             DETAIL=as.character(DETAIL))

    # Sortie du tableur
    assign("ID", ID, envir=globalenv())
    assign("DISTANCE", DISTANCE, envir=globalenv())
    assign("STATION", STATION, envir=globalenv())
    assign("ALTITUDE", ALTITUDE, envir=globalenv())
    assign("STAT", STAT, envir=globalenv())
    cat("Le tableur METEOFRANCE_df a été exporté \n")
    return(METEOFRANCE_df)
  }
}
