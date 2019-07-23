# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("svDialogs")) {install.packages("svDialogs")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("lwgeom")) {install.packages("lwgeom")}
if (!require("R.utils")) {install.packages("R.utils")}
if (!require("rvest")) {install.packages("rvest")}
if (!require("xml2")) {install.packages("xml2")}
if (!require("readxl")) {install.packages("readxl")}
if (!require("rlang")) {install.packages("rlang")}
if (!require("smoothr")) {install.packages("smoothr")}

XLSXtoPARCA <- function(rep=T){
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .xlsx contenant les références cadastrales")
  }

# Import des données .xlsx
  XLSX <- read_excel(rep)
  cat("Le fichier .xlsx a été chargé avec succès \n")

# Création des répertoires de sorties
  dir.create(paste(dirname(rep),"SIG",sep="/"))
  dir.create(paste(dirname(rep),"SIG","0 SORTIE",sep="/"))
  dir.create(paste(dirname(rep),"SIG","1 RASTER",sep="/"))
  dir.create(paste(dirname(rep),"SIG","2 PSG",sep="/"))
  dir.create(paste(dirname(rep),"SIG","3 TEMPO",sep="/"))
  dir.create(paste(dirname(rep),"SIG","4 ZONAGE",sep="/"))

# Détection d'erreur de saisie
  XLSX$DOUBLON <- duplicated(XLSX)
  DOUBLON <- XLSX %>% filter(DOUBLON %in% TRUE)
  if (nrow(DOUBLON)>0){
    message("Attention: Des doublons ont été détectés dans la saisie")}
  XLSX <- unique(XLSX)[,-18]

# Choix de la source
  form <- c("1 cadastre.data.gouv.fr",
            "2 IGN (c) BD Parcellaire (r)",
            "3 cadastre.gouv.fr")
  RES1 <- dlgList(form, multiple = F, title = "Choix de la source cadastrale", gui = .GUI)$res

  if (!length(RES1)) { # Boucle RES1
    cat("Aucune sélection effectuée > Traitement annulé \n")

  } else { # Boucle RES1
    RES2 <- ""
    RES4 <- ""
    PARCA <-st_sf(st_sfc(crs = 2154))

# Chargement par "1 cadastre.data.gouv.fr"
    if ("1 cadastre.data.gouv.fr" %in% RES1) { # Boucle "1 cadastre.data.gouv.fr"
      message("1 cadastre.data.gouv.fr")

      ## Adresses de téléchargement et de destination
      DEP_URL <-"https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/departements"
      COM_URL <-"https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/communes"

      ## Import des données cadastrales PARCELLES à l'échelle COMMUNES
      XLSX <- XLSX %>%
        mutate(DEP_CODE = str_pad(DEP_CODE, 2, "left", pad = "0"),
               COM_CODE = str_pad(COM_CODE, 3, "left", pad = "0"),
               ID_CAD = paste(DEP_CODE,COM_CODE,sep = ""))

      ID_CAD <- as.data.frame(unique(XLSX["ID_CAD"]))

      PARCELLES_SF <- st_sf(st_sfc())
      ERR <- 0
      DEP_ERR <- list()

      for (a in 1:nrow(ID_CAD)) { # Boucle de téléchargement
        DEP <- str_sub(ID_CAD[a,1],1,2)
        COM <- ID_CAD[a,1]

        URL <- paste(COM_URL, DEP, sep="/")

        LISTE_URL <- list(html_attr(html_nodes(read_html(URL), "a"), "href"))

        if(grepl(COM, LISTE_URL)!=F) {
          URL <- paste(COM_URL, DEP, COM, paste("cadastre-", COM, "-parcelles.json.gz", sep=""), sep="/")

          TD = tempdir() # répertoire temporaire
          TF = tempfile(tmpdir=TD, fileext=".gz") # fichier temporaire
          download.file(URL, TF, method="libcurl") # Téléchargement du fichier URL sous le nom TF dans TD
          R.utils::gunzip(TF, remove=F) # Extraction de TF dans TD

          PARCELLE_SF <- st_read(str_replace(TF, ".gz", ""), quiet=T) # Lecture du fichier sf
          st_crs(PARCELLES_SF) <- st_crs(PARCELLE_SF) # Changement du système de projection

          PARCELLES_SF <- rbind(PARCELLES_SF,PARCELLE_SF)

          cat("La commune", COM, "a été téléchargée \n")
        } else {
          ERR <- ERR+1
          message("La commune ", COM, " n'est pas disponible : \n > La BD Parcellaire ", DEP," est nécessaire ! \n")
        }
      } # Fin de boucle de téléchargement

      if(nrow(PARCELLES_SF )>0){ # Boucle de contenu
        PARCELLES_SF<- st_sf(st_transform(PARCELLES_SF , 2154), agr="constant")

        ## Sélection des PARCELLES de la propriété
        DATA_PARCAS <- PARCELLES_SF %>% # Création de l'IDU
          mutate(IDU = str_sub(id,3,14))
        DATA_PARCAS <- DATA_PARCAS %>%
          filter(DATA_PARCAS$IDU %in% XLSX$IDU) # Filtre des polygones sur IDU

        ## Création du .shp de la propriété
        SHP <- merge(x = DATA_PARCAS, y = XLSX, by = "IDU") # Jointure des tables
        PARCA <- unique(SHP) %>% # Suppression doublon résiduel
          mutate(SURF_CA = contenance/10000) %>%
          dplyr::select(REG_CODE, REG_NOM, DEP_CODE, DEP_NOM, COM_CODE, COM_NOM, PROP, ADRESSE, IDU, PREFIXE:LIEUDIT,OCCUP_SOL,TX_BOISE,REV_CA,SURF_CA) # Mise en ordre des champs
        PARCA <- st_transform(PARCA, 2154)
      } # Fin de boucle de contenu

      ## Analyse de complément
      if(ERR>0){
        MSG <- paste("Une ou plusieurs communes sont indisponibles. \nLe chargement des BD Parcellaire indiquées est nécessaire.")
        RES2 <- winDialog(type = "okcancel", MSG)
      } else {
        CODE <- 1 # Code de création de l'habillage
      }
    } # Fin de boucle "1 cadastre.data.gouv.fr"

# Chargement par "2 IGN (c) BD Parcellaire (r)"
    if (("2 IGN (c) BD Parcellaire (r)" %in% RES1)||(RES2=="OK")) { # Boucle "2 IGN (c) BD Parcellaire (r)"
      message("2 IGN (c) BD Parcellaire (r)")

      ## Import du fichier
      RES3 <- dlg_dir(default = getwd(), title="Choisir le fichier contenant le parcellaire cadastral departemental", gui = .GUI)$res

      if (is_empty(RES3)) { # Boucle RES3
        cat("Fichier non sélectionné -> traitement annulé \n")

      } else { # Boucle RES3
        SHP <- st_read(paste(RES3,"PARCELLE.SHP", sep="/")) # Lecture du shapefile

        ## Sélection des PARCELLES de la propriété
        if(!("CODE_COM" %in% colnames(SHP))){
          SHP <- SHP %>% mutate(CODE_COM = CODE_CO)}

        SHP <- SHP %>%
          mutate(IDU = paste(CODE_COM,COM_ABS,SECTION,NUMERO, sep=""))

        BD_PARCAS <- SHP %>%
          filter(SHP$IDU %in% XLSX$IDU) # Filtre des polygones sur IDU

        ## Création du .shp de la propriété
        BD_PARCA <- merge(x = BD_PARCAS[,"IDU"], y = XLSX, by = "IDU", all.x=T, all.y=F) # Jointure des tables
        BD_PARCA <- unique(BD_PARCA) %>% # Suppression doublon résiduel
          dplyr::select(REG_CODE, REG_NOM, DEP_CODE, DEP_NOM, COM_CODE, COM_NOM, PROP, ADRESSE, IDU, PREFIXE:LIEUDIT,OCCUP_SOL,TX_BOISE,REV_CA,SURF_CA) # Mise en ordre des champs

        ## Fusion des PARCA
        if(nrow(BD_PARCA)>0){ # Boucle de contenu
          BD_PARCA <- st_transform(BD_PARCA, st_crs("+init=epsg:2154"))
          PARCA <- unique(rbind(PARCA, BD_PARCA))
          RES4 <- ""
          CODE <- 2
          assign("rep_BDPARCA", RES3, envir=globalenv())
        } else {
          MSG <- paste("Une ou plusieurs communes sont indisponibles. \nLe chargement des EDIGEO locals est nécessaire.")
          RES4 <- winDialog(type = "okcancel", MSG)
        } # Fin de boucle de contenu
      } # Fin de boucle RES3
    } # Fin de boucle "2 IGN (c) BD Parcellaire (r)"

# Chargement par "3 cadastre.gouv.fr"
    if (("3 cadastre.gouv.fr" %in% RES1) || (RES4=="OK")){ # Boucle "3 cadastre.gouv.fr"
      message("3 cadastre.gouv.fr")

      ## Import du fichier
      RES5 <- tk_choose.files(default = rep, caption = "Selectionner la commande .zip")
      unzip(RES5, exdir=dirname(RES5))

      ## Création du parcellaire
      LISTE_BZ <- list.files(dirname(RES5), "*.bz2", recursive=T)
      for (a in 1:length(LISTE_BZ)) {
        bunzip2(paste(dirname(RES5), LISTE_BZ[a], sep="/"))
      }

      LISTE_TAR <- list.files(dirname(RES5), "*.tar", recursive=T)
      for (a in 1:length(LISTE_TAR)) {
        untar(paste(dirname(RES5), LISTE_TAR[a], sep="/"), exdir=paste(dirname(RES5), dirname(LISTE_TAR[a]), sep="/"))
      }

      LISTE_THF <- list.files(dirname(RES5), "*.THF", recursive=T)

      PARCELLES_SF <- st_sf(st_sfc())
      for (a in 1:length(LISTE_THF)) {
        PARCELLE_SF <- st_read(paste(dirname(RES5),LISTE_THF[a],sep="/"),"PARCELLE_id", quiet=T)
        st_crs(PARCELLES_SF) <- st_crs(PARCELLE_SF) # Changement du système de projection
        PARCELLES_SF <- rbind(PARCELLES_SF,PARCELLE_SF)
      }

      ## Sélection des PARCELLES de la propriété
      if(str_count(XLSX[1,9])>12){XLSX <- XLSX %>% # Création de l'IDU
        mutate(IDU = str_sub(IDU,3,14))}
      EDIGEO_PARCAS <- PARCELLES_SF %>%
        filter(PARCELLES_SF$IDU %in% XLSX$IDU) # Filtre des polygones sur IDU

      ## Création du .shp de la propriété
      SHP <- merge(x = EDIGEO_PARCAS, y = XLSX, by = "IDU") # Jointure des tables
      EDIGEO_PARCA <- unique(SHP) %>% # Suppression doublon résiduel
        dplyr::select(REG_CODE, REG_NOM, DEP_CODE, DEP_NOM, COM_CODE, COM_NOM, PROP, ADRESSE, IDU, PREFIXE:LIEUDIT,OCCUP_SOL,TX_BOISE,REV_CA,SURF_CA) # Mise en ordre des champs

      ## Fusion des PARCA
      if(nrow(EDIGEO_PARCA)>0){ # Boucle de contenu
        EDIGEO_PARCA <- st_transform(EDIGEO_PARCA, 2154)
        PARCA <- unique(rbind(PARCA, EDIGEO_PARCA))
        CODE <-3
        rep_EDIGEO <- paste(dirname(RES5),dirname(dirname(LISTE_THF[1])), sep="/")
        assign("rep_EDIGEO", rep_EDIGEO, envir=globalenv())
      }
    } # Fin de boucle "3 cadastre.gouv.fr"

# Exportation du fichier .shp "PARCA"
    if(nrow(PARCA)>0){ # Boucle création "PARCA"

      PARCA <- unique(PARCA) # Suppression des doublons résiduels

      repout2 <- paste(dirname(rep),"SIG","2 PSG",sep="/")
      repout3 <- paste(dirname(rep),"SIG","3 TEMPO",sep="/")
      assign("repout2", repout2, envir=globalenv())
      assign("repout3", repout3, envir=globalenv())

      NAME <- dlgInput("Entrer le nom du fichier de sortie: ",default = "")$res # Commande d'entrée du nom des fichiers
      assign("NAME", NAME, envir=globalenv())

      SEQUOIA:::WRITE(PARCA, repout2, paste(NAME,"PARCA.shp",sep="_"))
      SEQUOIA:::WRITE(PARCA, repout2, paste(NAME,"PARCA_polygon.shp",sep="_"))

      assign("repPARCA", paste(repout2, paste(NAME,"PARCA_polygon.shp",sep="_"), sep="/"), envir=globalenv())

      assign("PARCA", PARCA, envir=globalenv())
      assign("CODECA", CODE, envir=globalenv())
    } # Fin boucle création PARCA

  } # Fin de boucle RES1

}



