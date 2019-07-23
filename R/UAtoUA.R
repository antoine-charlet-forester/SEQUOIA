# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("units")) {install.packages("units")}
if (!require("data.table")) {install.packages("data.table")}

UAtoUA <- function(rep=T) {
  if(isFALSE(rep)){
    rep  <- tk_choose.files(caption = "Choisir le fichier .shp des unités d'analyses (UA)")

    NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_UA')[1,1]-1)
    assign("NAME", NAME, envir=globalenv())

    repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
    assign("repout2", repout2, envir=globalenv())
    repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
    assign("repout3", repout3, envir=globalenv())
  }

  # Import des données .shp
  UA <- st_read(rep,options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  cat("Le fichier .shp a été chargé avec succès  \n")

  LIST_SHP <- list.files(dirname(rep), "*.shp")
  setwd(dirname(rep))
  repPARCA <- str_replace(rep,"UA","PARCA")

  PARCA <- st_read(repPARCA,options = "ENCODING=UTF-8", quiet=T)  # Lecture du shapefile
  cat("Le fichier .shp a été chargé avec succès  \n")

  # Actualisation du champs PARFOR
  UA <- UA %>%
    mutate(PARFOR = as.character(paste(str_pad(N_PARFOR,2,"left",'0'),'.',str_pad(N_SSPARFOR,2,"left",'0'),sep='')),
           PARCA  = as.character(paste0(SECTION, ' ', N_PARCA)))

  # Détection d'erreurs de saisies
  SSPF <- unique(as.data.frame(UA[, "PARFOR"])[,-2])
  CODE=0
  for (a in 1:length(SSPF)){
    SSPF_UA <- UA %>% filter(PARFOR %in% SSPF[a])
    SSPF_UA <- as.data.frame(SSPF_UA)[,-ncol(SSPF_UA)]

    OCCUP_SOL <- as.data.frame(unique(SSPF_UA[, "OCCUP_SOL"]))
    if(nrow(OCCUP_SOL)>1){message("Plusieurs OCCUP_SOL pour la parcelle ", SSPF[a])
      CODE=CODE+1}

    PLT_TYPE <- as.data.frame(unique(SSPF_UA[, "PLT_TYPE"]))
    if(nrow(PLT_TYPE)>1){message("Plusieurs PLT_TYPE pour la parcelle ", SSPF[a])
      CODE=CODE+1}

    PLT_COMS <- as.data.frame(unique(SSPF_UA[, "PLT_COMS"]))
    if(nrow(PLT_COMS)>1){message("Plusieurs PLT_COMS pour la parcelle ", SSPF[a])
      CODE=CODE+1}

    AME_TYPE <- as.data.frame(unique(SSPF_UA[, "AME_TYPE"]))
    if(nrow(AME_TYPE)>1){message("Plusieurs AME_TYPE pour la parcelle ", SSPF[a])
      CODE=CODE+1}
  }

  if (CODE>0){ # Boucle erreur
    message(CODE, " anomalies détectées dans la table > Traitement annulé")

  } else {

    # Correction des SURF_CA
    PARCA$SURFACE <- PARCA$SURF_CA
    IDU <- as.data.frame(PARCA[c("IDU","SURFACE")])[,-3] # Réapitulatif des parcelles cadastrales
    SHP <- merge(UA, IDU, by = "IDU", all=T)
    SHP$SURF_CA <- SHP$SURFACE

    UA <- SHP[ , -which(names(SHP) %in% c("SURFACE"))] # Suppression du champ SURFACE

    # Création d'un id par UA IDUA
    UA$IDUA <- seq.int(nrow(UA))

    UA_COR <- st_sf(st_sfc())

    for (a in 1:nrow(IDU)) {
      IDUCA <- IDU[a,1]

      UA_TAB <- UA %>%
        filter(UA$IDU %in% IDUCA)

      # Calcul des SURF_CA
      UA_TAB$SURF_CA <- UA_TAB$SURF_CA*10000 # Calcul de la surface cadastrale

      # Calcul des SURF_SIG
      UA_TAB$SURF_SIG <- round(st_area(UA_TAB)) # Calcul de la surface cartographique

      # Détermination du coeff de correction
      TAB <- as.data.frame(UA_TAB) %>%
        group_by(IDU) %>% # Récapitulatif
        summarise(SIG = sum(SURF_SIG),
                  CA  = mean(SURF_CA)) %>%
        mutate(SURF_COEFF = round((CA/SIG),10))

      # Calcul des SURF_COR
      if ("SURF_COEFF" %in% names(UA_TAB)) {UA_TAB <- UA_TAB[ , -which(names(UA_TAB) %in% c("SURF_COEFF"))]}
      UA_TAB <- merge(UA_TAB, TAB[c("IDU","SURF_COEFF")], by = "IDU", all=T) %>%
        mutate(SURF_COEFF = as.numeric(SURF_COEFF),
               SURF_COR = round(SURF_SIG*SURF_COEFF,0),
               SURF_COR = round(SURF_COR,0))

      # Détermination de la difference restante
      SURF_CA <- set_units(IDU[a,2]*10000,m^2)
      DIFFERENCE <- SURF_CA-sum(UA_TAB$SURF_COR)
      # cat(paste0("a= ",a, " : ", DIFFERENCE, "\n"))

      # Correction de la surface
      if(abs(DIFFERENCE)>set_units(0,m^2)){
        MAX <- max(UA_TAB$SURF_COR)
        ROW <- grep(MAX, UA_TAB$SURF_COR)
        VALUE = as.data.frame(UA_TAB[ROW, "SURF_COR"])[1,1]
        UA_TAB[ROW, "SURF_COR"]=VALUE + DIFFERENCE
      }

      # Détermination de la difference restante
      SURF_CA <- set_units(IDU[a,2]*10000,m^2)
      DIFFERENCE <- SURF_CA-sum(UA_TAB$SURF_COR)
      #cat(paste0("b= ",a, " : ", DIFFERENCE, "\n"))

      # Simplification de UA_TAB
      UA_TAB <- UA_TAB[ , -which(names(UA_TAB) %in% c("IDUA","SURF_COEFF"))]

      # Création de UA_COR
      UA_TAB <- st_transform(UA_TAB, 2154)
      st_crs(UA_COR) <- st_crs(UA_TAB)
      UA_COR <- rbind(UA_COR, UA_TAB)
    }

    # Actualisation de UA
    UA_COR <- st_transform(UA_COR, 2154)
    UA_COR <- UA_COR %>%
      mutate(SURF_CA  = SURF_CA/10000,
             SURF_SIG = SURF_SIG/10000,
             SURF_COR = SURF_COR/10000) %>%
      dplyr::select(REG_CODE:ADRESSE,IDU,PREFIXE:N_PARCA, PARCA, LIEUDIT,PARFOR,N_PARFOR,N_SSPARFOR,GEOL_TYPE, OCCUP_SOL,PLT_TYPE,PLT_COMS,AME_TYPE,REV_CA,SURF_CA,SURF_SIG,SURF_COR,geometry)

    # Actualisation du .shp "UA_polygon"
    NAME = str_sub(rep,str_locate(rep,"PSG/")[1,2]+1,str_locate(rep,"_UA")[1,1]-1)
    assign("NAME", NAME, envir=globalenv())

    repout2 <- paste(dirname(dirname(dirname(rep))),"SIG","2 PSG",sep="/")
    assign("repout2", repout2, envir=globalenv())
    repout3 <- paste(dirname(dirname(dirname(rep))),"SIG","3 TEMPO",sep="/")
    assign("repout3", repout3, envir=globalenv())

    st_write(UA_COR, dsn=dirname(rep),layer =paste(NAME,"UA_polygon.shp",sep="_"), update=TRUE, delete_layer = TRUE, driver = "ESRI Shapefile", quiet=T, layer_options = "ENCODING=UTF-8")
    cat(paste("Le fichier",paste(NAME,"UA_polygon.shp",sep="_"),"a été exporté dans",dirname(rep)),"\n")
    SEQUOIA:::MAKE_PRJ(dirname(rep),paste(NAME,"UA_polygon",sep="_"))


    sortie <- as.data.frame(UA_COR)[,1:26]

    repOut <- paste(dirname(dirname(dirname(rep))), paste0(NAME, "_PSG",".xlsx"), sep="/")

    #options(warn=-1)
    #data.table::fwrite(sortie,"temp.csv")
    #sortie <- data.table::fread("temp.csv",encoding = "Latin-1")

    openxlsx::write.xlsx(sortie, repOut)
    cat(paste("Le tableur UA a été enregistré dans le repertoire : ", repOut),"\n")
    options(warn=1)
  } # Fin Boucle erreur
}
