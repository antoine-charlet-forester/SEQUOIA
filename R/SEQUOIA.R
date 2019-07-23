# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("svDialogs")) {install.packages("svDialogs")}
if (!require("rlang")) {install.packages("rlang")}

SEQUOIA <- function(enrg=FALSE) {

  form <- c("Téléchargements des données SEQUOIA",
            "Aide à la création d'une cartographie ponctuelle",
            "Outils cartographiques")

  RES1 <- dlgList(form, multiple = F, title = "SEQUOIA : Prenez de la hauteur sur votre forêt !", gui = .GUI)$res

  if (!length(RES1)) {
    warning("Aucune sélection effectuée > Traitement annulé \n")

  } else {

    if ("Téléchargements des données SEQUOIA" %in% RES1) { # Téléchargements des données SEQUOIA
      message("Téléchargements des données SEQUOIA")

      repRdata <- dlg_dir(default= getwd(), title = "Choisir le répertoire de telechargement des archives .Rdata")$res

      if(is_empty(repRdata)) {
        cat("Traitement annulé","\n" )
      } else {
        SEQUOIA:::INSEEtoRDATA(repRdata)
        SEQUOIA:::INPNtoRDATA(repRdata)
        SEQUOIA:::BDTOPO_HYDROtoSHP(F)
      }
    } # Fin Téléchargements des données SEQUOIA

    if ("Aide à la création d'une cartographie ponctuelle" %in% RES1) { # Aide à la création d'une cartographie ponctuelle

      form <- c("1 Conversion .html vers .xlsx",
                "2 Création PARCA et habillage",
                "3 Création UA",
                "4 Conversion ROAD vers ROUTE",
                "5 Finalisation UA et habillage")

      RES2 <- dlgList(form, multiple = T,
                      title = "Aide à la création d'une cartographie ponctuelle", gui = .GUI)$res

      filtre <- matrix(c("Classeur Excel", ".xlsx", "Classeur Excel 1999-2003", ".xls","Shapefile", ".shp", "All files", "*"),
                       4, 2, byrow = TRUE)

      if (!length(RES2)) {
        warning("Aucune sélection effectuée > Traitement annulé \n")

      } else {
        if ("1 Conversion .html vers .xlsx" %in% RES2) {
          message("1 Conversion .html vers .xlsx")


          rephtml <- dlg_dir(default= getwd(), title = "Choisir le dossier contenant les extraits de matrice cadastrale")$res

          if(is_empty(rephtml)) {
            cat("Traitement annulé","\n" )
          } else {
            SEQUOIA:::HTMLtoXLSX(rephtml, repRdata=NULL)
          }

        }
        if ("2 Création PARCA et habillage" %in% RES2) {
          message("2 Création PARCA et habillage")

          repxlsx <- tk_choose.files(caption = "Choisir le fichier .xlsx des references cadastrales", filters = filtre[1:2,])

          if(is_empty(repxlsx)) {
            cat("Traitement annulé","\n" )
          } else {
            SEQUOIA:::XLSXtoPARCA(repxlsx)
            SEQUOIA:::CAGEF(PARCA, CODECA)
          }

        }
        if ("3 Création UA" %in% RES2) {
          message("3 Création UA")

          SEQUOIA:::PARCAtoUA(rep=F)

        }
        if ("4 Conversion ROAD vers ROUTE" %in% RES2) {
          message("4 Conversion ROAD vers ROUTE")

          SEQUOIA:::ROADtoROUTE(repROAD=F)

        }
        if ("5 Finalisation UA et habillage" %in% RES2) {
          message("5 Finalisation UA et habillage")

          repUA <- tk_choose.files(caption = "Choisir le .shp des unités d'analyses (UA)", filters = matrix(c("Shapefile", ".shp"),1, 2, byrow = TRUE))

          if(is_empty(repUA)) {
            cat("Traitement annulé","\n" )
          } else {
            SEQUOIA:::UAtoUA(repUA)
            SEQUOIA:::UAtoSSPF(repUA)
            SEQUOIA:::UAtoTOPO(repUA)
          }
        }
      }

    } # Fin Aide à la création d'une cartographie ponctuelle

    if ("Outils cartographiques" %in% RES1) { # Outils cartographiques

      form <- c("MNT et courbes de niveau",
                "Zonnage environnementaux",
                "Création d'une fiche Climatologique")

      RES3 <- dlgList(form, multiple = T, title = "Outils cartographiques", gui = .GUI)$res

      if (!length(RES3)) {
        warning("Aucune sélection effectuée > Traitement annulé \n")

      } else {
        if ("MNT et courbes de niveau" %in% RES3) {
          message("MNT et courbes de niveau")

          NAME <- dlgInput("Entrer le nom du fichier de sortie (optionnel) : ",default = "")$res
          if(rlang::is_empty(NAME)) {NAME=NULL}
          SEQUOIA:::MNTonSHP(REP_SHP=F, NAME)
        }

        if ("Zonnage environnementaux" %in% RES3) {
          message("Zonnage environnementaux")

          if(!exists("repRdata")) {repRdata <- tk_choose.dir(default= getwd(), caption = "Choisir le répertoire de telechargement des archives .Rdata")}
          if(!exists("repPARCA")) {repPARCA <- tk_choose.files(caption = "Choisir le .shp du parcellaire cadastrale PARCA", filters = matrix(c("Shapefile", ".shp"),1, 2, byrow = TRUE))}

          SEQUOIA:::INPNonPARCA(repPARCA,paste(repRdata,"INPN.Rdata",sep="/"))

        }

        if ("Création d'une fiche Climatologique" %in% RES3) {
          message("Création d'une fiche Climatologique")

          if(!exists("NAME")) {NAME <- dlgInput("Entrer le nom du fichier de sortie: ",default = "")$res}
          if(exists("repout2")) { repPARCA <- paste(repout2, paste(NAME,"PARCA_polygon.shp",sep="_"), sep="/")} else {
            repPARCA <- tk_choose.files(caption = "Choisir le .shp du parcellaire cadastrale PARCA", filters = matrix(c("Shapefile", ".shp"),1, 2, byrow = TRUE))
          }
          if(!exists("repRdata")) {repRdata <- tk_choose.dir(default= getwd(), caption = "Choisir le répertoire de telechargement des archives .Rdata")}

          setwd("~/")
          repshp <- repPARCA
          save(repshp, repRdata, NAME, file = "PREPA-FICHE.Rdata")

          library(SEQUOIA)
          rep <- path.package("SEQUOIA", quiet = FALSE)
          rmd <- paste(rep, "CLIM.Rmd", sep="/")
          rmarkdown::render(rmd, prettydoc::html_pretty(theme = "cayman"), paste0(NAME,"_FICHE-CLIM.html"), dirname(dirname(dirname(repshp))), encoding = "UTF-8")

        }
       } # Fin Outils cartographiques
    }
  }
}
