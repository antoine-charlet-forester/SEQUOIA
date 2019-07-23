# Lancement des library
if (!require("sf")) {install.packages("sf")}
if (!require("stringr")) {install.packages("stringr")}

WRITE <- function(sf, repout, nom){

  st_write(sf, dsn=repout, layer =nom, update=TRUE, delete_layer = TRUE,
           driver = "ESRI Shapefile", quiet =T, layer_options = "ENCODING=UTF-8")
  SEQUOIA:::MAKE_PRJ(repout,str_replace(nom,".shp",""))
  cat(paste("Le fichier",nom,"a été exporté dans",repout),"\n")

}
