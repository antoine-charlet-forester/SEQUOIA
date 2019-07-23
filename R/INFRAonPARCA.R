# Lancement des library
if (!require("tcltk")) {install.packages("tcltk")}
if (!require("sf")) {install.packages("sf")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("svDialogs")) {install.packages("svDialogs")}
if (!require("stringr")) {install.packages("stringr")}
if (!require("lwgeom")) {install.packages("lwgeom")}
if (!require("osmdata")) {install.packages("osmdata")}
if (!require("smoothr")) {install.packages("smoothr")}

# rep  <- tk_choose.files(caption = "Choisir le fichier .shp contenant le parcellaire cadastral")

INFRAonPARCA <- function(rep) {

# Import des données .shp
PARCA <- st_read(rep, options = "ENCODING=UTF-8", agr = "constant", crs=2154, quiet=T)  # Lecture du shapefile
cat(" \n", "Le fichier .shp a été chargé avec succès  \n")

# Recherche du nom
NAME <- str_sub(rep, str_locate_all(rep,'/')[[1]][nrow(str_locate_all(rep,'/')[[1]]),1]+1,str_locate(rep,'_PARCA')[1,1]-1)

# Téléchargement données OSM
  new_url <- "https://overpass.kumi.systems/api/interpreter"
  # new_url <- "https://lz4.overpass-api.de/api/interpreter"
  # new_url <- "http://overpass.openstreetmap.ie/api/interpreter"
set_overpass_url(new_url)

DATA <- opq(bbox=st_bbox(st_transform(st_buffer(st_combine(PARCA), 1000, nQuadSegs = 30), "+init=epsg:4326"))) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_xml(filename = 'INFRA_LINE')

INFRA_OSM_LINE <- st_transform(st_read('INFRA_LINE', layer = 'lines', quiet=T), 2154, stringsAsFactors = FALSE)
INFRA_OSM_LINE <- st_sf(as.data.frame(INFRA_OSM_LINE, options = "ENCODING=UTF-8"))


# Tempon de travail
T = 1000
PROP_TEMPON_4 <- st_union(st_buffer(PARCA, T-1, nQuadSegs = 30))
PROP_TEMPON_4 <- st_sf(st_cast(PROP_TEMPON_4, 'POLYGON'))
CONVEX_ALL <- st_sf(st_sfc())
for (c in 1:nrow(PROP_TEMPON_4)){
  CONVEX <- st_convex_hull(PROP_TEMPON_4[c,])
  st_crs(CONVEX_ALL) <- st_crs(CONVEX)
  CONVEX_ALL <- rbind(CONVEX_ALL, CONVEX)
}
PROP_TEMPON <- CONVEX_ALL

# Travail sur RD
type <- c('secondary', 'tertiary')
INFRA_RD_line <- st_intersection(st_sf(INFRA_OSM_LINE %>% filter(highway %in% type), agr="constant"), PROP_TEMPON)
INFRA_RD_line <- smooth(INFRA_RD_line, method = "ksmooth")
INFRA_RD_polygon <- st_sf(st_buffer(st_union(INFRA_RD_line), 6, 10))
INFRA_RD_polygon$TYPE = 'RD'

# Travail sur RC
type <- c('residential')
INFRA_RC_line <- st_intersection(st_sf(INFRA_OSM_LINE %>% filter(highway %in% type), agr="constant"), PROP_TEMPON)
INFRA_RC_line <- smooth(INFRA_RC_line, method = "ksmooth")
INFRA_RC_polygon <- st_buffer(st_sf(st_union(INFRA_RC_line)), 4, 10)
INFRA_RC_polygon$TYPE <- 'RC'

# Creation de INFRA_polygon
INFRA_RC_polygon <- st_difference(st_sf(INFRA_RC_polygon, agr="constant"), st_sf(INFRA_RD_polygon, agr="constant"))[,1]

Tab1 <- as.data.frame(INFRA_RD_polygon)
names(Tab1) <- c('geometry', 'TYPE')
Tab2 <- as.data.frame(INFRA_RC_polygon)
names(Tab2) <- c('TYPE','geometry')
INFRA_polygon <- st_sf(rbind(Tab1, Tab2))
INFRA_polygon <- st_make_valid(st_sf(st_cast(INFRA_polygon, 'POLYGON'), agr="constant"))

st_write(INFRA_polygon, dsn=dirname(rep),layer =paste(NAME,"TEST_polygon.shp",sep="_"), update=TRUE, delete_layer = TRUE, driver = "ESRI Shapefile", layer_options = "ENCODING=UTF-8", quiet =T)
SEQUOIA::MAKE_PRJ(dirname(rep),paste(NAME,"TEST_polygon",sep="_"))

# Creation de INFRA_line
INFRA_line <- st_node(st_cast(INFRA_polygon, 'MULTILINESTRING'))

UNION <- st_sf(st_union(st_buffer(st_union(INFRA_RD_line), 5.9999, 10),
                        st_buffer(st_union(INFRA_RC_line), 3.9999, 10)))


INFRA_line <- st_make_valid(st_difference(INFRA_line, UNION))

st_write(INFRA_line, dsn=dirname(rep),layer =paste(NAME,"TEST_line.shp",sep="_"), update=TRUE, delete_layer = TRUE, driver = "ESRI Shapefile", layer_options = "ENCODING=UTF-8", quiet =T)
SEQUOIA::MAKE_PRJ(dirname(rep),paste(NAME,"TEST_line",sep="_"))

}
