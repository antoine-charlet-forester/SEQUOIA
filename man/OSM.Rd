\name{OSM}
\encoding{UTF-8}
\alias{OSM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Telechargement des donnees OpenStreetMap
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction OSM télécharge les données OSM autour d'un parcellaire cadastral (sf) et génère un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
OSM(PARCA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PARCA}{
%%     ~~Describe \code{x} here~~
sf du parcellaire cadastral
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Parmi les fichiers nécessaires :
  \item{COMS_polygon}{
Communes environnantes (.shp)
}
  \item{COMS_line}{
Contours des communes environnantes (.shp)
}
  \item{COM_line}{
Contours des communes environnantes (.shp)
}
  \item{COMS_point}{
Centroid des communes environnantes (.shp)
}
  \item{COM_point}{
Centroid des communes environnantes (.shp)
}
  \item{INFRA_polygone}{
Infrastructures cadatrées environnantes (sf)
}
  \item{INFRA_line}{
sf Infrastructures cadatrées environnantes (sf)
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
### Fonctionnement :
  OSM(PARCA)
}
