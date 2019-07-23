\name{DIGITALISonSHP}
\encoding{UTF-8}
\alias{AURELHYonSHP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Climatologie DIGITALIS sur une zone d'etude
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction \code{DIGITALISonSHP} renseigne des données climatologiques sur une zone détude (\code{.shp} quelconque).

Les données climatiques Digitalis ont été obtenues à partir de jeux de données ponctuelles des stations Météo-France qui ont été spatialisées en utilisant des modèles statistiques. Ces modèles ont été élaborés à l’aide de variables topographiques, géographiques et d’occupation dominante du sol, dont la distribution spatiale est connue de façon relativement précise.

Les informations ci-dessous correspondantes aux normales annuelles déterminés par le modèle Digitalis sur une étendue de 2,5 km autour de la forêt pour la période 1961-1990. Les données mensuelles ne sont pas disponibles.
}
\usage{
DIGITALISonSHP(repshp, repRdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repshp}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier \code{.shp} de la zone d'étude. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du fichier.
}
  \item{repRdata}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier \code{.Rdata} contenant les données DIGITALIS. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction réclame une archive .Rdata externe non fournie.
}
\value{
%%  ~Describe the value returned
La fonction retourne un dataframe des données DIGITALIS. Ces renseignements sont fournies au mois et cumulés/moyennés à l'année.

}
\references{
%% ~put references to the literature/web site here ~
Les données sont explicitées sur le portail de SYLVAE:
\url{https://silvae.agroparistech.fr/home/?page_id=495}
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
  DIGITALISonSHP(repshp = F, repRdata = F)
}
