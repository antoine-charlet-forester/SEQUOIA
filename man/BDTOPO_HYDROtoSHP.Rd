\name{BDTOPO_HYDROtoSHP}
\encoding{UTF-8}
\alias{BDTOPO_HYDROtoSHP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Telechargement des données IGN (c) BD TOPO (r) Hydrographie
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction \code{BDTOPO_HYDROtoSHP} télécharge depuis un serveur Google Drive les données IGN (c) BD TOPO (r) Hydrographie et les enregistre dans l'arborescence choisie.
}
\usage{
BDTOPO_HYDROtoSHP(repBD)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repBD}{
%%     ~~Describe \code{x} here~~
Répertoire d'enregistrement des données. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction télécharge des \code{.shp} nécessaires à la fonction \code{\link{BDTOPO_HYDRO}}.
}
\value{
%%  ~Describe the value returned
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

  BDTOPO_HYDROtoSHP(F)
}
