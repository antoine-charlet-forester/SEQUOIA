\name{CAGEF}
\encoding{UTF-8}
\alias{CAGEF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creation de donnees d'habillage "CAGEF"
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction CAGEF utilise les fonctions OSM, BDTOPO_HYDRO, IGN_VEG et ETALAB/BD_PARCA/EDIGEO pour générer un ensemble de .shp (EPSG 2154) et d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
CAGEF(PARCA, CODECA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PARCA}{
%%     ~~Describe \code{x} here~~
sf du parcellaire cadastral
}
  \item{CODECA}{
%%     ~~Describe \code{x} here~~
Code de référence des données cadastrales à utiliser 1: Etalab, 2: IGN (c) BD Parcellaire (r), 3: Edigeo
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Cf. Autres ressources
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
  CAGEF(PARCA=F, CODECA=1)
}
