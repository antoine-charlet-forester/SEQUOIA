\name{IGN_VEG}
\encoding{UTF-8}
\alias{IGN_VEG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Telechargement de vegetation depuis IGN (c) BDTOPO (r) ou OCS_GE (r)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction IGN_VEG charge les données de végétations depuis IGN (c) BDTOPO (r) ou OCS_GE (r) autour d'un parcellaire cadastral (sf) et génère un ensemble d'objet sf nécessaires à la réalisation d'une cartographie forestière ponctuelle.
}
\usage{
IGN_VEG(PARCA, repVEG)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PARCA}{
%%     ~~Describe \code{x} here~~
sf du parcellaire cadastral.
}
  \item{repVEG}{
%%     ~~Describe \code{x} here~~
Répertoire de la IGN (c) BDTOPO (r) ou OCS_GE (r) format départemental
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Les objets sf suivants sont produits :
  \item{VEG_polygon}{
Polygones des surfaces forestières (sf)
}
  \item{VEG_line}{
Lignes contours surfaces forestières(sf) (TYPE=VEG)
}
  \item{VEG_point}{
Points natures de culture (sf). "FEV"=Feuillus; "REV"= Résineux.
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
  IGN_VEG(PARCA=F, repVEG=F))
}
