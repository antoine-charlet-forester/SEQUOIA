\name{MAKE_PRJ}
\encoding{UTF-8}
\alias{MAKE_PRJ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Generation d'un .prj sous EPSG 2154
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction st_write du package sf génère un problème
d EPSG pour les fichiers exporté avec le  système de
projection EPSG: 2154 RGF93_Lambert_93. A la lecture
sous QGis, le système lu est l EPSG: 102110
RGF_1993_Lambert_93 et non l EPSG 2154.

La fonction MAKE_PRJ génère un fichier .prj correctif
écrasant le fichier .prj produit par la fonction
st_write imposant le système de projection EPSG: 2154
pour parfaire la lecture sous Qgis.
}
\usage{
MAKE_PRJ(rep, couche)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
Répertoire d'export du fichier.
}
  \item{couche}{
%%     ~~Describe \code{x} here~~
Non du fichier (sans extension).

Le nom du fichier réintègre automatiquement l'extension.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
  rep <- getwd()
  x <- test

  MAKE_PRJ(rep, couche)
  Le fichier test.prj a été exporté dans C:/Users/Utilisateur/Documents/SEQUOIA/
}
