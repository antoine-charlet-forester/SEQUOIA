\name{INFRA_POLYGONtoLINE}
\encoding{UTF-8}
\alias{INFRA_POLYGONtoLINE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Conversion de INFRA_POLYGON en INFRA_line
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction INFRA_POLYGONtoLINE récupère le .shp (EPSG 2154) des polygones des infrastructures et le reconverti .shp (EPSG 2154) des lignes.

Les recouvrements et une partie des extrémités sont supprimées au passage.

}
\usage{
INFRA_POLYGONtoLINE(rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
Répertoire du fichier .shp des polygones d'infrastructure (INFRA_polygone)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
  \item{INFRA_line}{
Contour des polygones
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
  rep  <- tk_choose.files(caption = "Choisir le fichier .shp INFRA_polygone")

  INFRA_POLYGONtoLINE(rep)
}
