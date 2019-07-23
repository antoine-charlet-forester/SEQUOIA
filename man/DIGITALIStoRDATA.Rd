\name{DIGITALIStoRDATA}
\encoding{UTF-8}
\alias{DIGITALIStoRDATA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Recuperation des donnees DIGITALIS et creation d'une archive R.data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction \code{DIGITALIStoRDATA} télécharge depuis la plateforme ouverte SILVAE les données météorologiques DIGITALIS. La fonction les sauvegarde ensuite dans une archive \code{.Rdata} si un répertoire de sauvegarde est fournit.
}
\usage{
DIGITALIStoRDATA(repDIG)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repDIG}{
%%     ~~Describe \code{x} here~~
Répertoire d'enregistrement des données et de création du \code{.Rdata}. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction nécessite une opération manuelle d'extraction des archives \code{.7z} téléchargées.
}
\value{
%%  ~Describe the value returned
La fonction produit une archive .Rdata nécessaire à la fonction \code{DIGITALISonSHP}.
}
\references{
%% ~put references to the literature/web site here ~
Les codes des champs de l'INSEE sont explicités sur le site de l'institut :
\url{https://www.insee.fr/fr/information/3363504}
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

  DIGITALIStoRDATA(repDIG = F)
}
