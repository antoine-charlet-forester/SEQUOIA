\name{AURELHYtoRDATA}
\encoding{UTF-8}
\alias{AURELHYtoRDATA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Enregistrement des donnees AURELHY en .Rdata
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction AURELHYtoRDATA charge les données AURELHY depuis un dossier personnel et les enregistre en données .Rdata.
}
\usage{
AURELHYtoRDATA(rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rep}{
%%     ~~Describe \code{x} here~~
Répertoire des fichiers AURELHY. Si \code{FALSE}, la fonction génère une boite de dialogue de sélection du dossier.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La fonction produit une archive .Rdata nécessaire à la fonction \code{\link{AURELHYonSHP}}.
}
\value{
%%  ~Describe the value returned
La fonction produit une archive .Rdata.
}

\author{
%%  ~~who you are~~
Matthieu CHEVEREAU <\email{matthieuchevereau@yahoo.fr}>
}
\note{
%%  ~~further notes~~
Attention: Ce code est un code développeur. Il ne fonctionne que sur le poste de l'auteur à titre privé.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
### Fonctionnement :
  rep  <- tk_choose.dir(caption = "Choisir le répertoire de sortie")

  AURELHYtoRDATA(rep)
}
