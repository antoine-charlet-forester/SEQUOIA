\name{mc_linearize}
\encoding{UTF-8}
\alias{mc_linearize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Linearisation d un objet sf de class (MULTI)POLYGON
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
La fonction mc_linearize linearise un objet sf de class (MULTI)POLYGON en un objet sf de class LINESTRING épuré (sans doublons)

}
\usage{
mc_linearize(polygon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{polygon}{
%%     ~~Describe \code{x} here~~
objet sf de class (MULTI)POLYGON
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
  line <- mc_linearize(polygon)

}
