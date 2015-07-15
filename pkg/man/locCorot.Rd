\name{locCorot}
\alias{locCorot}
\title{
Parameters for chorotype mapping
}
\description{
This function provides ways for chorotype mapping. For each locality (rows in data), values of four parameters are given. Pi: Presence of chorotype i (presence/absence of at least one species of the chorotypical cluster); SRi: Species Richness of chorotype i (number of species of the chorotypical cluster); MMDi: Maximum Membership Degree-of any species-in chorotype i (fuzzy version of Pi); FSRi: Fuzzy Species Richness (fuzzy version of SRi, i.e sum of all species' degrees of membership in chorotype i).
}
\usage{
locCorot(macoquires)
}
\arguments{
  \item{macoquires}{object resulting from function \code{\link{macoqui}}}
}
\details{
Before applying \code{locCorot}, \code{\link{macoqui}} must be run, and outputs stored as object 'macoquires'.

For \code{locCorot} to be run, function \code{\link{macoqui}} must be executed with a presences/absences data set. 
}
\value{
This function returns (invisibly) the \code{macoquires} input list with one added element, which is also shown in the console:
\item{locCorotipos}{locality identification (correlative numbers starting in 1 are given to lines in data); Pi, SRi, MMDi and FSRi values of every chorotype i in each locality}
}
\references{
}
\author{
Jesus Olivero, Ramon Hidalgo, Ana L. Marquez, A. Marcia Barbosa, Raimundo Real
}
\note{
This function can take some computing time.
}
\seealso{
\code{\link{macoqui}}, \code{\link{ver.matRmacoqui}}
}
\examples{
data(amphib)
macoquires <- macoqui(amphib)
locs <- locCorot(macoquires) 
}
\keyword{}
\keyword{}
