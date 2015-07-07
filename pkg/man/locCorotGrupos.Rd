\name{locCorotGrupos}
\alias{locCorotGrupos}
\title{
Parameters for cluster mapping
}
\description{
This function provides ways for cluster mapping. For each locality (lines in data), values of four parameters are given. Pi: Presence of the cluster i (presence/absence of at least one species of the cluster); SRi: Species Richness of the cluster i (number of species of the cluster); MMDi: Maximum Membership Degree-of any species-in the cluster i (fuzzy version of Pi); FSRi: Fuzzy Species Richness (fuzzy version of SRi, i.e sum of all species' degree of membership in the cluster i).
}
\usage{
locCorotGrupos(fuzzyres, grupos)
}
\arguments{
  \item{fuzzyres}{object resulting from function \code{\link{fuzzy.Clusters}}}
  \item{grupos}{dendrogram clusters-identified by the analyst-on which the analysis is focused (see \code{\link{fuzzy.Clusters}})}
}
\details{
Before applying \code{locCorotGrupos}, \code{\link{fuzzy.Clusters}} must be run, and outputs stored as object \code{fuzzyres}.

Function \code{locCorotGrupos} currently only works if \code{\link{fuzzy.Clusters}} was run with an object (\code{macoquires}) which resulted from executing \code{\link{macoqui}} with a presences/absences data set.
}
\value{
This function returns the input list (invisibly), with one added element which is also shown in the console:
\item{locGrupos}{locality identification (correlative numbers starting in 1 are given to rows in data); Pi, SRi, MMDi and FSRi values of every cluster i in each locality}
}
\references{
}
\author{
Jesus Olivero, Ramon Hidalgo, Ana L. Marquez, A. Marcia Barbosa, Raimundo Real
}
\note{
This function can take some computing time for large data sets.
}
\seealso{
\code{\link{fuzzy.Clusters}}, \code{\link{ver.matRmacoqui}}
}
\examples{
data(amphib)
macoquires <- macoqui(amphib)
grupos <- c(12,1,12,2,13,1,13,2,15,1,10,1,10,2,17,2)
fuzzyres <- fuzzy.Clusters(macoquires, grupos)
fuzzylocs <- locCorotGrupos(fuzzyres, grupos)
}
\keyword{}
\keyword{}
