\name{ver.matRmacoqui}
\alias{ver.matRmacoqui}
\title{
Friendly visualization of RMacoqui outputs
}
\description{
This function allows a user-friendly visualization of objects resulting from functions \code{\link{macoqui}}, code{\link{locCorot}}, \code{\link{fuzzy.Clusters}} and \code{\link{locCorotGrupos}}. The results listed in these objects are visualized as tables, and are easily exportable as .txt files.
}
\usage{
ver.matRmacoqui(x)
}
\arguments{
  \item{x}{
object resulting from function \code{\link{macoqui}} or from function \code{\link{fuzzy.Clusters}}.
}
}
\details{
}
\value{
The returned output is a Tcl/Tk interface (which appears in a separate window) with a menu of links.

The following links are provided in the menu after \code{ver.matRmacoqui(macoquires)}:

\item{Similarity Initial Order}{}
\item{Similarity Cluster Order}{}
\item{Significant Similarity}{}
\item{Intermediate MACOQUI Parameters}{}
\item{MACOQUI Parameters}{}
\item{Chorotype Report}{}
\item{Degree of Membership in Chorotypes}{}
\item{Intermediate Fuzzy Parameters}{}
\item{Cardinality Intersection-Union between Chorotypes}{}
\item{Fuzzy Parameters}{}
\item{Chorotypes in Localities}{}

The following links are provided in the menu after \code{ver.matRmacoqui(fuzzyres)}:

\item{Cluster Description}{}
\item{Species Presences in Clusters}{}
\item{Degree of Membership in Clusters}{}
\item{Intermediate Fuzzy Parameters}{}
\item{Cardinality Intersection-Union between Clusters}{}
\item{Fuzzy Parameters}{}
\item{Clusters in Localities}{}
}
\references{
}
\author{
Jesus Olivero, Ramon Hidalgo, Ana L. Marquez, A. Marcia Barbosa, Raimundo Real}
\note{
Before running \code{ver.matRmacoqui}, the \pkg{tcltk} package must be installed and loaded.
}
\seealso{
\code{\link{macoqui}}, \code{\link{locCorot}}, \code{\link{fuzzy.Clusters}}, \code{\link{locCorotGrupos}}
}
\examples{
## ver.matRmacoqui for 'macoqui', not including link to 'Chorotypes in Localities':

data(amphib)
macoquires <- macoqui(amphib)
ver.matRmacoqui(macoquires) 


## ver.matRmacoqui for 'macoqui', including link to 'Chorotypes in Localities':

data(amphib)
macoquires <- macoqui(amphib)
locs <- locCorot(macoquires)
ver.matRmacoqui(locs) 


## ver.matRmacoqui for 'fuzzy.Clusters', not including link to 'Chorotypes in Localities':

data(amphib)
macoquires <- macoqui(amphib)
grupos <- c(12,1,12,2,13,1,13,2,15,1,10,1,10,2,17,2)
fuzzyres <- fuzzy.Clusters(macoquires, grupos)
ver.matRmacoqui(fuzzyres)


## ver.matRmacoqui for 'fuzzy.Clusters', including link to 'Chorotypes in Localities':

data(amphib)
macoquires <- macoqui(amphib)
grupos <- c(12,1,12,2,13,1,13,2,15,1,10,1,10,2,17,2)
fuzzyres <- fuzzy.Clusters(macoquires, grupos)
fuzzylocs <- locCorotGrupos(fuzzyres, grupos)
ver.matRmacoqui(fuzzylocs)
}
\keyword{}
\keyword{}
