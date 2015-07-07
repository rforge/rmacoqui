\name{fuzzy.Clusters}
\alias{fuzzy.Clusters}
\title{Fuzzy-logic analysis of dendrogram clusters selected by the researcher}
\description{
This function provides fuzzy-logic descriptions and inter-relationships for all clusters in the dendrogram on which the analyst wishes to put the focus.
}
\usage{
fuzzy.Clusters(macoquires, grupos)
}
\arguments{
  \item{macoquires}{
  object resulting from function \code{\link{macoqui}}
  }
  \item{grupos}{
dendrogram clusters -identified by the analyst- on which the analysis is focused
}
}
\details{
Before applying \code{fuzzy.Clusters}, \code{\link{macoqui}} must be run, and outputs must be stored as object 'macoquires'.

grupos <- c(n, b, ...), where:
  
	n	dendrogram-node number (as in the dendrogram plot)
	b	node-branch corresponding to the cluster (1 if left, 2 if right)
	...	idem for all clusters to be analized
}
\value{
This function returns a list of 9 elements, some of which are shown in the console:
\item{ngrupos}{the input number of analysed groups}
\item{grupos}{the input \code{groups} object}
\item{desGrupos}{a description of the input groups, including their node and branch}
\item{elementosGrupos1}{}
\item{elementosGrupos2}{}
\item{ldifusaGrupos}{parameters to compute cluster fuzzy entropy, fuzzy union of clusters, and fuzzy intersection between clusters}
\item{mcardinales}{cardinals of fuzzy unions between clusters (top-right semi-matrix), and of fuzzy intersections between clusters (bottom-left semi-matrix)}
\item{mentropia}{cluster fuzzy parameters (cardinal, entropy, height) and number of species in the cluster; fuzzy inclusion of the cluster in other clusters; fuzzy overlap between clusters}
\item{datos}{the input species dataset}
}
\references{
}
\author{
Jesus Olivero, Ramon Hidalgo, Ana L. Marquez, A. Marcia Barbosa, Raimundo Real
}
\note{
}
\seealso{
\code{\link{macoqui}}, \code{\link{locCorotGrupos}}, \code{\link{ver.matRmacoqui}}
}
\examples{
data(amphib)
macoquires <- macoqui(amphib)
grupos <- c(12,1,12,2,13,1,13,2,15,1,10,1,10,2,17,2)
fuzzyres <- fuzzy.Clusters(macoquires, grupos)
}
\keyword{}
\keyword{}
