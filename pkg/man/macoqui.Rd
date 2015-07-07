\name{macoqui}
\alias{macoqui}
\title{
Identify and describe chorotypes
}
\description{
A chorotype is a distribution pattern, shared by a group of species, which can be operatively identified within an area (Baroni-Urbani et al., 1978). The function 'macoqui' provides a set of outputs for the detection and description of chorotypes, based on similarities between species distributions. The most important outputs are: (1) a dendrogram showing a classification of species according to distribution similarities; (2) a 'chorotype report' that identifies chorotypical clusters (i.e. groups of species shaping each chorotype) with branches of the dendrogram. Other outputs describe chorotypes as fuzzy sets, and show the fuzzy relationships between all chorotypes.}
\usage{
macoqui(datos, lang = "en", isprox = 0, nloc = 0, vmax = 0, vmin = 0)
}
\arguments{
  \item{datos}{
  a matrix of data, which can be either presences/absences or similarities
  }
  \item{lang}{
lang="en" provides outputs with labels in English language (default option); lang="es" provides outputs with labels in Spanish language
}
  \item{isprox}{
isprox=1 if data is a similarity (or proximity) matrix, and isprox=0 if data is a presence/absence matrix (isprox=0 by default)
}
  \item{nloc}{
number of localities considered for computing similarity (only required if isprox=1; otherwise nloc=0, which is the default value)
}
  \item{vmax}{
critical value above which similarity is significant (required if isprox=1, it can be specified if isprox=0 as well; by default, vmax automatically chooses critical values for the Baroni-Urbani & Buser index)
}
  \item{vmin}{
critical value below which dissimilarity is significant (required if isprox=1, it can be specified if isprox=0 as well; by default, vmin automatically chooses critical values for the Baroni-Urbani & Buser index)
}
}
\details{
The 'data' set is, by default, a matrix of presences (1) and absences (0) of species (columns) in localities (rows). In this version, headers are only admitted in columns, and empty columns produce a function error. By default, 'macoqui' uses the Baroni-Urbani & Buser (1976) index to calculate similarities between distributions, and automatically estimates significant similarities (+) and differences (-), according to the table of critical values (vmax and vmin) of this index. For other indices, it is necessary to provide a similarity matrix (isprox=1), and values for nloc, vmax and vmin.
}
\value{
This function returns a list of 30 elements, some of which are returned invisibly and some of which are also shown in the console:
\item{nvar}{number of species (number of columns in data)}
\item{nval}{number of localities (number of rows in data, excluding header)}
\item{extremos}{list of the 2 critical values, \code{vmax} and \code{vmin}}
\item{metodo}{similarity index employed: Baroni-Urbani & Buser}
\item{mprox}{similarity matrix, columns arranged as in data}
\item{mproxOrdc}{similarity matrix, columns arranged as in the dendrogram}
\item{msignosC}{significant similarity matrix; significant similarities represented as '+', significant dissimilarities represented as '-'}
\item{dend}{a dendrogram in which species distributions are classified}
\item{macdatos}{similarity values of each dendrogram node, and intermediate parameters for chorotype search}
\item{macres}{Macoqui parameters for chorotype search in each dendrogram node}
\item{ncor}{number of chorotypes identified}
\item{CorElementos}{chorotype composition: species (numbered as arranged in data) and the chorotype of which they are chorotypical clusters (0 means "species not classified within any chorotype"); degree of membership of every species in each chorotype, as defined by fuzzy logic}
\item{desCorotipos}{chorotype location in the dendrogram}
\item{ldifusa}{parameters to compute chorotype fuzzy entropy, fuzzy union between chorotypes, and fuzzy intersection between chorotypes}
\item{mcardinales}{cardinals of fuzzy unions between chorotypes (top-right semi-matrix), and of fuzzy intersections between chorotypes (bottom-left semi-matrix)}
\item{mentropia}{chorotype fuzzy parameters (cardinal, entropy, height) and number of species in the chorotypical cluster; fuzzy inclusion of chorotypes in other chorotypes; fuzzy overlap between chorotypes}
\item{isprox}{the input \code{isprox} value}
\item{datos}{a data frame with the input data}

Another output provided is a plot of the \code{dend}rogram in which species distributions are classified where nodes are identified with numbers, for interpretation of the elements \code{macdatos}, \code{macres} and \code{desCorotipos}.
}
\references{
Baroni-Urbani C., Buser M.W. (1976) Similarity of binary data. Systematic Zoology 25:251-259.

Baroni-Urbani C., Rufo S., Vigna-Taglianti A. (1978) Materiali per una biogeografia italiana fondata su alcuni generi di Coleotteri, Cicindelidi, Carabidi e Crisomelidi. Estratto della Memorie della Societa Entomologica Italiana 56:35-92.
}
\author{
Jesus Olivero, Ramon Hidalgo, Ana L. Marquez, A. Marcia Barbosa, Raimundo Real
}
\note{
}

\seealso{
\code{\link{ver.matRmacoqui}}
}
\examples{
## If the data set is a presences/absences matrix:

data(amphib)
macoquires <- macoqui(amphib)
macoquires


## If the data set is a similarity matrix:

data(simil)
macoquires <- macoqui(simil, nloc=273, isprox=1, vmax=0.553, vmin=0.445)
macoquires
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
