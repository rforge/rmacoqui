\name{RMacoqui-package}
\alias{RMacoqui-package}
\alias{RMacoqui}
\docType{package}
\title{
Identifying and describing chorotypes based on species distributions}
\description{
RMacoqui develops a probability procedure for the identification of chorotypes, i.e. distribution patterns shared by a group of species, which can be operatively identified within an area (Baroni-Urbani et al., 1978). This method was first published by M�rquez et al. (1997), based on  a prior proposal for detecting biogeographic boundaries (Real et al., 1992), and with a mathematic rationale derived from McCoy et al. (1986).
The method for chorotype identification was later enhanced by Mu�oz et al. (2003) and Real et al. (2008), and was updated and contextualized under a fuzzy logic framework by Olivero et al. (2011).
RMacoqui outputs  are also useful as the basis for delimiting biogeographic regions and transition zones with the support of the fuzzy logic (Olivero et al. 2013).
}
\details{
\tabular{ll}{
Package: \tab RMacoqui\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-04-04\cr
License: \tab GPL-2\cr
}
}
\author{
Jesus Olivero, Ramon Hidalgo, Ana L. Marquez, A. Marcia Barbosa, Raimundo Real

Maintainer: Ramon Hidalgo <ramon@uma.es>
}
\references{
Baroni-Urbani C., Rufo S., Vigna-Taglianti A. (1978) Materiali per una biogeografia italiana fondata su alcuni generi di Coleotteri, Cicindelidi, Carabidi e Crisomelidi. Estratto della Memorie della Societa Entomologica Italiana 56:35-92.

Marquez A.L., Real R., Vargas J.M., Salvo A.E. (1997) On identifying common distribution patterns and their causal factors: a probabilistic method applied to pteridophytes in the Iberian Peninsula. Journal of Biogeography 24:613-631.

McCoy E.D., Bell S.S., Waters K. (1986) Identifying biotic boundaries along environmental gradients. Ecology 67:749-759.

Munoz A.R., Real R., Olivero J., Marquez A.L., Guerrero J.C., Barcena S.B., Vargas J.M. (2003) Biogeographical zonation of African hornbills and their biotic and geographic characterisations. Ostrich 74:39-47.

Olivero J., M�rquez A.L., Real R. (2013) Integrating fuzzy logic and statistics to improve the reliable delimitation of biogeographic regions and transition zones. Systematic Biology 62:1-21.

Olivero J., Real R., M�rquez A.L. (2011) Fuzzy chorotypes as a conceptual tool to improve insight into biogeographic patterns. Systematic Biology 60:645-660.

Real R., Olivero J., Vargas J.M. (2008) Using chorotypes to deconstruct biogeographical and biodiversity patterns: the case of breeding waterbirds in Europe. Global Ecology and Biogeography 17:735-746.

Real R., Vargas J.M., Guerrero J.C. (1992) Analisis biogeografico de clasificacion de areas y especies. Monograf�as de Herpetologia 2:73-84.
}
\keyword{ package }
\seealso{
}
\examples{

## EXAMPLE 1: Basic usage of function macoqui
##
## If the data set is a presences/absences matrix:

data(amphib)
macoquires <- macoqui(amphib)


## If the data set is a similarity matrix:

data(simil)
macoquires <- macoqui(simil, nloc=273, isprox=1, vmax=0.553, vmin=0.445)


## Friendly 'macoqui' output:

ver.matRmacoqui(macoquires)


## EXAMPLE 2: Basic usage of function locCorot
##
## Parameters for chorotype mapping (only available if 'data' is a presences/absences matrix), 
## and friendly output:

data(amphib)
macoquires <- macoqui(amphib)
locs <- locCorot(macoquires)
ver.matRmacoqui(locs)
 
## EXAMPLE 3: Basic usage of function fuzzy.Clusters
##
## Fuzzy-logic analysis of clusters selected by the researcher, and friendly output:

data(amphib)
macoquires <- macoqui(amphib)
grupos <- c(12,1,12,2,13,1,13,2,15,1,10,1,10,2,17,2)
fuzzyres <- fuzzy.Clusters(macoquires, grupos)
ver.matRmacoqui(fuzzyres)

## EXAMPLE 4: Basic usage of function locCorotGrupos
##
## Parameters for cluster mapping, and friendly output:

data(amphib)
macoquires <- macoqui(amphib)
grupos <- c(12,1,12,2,13,1,13,2,15,1,10,1,10,2,17,2)
fuzzyres <- fuzzy.Clusters(macoquires, grupos)
fuzzylocs <- locCorotGrupos(fuzzyres, grupos)
ver.matRmacoqui(fuzzylocs)

}
