\name{eHGDP}
\alias{eHGDP}
\docType{data}
\title{HGDP-CEPH dataset
%%   ~~ data name/kind ... ~~
}
\description{Worldwide human population microsatellite data from human genetic dievrsity project HGDP consists of 1350 individuals from 79 populations genotyped at 678 microsatellite loci (Cann HM et al 2002).The datasets is used in adegenet package. See adegenet.  
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("eHGDP")}

\format{eHGDP is a genind object with a data frame named popInfo as supplementary component (eHGDP@other$popInfo), which contains the following variables.

   Population: 
   {a character vector indicating populations.}
  
   Region: 
   {a character vector indicating the geographic region of each population.}
  
   Label: 
   {a character vector indicating the correspondence with population labels used in the genind object (i.e., as output by pop(eHGDP)).}
  
   Latitude, Longitude:
   {geographic coordinates of the populations, indicated as north and east degree.}
  
  Dq0, Dq1, Dq2: 
  {Genetic diversity of order q=0, 1, 2, details can be found in HierDq function.}
  
}

\source{Human Genome Diversity Project (HGDP) and Centre d'Etude du Polymorphisme Humain (CEPH). 
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Cann HM, de Toma C, Cazes L, Legrand MF, Morel V, et al. (2002) A human genome diversity cell line panel. Science 296: 261-262.

Jombart, T., Devillard, S. and Balloux, F. Discriminant analysis of principal components: a new method for the analysis of genetically structured populations. Submitted to BMC genetics.
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(eHGDP)
}
\keyword{datasets}
