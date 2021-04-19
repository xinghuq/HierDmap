\name{HierDgenind}
\alias{HierDgenind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculateing genetic diversity profiles (based on Hill numbers) and diversity in a hierarchy.
%%  ~~function to do ... ~~
}
\description{Calculating genetic diversity profiles using self-defined hierarchical structure.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HierDgenind(x, pop_region, pop, q)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The genetic data, genlind format.
%%     ~~Describe \code{x} here~~
}
  \item{pop_region}{The population label in regions 
%%     ~~Describe \code{pop_region} here~~
}
  \item{pop}{The population labels
%%     ~~Describe \code{pop} here~~
}
  \item{q}{The order of diversity. 
%%     ~~Describe \code{q} here~~
}
}
\details{
This function calculates the diversity profiles based on Hill numbers. It especially good at deal with high throughout genotyping data. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Diversity profile,Dq0, Dq1, Dq2 in ecosystem, regions, and population per locus, per population.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Jombart, T. (2008). adegenet: a R package for the multivariate analysis of genetic markers. Bioinformatics, 24(11), 1403-1405.

Qin, X. (2019). HierDpart: partitioning hierarchical diversity and differentiation across metrics and scales, from genes to ecosystems. R package version 0.5. 0 https://cran. r-project. org/package= HierDpart.
%% ~put references to the literature/web site here ~
}
\author{qinxinghu@gmail.com
%%  ~~who you are~~
}

\examples{
library(adegenet)
data(eHGDP)
# example genepop file
### formating data, restructing the hierarchical data structure 
levels(eHGDP$pop)=eHGDP$other$popInfo$Population
###struct a table with individual region/pop labels
popstr= eHGDP$other$popInfo[match(eHGDP$pop,eHGDP$other$popInfo$Population),]
popstr$Population=factor(popstr$Population,levels=unique(popstr$Population))
popstr$Region=factor(popstr$Region,levels=unique(popstr$Region))
#library(poppr)
#eHGDP1=missingno(eHGDP, type = "mean",  quiet = FALSE, freq = FALSE)

eHGDP$tab=(tab(eHGDP, freq = FALSE, NA.method = "mean"))

HiereHGDPq0=HierDgenind(eHGDP,q=0,pop_region = popstr$Region,pop = popstr$Population)
HiereHGDPq1=HierDgenind(eHGDP,q=1,pop_region = popstr$Region,pop = popstr$Population)
HiereHGDPq2=HierDgenind(eHGDP,q=2,pop_region = popstr$Region,pop = popstr$Population)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Diversity profiles}% use one of  RShowDoc("KEYWORDS")
