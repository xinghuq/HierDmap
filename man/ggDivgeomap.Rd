\name{ggDivgeomap}
\alias{ggDivgeomap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geo-biodiversity profile distribution map
%%  ~~function to do ... ~~
}
\description{Plotting diversity profiles on a map. The genetic diversity profiles can be calculated using HierDq or HierDq_genlind function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggDivgeomap(data,x,y, scale = "medium", returnclass = "sf", 
size, title = "Global Genetic Diversity Map", 
subtitle = ("Scaled relative genetic diversity of HGDP populations"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The dataframe contains the longitude, latitude, and the geographic coordinates of the location.
%%     ~~Describe \code{data} here~~
}
 \item{x}{The longitude of the location.
%%     ~~Describe \code{data} here~~
}
 \item{y}{The latitude of the location.
%%     ~~Describe \code{data} here~~
}
  \item{scale}{scale of map to return, one of 110, 50, 10 or 'small', 'medium', 'large'
%%     ~~Describe \code{scale} here~~
}
  \item{returnclass}{'sp' default or 'sf' for Simple Features
%%     ~~Describe \code{returnclass} here~~
}
  \item{size}{The point size used to indicate the (relative) diversity.
%%     ~~Describe \code{size} here~~
}
  \item{title}{ Tittle of the plot, text.
%%     ~~Describe \code{tittle} here~~
}
  \item{subtitle}{Subtitle of the plot
%%     ~~Describe \code{subtitle} here~~
}
  \item{\dots}{Aesthetic passing to ggplot
%%     ~~Describe \code{\dots} here~~
}
}
\details{ There are four ways of plotting geographic map in this package. "ggDiversity" uses ggplot2 and rnaturalearth.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A geographic map with points (size indicates diversity value) and densities.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Wickham, H. (2011). ggplot2. Wiley Interdisciplinary Reviews: Computational Statistics, 3(2), 180-185.

South, A. (2017). Rnaturalearth: world map data from natural earth. R package version 0.1. 0.

Qin, X. (2019). HierDpart: partitioning hierarchical diversity and differentiation across metrics and scales, from genes to ecosystems. R package version 0.5. 0 https://cran.r-project.org/package=HierDpart.

%% ~put references to the literature/web site here ~
}
\author{qinxinghu@gmail.com
%%  ~~who you are~~
}
\examples{
#Load data
data(Dprofile)
##plot
ggDivgeomap(data = Dprofile,x=Dprofile$Longitude, y= Dprofile$Latitude,
scale = "medium", returnclass = "sf",size=normalize(Dprofile$Dq1)*10,
title="Human Population Genetic Diversity Map",
subtitle=("Scaled relative genetic diversity of HGDP populations"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Biogeographic map}% use one of  RShowDoc("KEYWORDS")
