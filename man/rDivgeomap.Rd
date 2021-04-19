\name{rDivgeomap}
\alias{rDivgeomap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Method of plot geographic diveristy map in R
%%  ~~function to do ... ~~
}
\description{Plot the geographic map of genetic diversity profiles.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rDivgeomap(x, y , xlim = c(-180, 180), ylim = c(-90, 90), 
asp = 1, bg = "aliceblue", cols = "white", fill = T, 
border = "lightgrey", wrap = c(-180, 180), 
col =grDevices::rgb(red = 0, green = 0, blue = 1, alpha = 0.3), 
size, pch = 20, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The longitude of the location
%%     ~~Describe \code{x} here~~
}
  \item{y}{The latitude of the location
%%     ~~Describe \code{y} here~~
}
  \item{xlim}{Used to plot the range of the longitude in world map.
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{The range of the latitude in a world map.
%%     ~~Describe \code{ylim} here~~
}
  \item{asp}{Plot parameter used in rgeos
%%     ~~Describe \code{asp} here~~
}
  \item{bg}{Plot parameter, colour pattern used to fill the points
%%     ~~Describe \code{bg} here~~
}
  \item{cols}{The colour of background
%%     ~~Describe \code{cols} here~~
}
  \item{fill}{Fill the points, default is True.
%%     ~~Describe \code{fill} here~~
}
  \item{border}{The border of map
%%     ~~Describe \code{border} here~~
}
  \item{wrap}{The range of coordinates
%%     ~~Describe \code{wrap} here~~
}
  \item{col}{The colour of points
%%     ~~Describe \code{col} here~~
}
  \item{size}{ The size of the diversity in a location.
%%     ~~Describe \code{cex} here~~
}
  \item{pch}{The plot parameter, the indicator of the point.
%%     ~~Describe \code{pch} here~~
}
  \item{\dots}{Arguments passing to regos plot
%%     ~~Describe \code{\dots} here~~
}
}
\details{This function plot geographic map using "rgeos", a class for representing polygons composed of multiple contours, some of which may be holes.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A diversity map in a world map
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

Wickham, H. (2011). ggplot2. Wiley Interdisciplinary Reviews: Computational Statistics, 3(2), 180-185.

Bivand, R., & Rundel, C. (2017). rgeos: interface to geometry engine-open source (GEOS). R package version 0.3-26.

Qin, X. (2019). HierDpart: partitioning hierarchical diversity and differentiation across metrics and scales, from genes to ecosystems. R package version 0.5. 0 https://cran.r-project.org/package=HierDpart.

%% ~put references to the literature/web site here ~
}
\author{qinxinghu@gmail.com
%%  ~~who you are~~
}

\examples{
data(Dprofile)
rDivgeomap(Dprofile$Longitude,Dprofile$Latitude,size=normalize(Dprofile$Dq1)*10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Biogeographic map}% use one of  RShowDoc("KEYWORDS")

