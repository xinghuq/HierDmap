\name{dgDivgeomap}
\alias{dgDivgeomap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diversity-density geographic map.
%%  ~~function to do ... ~~
}
\description{The density of biodiversity in geographic map.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dgDivgeomap(data, x = Longitude, y = Latitude, lon = c(-160, 160), lat = c(-60, 70), mapsource = "osm", mapcolor = "color", maptype = "satellite", point_color = "red", pointalpha = 0.2, pointsize = normalize(HiereHGDPq1$HierD$MDpop) * 10, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A data frame contains the longitute, latitude, and diversities of each location.
%%     ~~Describe \code{data} here~~
}
  \item{x}{The longitude of the sampling location
%%     ~~Describe \code{x} here~~
}
  \item{y}{The latitude of the sampling location
%%     ~~Describe \code{y} here~~
}
  \item{lon}{ The range of longitude in a map
%%     ~~Describe \code{lon} here~~
}
  \item{lat}{The range of latitude in a map
%%     ~~Describe \code{lat} here~~
}
  \item{mapsource}{The map sources,Google Maps ("google"), OpenStreetMap ("osm"), Stamen Maps ("stamen")
%%     ~~Describe \code{mapsource} here~~
}
  \item{mapcolor}{color ("color") or black-and-white ("bw")
%%     ~~Describe \code{mapcolor} here~~
}
  \item{maptype}{character string providing map theme. options available are "terrain", "terrain-background", "satellite", "roadmap", and "hybrid" (google maps), "terrain", "watercolor", and "toner" (stamen maps), or a positive integer for cloudmade maps (see ?get_cloudmademap)
%%     ~~Describe \code{maptype} here~~
}
  \item{point_color}{The colour of diversity point
%%     ~~Describe \code{point_color} here~~
}
  \item{pointalpha}{The value used to plot the points.
%%     ~~Describe \code{pointalpha} here~~
}
  \item{pointsize}{The values used to indicate the relative size the points(diversity).
%%     ~~Describe \code{pointsize} here~~
}
  \item{\dots}{passing to ggplot2.
%%     ~~Describe \code{\dots} here~~
}
}
\details{This function plots the diversity and density of the diversity map using "ggmap"and "ggplot2".
%%  ~~ If necessary, more details than the description above ~~
}
\value{A density and diversity geographic map.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

Wickham, H. (2011). ggplot2. Wiley Interdisciplinary Reviews: Computational Statistics, 3(2), 180-185.

Kahle, D., & Wickham, H. (2013). ggmap: Spatial Visualization with ggplot2. The R journal, 5(1), 144-161.


Qin, X. (2019). HierDpart: partitioning hierarchical diversity and differentiation across metrics and scales, from genes to ecosystems. R package version 0.5. 0 https://cran. r-project. org/package= HierDpart.


%% ~put references to the literature/web site here ~
}
\author{qinxinghu@gmail.com
%%  ~~who you are~~
}

\examples{
data(Dprofile)
dgDivgeomap(Dprofile,x=Dprofile$Longitude, y=Dprofile$Latitude,lon = c(-160, 160), lat = c(-60, 70),  mapsource = "osm", mapcolor = "color", maptype="satellite",
                     point_color="red", pointalpha = .2, pointsize=normalize(Dprofile$Dq1)*10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
