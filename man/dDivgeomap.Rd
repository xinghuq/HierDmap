\name{dDivgeomap}
\alias{dDivgeomap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot diversity plot in a biodiversity geographic map.
%%  ~~function to do ... ~~
}
\description{The density of biodiversity in geographic map.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dDivgeomap(data, x = Longitude, y = Latitude, label = Population, border_colour = NA, border_fill = "antiquewhite", density_alpha = I(0.2), density_fill, density_size = 1, bins = 5, geom = "polygon", point_color = "red", pointalpha = 0.2, pointsize = normalize(HiereHGDPq1$HierD$MDpop) * 10, ...)
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
  \item{label}{ The locations(sites or population) that will be ploted 
%%     ~~Describe \code{label} here~~
}
  \item{border_colour}{ The colour of the border of geographic map 
%%     ~~Describe \code{border_colour} here~~
}
  \item{border_fill}{Geographic map border 
%%     ~~Describe \code{border_fill} here~~
}
  \item{density_alpha}{The density plot argument passing to stat_density_2d
%%     ~~Describe \code{density_alpha} here~~
}
  \item{density_fill}{The density plot argument passing to stat_density_2d
%%     ~~Describe \code{density_fill} here~~
}
  \item{density_size}{The density plot argument passing to stat_density_2d
%%     ~~Describe \code{density_size} here~~
}
  \item{bins}{The density plot argument passing to stat_density_2d
%%     ~~Describe \code{bins} here~~
}
  \item{geom}{The density plot argument passing to stat_density_2d
%%     ~~Describe \code{geom} here~~
}
  \item{point_color}{The diversity point color
%%     ~~Describe \code{point_color} here~~
}
  \item{pointalpha}{The diversity ponit parameter passing to geom_point()
%%     ~~Describe \code{pointalpha} here~~
}
  \item{pointsize}{The point size used to indicate the (relative) size of diversity values
%%     ~~Describe \code{pointsize} here~~
}
  \item{\dots}{other arguments passing to ggplot.
%%     ~~Describe \code{\dots} here~~
}
}
\details{There are four ways of plotting geographic map in this package. "dDiversity" uses ggplot2.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A density and diversity map in a world map
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

Wickham, H. (2011). ggplot2. Wiley Interdisciplinary Reviews: Computational Statistics, 3(2), 180-185.

Qin, X. (2019). HierDpart: partitioning hierarchical diversity and differentiation across metrics and scales, from genes to ecosystems. R package version 0.5. 0 https://cran. r-project. org/package= HierDpart.
%% ~put references to the literature/web site here ~
}
\author{qinxinghu@gmail.com
%%  ~~who you are~~
}

\examples{
data(Dprofile)
  dDivgeomap(Dprofile,x=Dprofile$Longitude, y=Dprofile$Latitude,label=Dprofile$Population,border_colour=NA, border_fill="antiquewhite",density_fill =Dprofile$Dq1 , density_alpha = I(.2),density_size = 1, bins = 5, geom = "polygon"
,point_color="red", pointalpha = .2, pointsize=normalize(Dprofile$Dq1)*10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Biogeographic map}% use one of  RShowDoc("KEYWORDS")

