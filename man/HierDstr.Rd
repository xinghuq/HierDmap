\name{HierDstr}
\alias{HierDstr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Formating the hierarchial diversity data structure used for plotting
%%  ~~function to do ... ~~
}
\description{This function formats the hierarchial diversity data used for plotting the diversity in a hierarchy.The function takes the values produced from "HierDq", and then formats it into igraph hierarchy.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}

\arguments{
  \item{x}{The diversity data from HierDq 
%%     ~~Describe \code{x} here~~
}
}
\details{ The plot has two forms, if the geographic coordinates are provided, it will plot the geo-map using "HierGeoplot", if not, it will plot the hierarchy map.
%%  ~~ If necessary, more details than the description above ~~
}
\value{The hierarchical diversity formated in a igraph format.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Qin, X. (2019). HierDpart: partitioning hierarchical diversity and differentiation across metrics and scales, from genes to ecosystems. R package version 0.5. 0 https://cran.r-project.org/package=HierDpart.
%% ~put references to the literature/web site here ~
}
\author{qinxinghu@gmail.com
%%  ~~who you are~~
}
\examples{

### load data
f <- system.file('extdata',package='HierDpart')
infile <- file.path(f, "Island.gen")
### calculate diversity profile
HierDisland=HierDq(infile,q=2,ncode=3,nreg = 4,r=c(7,4,2,3))
### structure the diversity values
hie_mapdata=HierDstr(HierDisland)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ HierDstr}% use one of  RShowDoc("KEYWORDS")

