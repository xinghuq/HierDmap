\name{HierDplot}
\alias{HierDplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting hierarchical diversity
%%  ~~function to do ... ~~
}
\description{Plotting hierarchical diversity map
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}

\arguments{
  \item{x}{The diversity structure data produced from "HierDstr"
%%     ~~Describe \code{x} here~~
}
  \item{layout}{The plot layout, either a valid string, a function, a matrix, or a data.frame, see "ggraph".
%%     ~~Describe \code{layout} here~~
}
  \item{\dots}{Arguments passing to igraph
%%     ~~Describe \code{\dots} here~~
}
}
\details{Plot the hierarchical diversity
%%  ~~ If necessary, more details than the description above ~~
}
\value{ A HierDmap plot
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Pedersen, T. L., Pedersen, M. T. L., LazyData, T. R. U. E., Rcpp, I., & Rcpp, L. (2017). Package ‘ggraph’. Retrieved January, 1, 2018.
%% ~put references to the literature/web site here ~
}
\author{qinxinghu@gmail.com
%%  ~~who you are~~
}

\examples{

### load data
f <- system.file('extdata',package='HierDpart')
infile <- file.path(f, "Island.gen")
### calculate diversity values
HierDisland=HierDq(infile,q=2,ncode=3,nreg = 4,r=c(7,4,2,3))
hie_mapdata=HierDstr(HierDisland)
HierDplot(hie_mapdata,layout = 'circlepack')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{HierDplot}% use one of  RShowDoc("KEYWORDS")

