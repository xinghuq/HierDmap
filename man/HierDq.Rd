\name{HierDq}
\alias{HierDq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculating hierarchical diversity based on Hill numbers
%%  ~~function to do ... ~~
}
\description{Calculating hierarchical diversity based on Hill numbers.When q=0, it is richness, when q=1, it is diversity of Shannon (exponential of shannon entropy), when q=2, it is diversity of Simpson. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HierDq(x, q, nreg, r, ncode)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Genotype file, current version accepts genepop format.
%%     ~~Describe \code{x} here~~
}
  \item{q}{The order of Hill numbers
%%     ~~Describe \code{q} here~~
}
  \item{nreg}{ number of regions
%%     ~~Describe \code{nreg} here~~
}
  \item{r}{The population arrangement or the hierarchy in the region
%%     ~~Describe \code{r} here~~
}
  \item{ncode}{ The genotype coding type
%%     ~~Describe \code{ncode} here~~
}
}
\details{The function calculates diversity at ecosystem level, regional level, and local population level based on HierDpart package (Qin, X.2019).
%%  ~~ If necessary, more details than the description above ~~
}
\value{Diversity at ecosystem level, regional level, and local population level.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Qin, X. (2019). HierDpart: partitioning hierarchical diversity and differentiation across metrics and scales, from genes to ecosystems. R package version 0.5. 0 https://cran. r-project. org/package= HierDpart.
%% ~put references to the literature/web site here ~
}
\author{qinxinghu@gmail.com
%%  ~~who you are~~
}

\examples{
f <- system.file('extdata',package='HierDpart')
infile <- file.path(f, "Island.gen")
file =  diveRsity::readGenepop(infile, gp=2, bootstrap = FALSE)
HierDisland=HierDq(infile,q=2,ncode=3,nreg = 4,r=c(7,4,2,3))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{HierDq }% use one of  RShowDoc("KEYWORDS")

