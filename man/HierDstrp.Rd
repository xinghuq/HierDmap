\name{HierDstrp}
\alias{HierDstrp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Formating the hierarchial diversity data structure for "HierDgenlind" function" used for plotting
%%  ~~function to do ... ~~
}
\description{This function is expecially used for self-defined hierarchical structure. The result is used to feed into "HierDplot"" for visualizing the hierarchy of the diversity.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HierDstrp(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Object produced from "HierDgenlind".
%%     ~~Describe \code{x} here~~
}
}
\details{
Formatting the data produced from "HierDgenlind" and then used for plotting the hierarchy of diversity.
%%  ~~ If necessary, more details than the description above ~~
}
\value{

A data frame used for "HierDplot".
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{qinxinghu@gmail.com
%%  ~~who you are~~
}

\examples{
#not run
#a=HierDstrp(HiereHGDPq0)
#HierDplot(a,layout = 'circlepack')+ scale_fill_distiller(palette = "RdPu")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{HierDstr}% use one of  RShowDoc("KEYWORDS")

