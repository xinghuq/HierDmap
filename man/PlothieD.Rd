\name{PlothieD}
\alias{PlothieD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting an interactive diversity map in a hierarchy 
%%  ~~function to do ... ~~
}
\description{Plot an interactive map of hierarchy of diversity, from local population to regions and ecosystem with self-defined hierarchy structure. This function plots the diversity in the hierarchy, from local population to regions and ecosystem with self-defined hierarchical structure.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlothieD(x, size = "Dq2", color_min = "hsl(56,80\%,80\%)", 
color_max = "hsl(341,30\%,40\%)", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Genetic diversity files, HierDgenind object, produced from function "HierDgenind".
%%     ~~Describe \code{x} here~~
}
  \item{size}{The size of the diversity or the value of diversity will be plotted. This can be choosen after calculated the diversity profiles using HierDgenind.
%%     ~~Describe \code{size} here~~
}
  \item{color_min}{string representing the minimum value of the color range for the circles. The string can be either a hexadecimal, RGB, or HSL color. "hsl(152, 80\%, 80\%)" is the default.
%%     ~~Describe \code{color_min} here~~
}
  \item{color_max}{string representing the maximum value of the color range for the circles. The string can be either a hexadecimal, RGB, or HSL color. "hsl(228, 30\%,40\%)" is the default.
%%     ~~Describe \code{color_max} here~~
}
  \item{\dots}{other arguments passing to circlepackeR.
%%     ~~Describe \code{\dots} here~~
}
}
\details{ Plot diversity (q= 0,1,2,...) with self-defined hierarchy. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{A interactive plot showing the size of biodiversity in a hierarchy.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Chao, A., Chiu, C. H., & Jost, L. (2014). Unifying species diversity, phylogenetic diversity, functional diversity, and related similarity and differentiation measures through Hill numbers. Annual review of ecology, evolution, and systematics, 45, 297-324.

Qin, X. (2019). HierDpart: partitioning hierarchical diversity and differentiation across metrics and scales, from genes to ecosystems. R package version 0.5.0 https://cran.r-project.org/package=HierDpart.

Marcon, E., & Herault, B. (2015). entropart: An R package to measure and partition diversity. Journal of Statistical Software, 67(1), 1-26.


Jombart, T. (2008). adegenet: a R package for the multivariate analysis of genetic markers. Bioinformatics, 24(11), 1403-1405.

Froelich, Jerome. et al.2015. circlepackeR: R htmlwidget for circle packing visualization. R Package. https://github.com/jeromefroe/circlepackeR.

%% ~put references to the literature/web site here ~
}
\author{qinxinghu@gmail.com
%%  ~~who you are~~
}

\examples{
##example
data(Dprofile)

PlothieD(Dprofile[,-1],size = "Dq1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Plot Diversity Hierarchical Structure}% use one of  RShowDoc("KEYWORDS")

