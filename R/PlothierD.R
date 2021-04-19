### interactive plots, this is a good example can be used for visualization of hier diversity for world populations, using HierGenomeScan

# Libraries
#if (!requireNamespace("devtools", quietly = TRUE))
#  install.packages("devtools")
#devtools::install_github("jeromefroe/circlepackeR")

#if (!requireNamespace("circlepackeR", quietly = TRUE))
#  install.packages("circlepackeR")
#devtools::install_github("jeromefroe/circlepackeR")

#library(tidyverse)
#library(hrbrthemes)


#devtools::install_github("jeromefroe/circlepackeR")
#library(circlepackeR)  
#library(data.tree)


### The input format: hierstr + HierD

PlothieD=function(x, size = "Dq2",color_min = "hsl(56,80%,80%)", color_max = "hsl(341,30%,40%)",...){
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  # Libraries
  if (!requireNamespace("devtools", quietly = TRUE))
    utils::install.packages("devtools")
  requireNamespace("devtools")
  devtools::install_github("jeromefroe/circlepackeR")
  
  if (!requireNamespace("circlepackeR", quietly = TRUE))
    install.packages("circlepackeR")
  devtools::install_github("jeromefroe/circlepackeR")
  x$Dq0=normalize(x$Dq0)*100
  x$Dq1=normalize(x$Dq1)*100
  x$Dq2=normalize(x$Dq2)*100
  x$pathString=paste("world", x[,1], x[,2], x[,3], sep = "/")
  popdiv=data.tree::as.Node(x,...)
  # You can custom the minimum and maximum value of the color range.
  circlepackeR::circlepackeR(popdiv, size = size, color_min = color_min, color_max =color_max,...)
}



##example
#diversitypf=read.csv(file = "eHGDP_info.csv",h=T)
#diversitypfall=cbind(diversitypf,HiereHGDPq0$HierD$MDpop,HiereHGDPq1$HierD$MDpop,HiereHGDPq2$HierD$MDpop)
#colnames(diversitypfall)[8:10]=c("Dq0","Dq1","Dq2")

#plothieD(diversitypfall[,-1],size = "Dq1")





