#### function for formatting the hierarchical D profile data

HierDstr=function(x){
  Hier=x$Hier
  nreg=x$nreg
#  r=x$r
#  n=x$npops
#colMeans(x$Dpop)
colMedians=function(x,na.rm){
  colmedval=apply(x, 2, function (x,...) {median(x,na.rm = na.rm,...)})
  return(colmedval)
}

MedianD=list(MDpop=colMedians(x$Dpop,na.rm = FALSE),MDreg=colMedians(x$Dreg,na.rm = FALSE),MDeco=colMedians(x$Deco,na.rm = FALSE))

#HierStr=as.data.frame(matrix(data = 0, ncol = 2, nrow = nreg+npops))

col2=rbind(as.matrix(unique(Hier[,2])),as.matrix(unique(Hier[,3])))
col1=rbind(as.matrix(Hier[,1][1:nreg]),as.matrix(Hier[,2]))
HierStr=as.data.frame(cbind(col1,col2))
colnames(HierStr)=c("from","to")

Dv1=as.data.frame(rbind(as.matrix("ecosystem"), as.matrix(unique(Hier[,2])),as.matrix(Hier[,3])))
Dv2=as.data.frame(rbind(as.matrix(MedianD$MDeco),as.matrix(MedianD$MDreg),as.matrix(MedianD$MDpop)))
Dv3=abbreviate(Dv1[,1],method = c("both.sides"))
HierDval=as.data.frame(cbind(Dv1,Dv2,Dv3))
colnames(HierDval)=c("names","size","Shortnames")
return(list(HierD=MedianD,HierStr=HierStr,HierDval=HierDval))

}


### example
#f <- system.file('extdata',package='HierDpart')
#infile <- file.path(f, "Island.gen")
#HierDisland=HierDq(infile,q=2,ncode=3,nreg = 4,r=c(7,4,2,3))
#hie_mapdata=HierDstr(HierDisland)

#HierDisland=HierDq(infile,q=2,ncode=3,nreg = 4,r=c(7,4,2,3))
#hie_mapdata=HierDstr(HierDisland)


HierDstrp=function(x){
  Hier=x$Hier
  nreg=x$nreg
  pop=x$pop
  #  r=x$r
  #  n=x$npops
  #colMeans(x$Dpop)
  colMedians=function(x,na.rm){
    colmedval=apply(x, 2, function (x,...) {median(x,na.rm = na.rm,...)})
    return(colmedval)
  }
  
  MedianD=list(MDpop=colMedians(x$Dpop,na.rm = FALSE),MDreg=colMedians(x$Dreg,na.rm = FALSE),MDeco=colMedians(x$Deco,na.rm = FALSE))
  
  #HierStr=as.data.frame(matrix(data = 0, ncol = 2, nrow = nreg+npops))
  requireNamespace("dplyr")
  Hier1=Hier %>% 
    distinct(pop, .keep_all = T)
  col2=rbind(as.matrix(unique(Hier1[,2])),as.matrix(unique(Hier1[,3])))
  col1=rbind(as.matrix(Hier1[,1][1:nreg]),as.matrix((Hier1[,2])))
  HierStr=as.data.frame(cbind(col1,col2))
  colnames(HierStr)=c("from","to")
  
  Dv1=as.data.frame(rbind(as.matrix("eco 1"), as.matrix(unique(Hier[,2])),as.matrix(unique(Hier[,3]))))
  Dv2=as.data.frame(rbind(as.matrix(MedianD$MDeco),as.matrix(MedianD$MDreg),as.matrix(MedianD$MDpop)))
  Dv3=abbreviate(Dv1[,1],method = c("both.sides"))
  HierDval=as.data.frame(cbind(Dv1,Dv2,Dv3))
  colnames(HierDval)=c("names","size","Shortnames")
  return(list(HierD=MedianD,HierStr=HierStr,HierDval=HierDval))
  
}

##example
#
#a=HierDstrp(HiereHGDPq0)
#HierDplot(a,layout = 'circlepack')+ scale_fill_distiller(palette = "RdPu")




