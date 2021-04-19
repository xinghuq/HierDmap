#### Function hieraral diversity genind


HierDgenind=function (x,pop_region,pop,q) {

  requireNamespace("dplyr")
  library('entropart')
  CheckentropartArguments=FALSE
  defaultW <- getOption("warn") 
  options(warn = -1)
  
  
  nreg=length(unique(pop_region))
  npops = length(unique(pop))
  nloci = length(levels(x$loc.fac))
  nind=length(x$pop)
  ecosystem=rep("eco 1",nind)
 
 # rsample=as.data.frame(table(pop_region))
  
 
  rgenfiles=x
  rgenfiles$pop=pop_region
  genf_overall=x
  genf_overall$pop=as.factor(ecosystem)
  
  ### separate by locus
  gind_pop=adegenet::seploc(x)
  gind_reg=adegenet::seploc(rgenfiles)
  gind_eco=adegenet::seploc(genf_overall)
  ## convert to hierfstat format
  hierfst_pop=lapply(gind_pop,function (x) hierfstat::genind2hierfstat(x))
  hierfst_reg=lapply(gind_reg,function (x) hierfstat::genind2hierfstat(x))
  hierfst_eco=lapply(gind_eco,function (x) hierfstat::genind2hierfstat(x))
  ### get allele ccount per locus
  allecount_pop=lapply(hierfst_pop,function(x) hierfstat::allele.count(x))
  allecount_reg=lapply(hierfst_reg,function(x) hierfstat::allele.count(x))
  allecount_eco=lapply(hierfst_eco,function(x) hierfstat::allele.count(x))
  ## get diversity profiles 
 
  Dpop = as.data.frame(matrix(data = 0, ncol = npops, nrow = nloci))
  for (i in 1:nloci) {
    for (j in 1:npops) {
      Dpop[i, j] = entropart::Diversity(allecount_pop[[i]]$`data[, -1]`[, j], q, CheckArguments = FALSE)
    }
  }
  rownames(Dpop) = unique(x$loc.fac)
  colnames(Dpop) = unique(pop)
  
 CheckArguments=FALSE 
  Dreg = as.data.frame(matrix(data = 0, ncol = nreg, nrow = nloci))
  for (i in 1:nloci) {
    for (j in 1:nreg) {
      Dreg[i, j] = entropart::Diversity(allecount_reg[[i]]$`data[, -1]`[, j], q, CheckArguments = FALSE)
    }
  }
  rownames(Dreg) = unique(x$loc.fac)
  colnames(Dreg) = unique(pop_region)
  
  
  Deco = as.data.frame(matrix(data = 0, ncol = length(unique(ecosystem)), nrow = nloci))
  
  for (i in 1:nloci) {
    for (j in 1:length(unique(ecosystem))) {
      Deco[i, j] = entropart::Diversity(allecount_eco[[i]]$`data[, -1]`[, j], q, CheckArguments = FALSE)
    }
  }
  rownames(Deco) = unique(x$loc.fac)
  colnames(Deco) = unique(ecosystem)
  colMedians=function(x,na.rm){
    colmedval=apply(x, 2, function (x,...) {median(x,na.rm = na.rm,...)})
    return(colmedval)
  }
  
  MedianD=list(MDpop=colMedians(Dpop,na.rm = FALSE),MDreg=colMedians(Dreg,na.rm = FALSE),MDeco=colMedians(Deco,na.rm = FALSE))
  Hier=data.frame(ecosystem=ecosystem,pop_region=pop_region,pop=pop)
  return(list(HierD=MedianD,Hier=Hier,Dpop=Dpop,Dreg=Dreg,Deco=Deco,pop_region=pop_region,pop=pop,nreg=nreg,n=npops))
  
}


#library(adegenet)
#data(eHGDP)
# example genepop file
### formating data, restructing the hierarchical data structure 
#levels(eHGDP$pop)=eHGDP$other$popInfo$Population
###struct a table with individual region/pop labels
#popstr= eHGDP$other$popInfo[match(eHGDP$pop,eHGDP$other$popInfo$Population),]
#popstr$Population=factor(popstr$Population,levels=unique(popstr$Population))
#popstr$Region=factor(popstr$Region,levels=unique(popstr$Region))
#library(poppr)
#eHGDP1=missingno(eHGDP, type = "mean",  quiet = FALSE, freq = FALSE)

#eHGDP$tab=(tab(eHGDP, freq = FALSE, NA.method = "mean"))

#HiereHGDPq0=HierDgenind(eHGDP,q=0,pop_region = popstr$Region,pop = popstr$Population)
#HiereHGDPq1=HierDgenind(eHGDP,q=1,pop_region = popstr$Region,pop = popstr$Population)
#HiereHGDPq2=HierDgenind(eHGDP,q=2,pop_region = popstr$Region,pop = popstr$Population)


