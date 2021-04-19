#### Function hieraral diversity

### used for an genepop format, ideally if you do not have already set the hierarchy

HierDq=function (x,q, nreg, r, ncode) {
  read.genepop1 <- function(file, ncode, quiet = FALSE) {
    if (toupper(adegenet::.readExt(file)) != "GEN") 
      stop("File extension .gen expected")
    if (!quiet) 
      cat("\n Converting data from a Genepop .gen file to a genind object... \n\n")
    prevcall <- match.call()
    txt <- scan(file, sep = "\n", what = "character", 
                quiet = TRUE)
    if (!quiet) 
      cat("\nFile description: ", txt[1], "\n")
    txt <- txt[-1]
    txt <- gsub("\t", " ", txt)
    NA.char <- paste(rep("0", ncode), collapse = "")
    locinfo.idx <- 1:(min(grep("POP", toupper(txt))) - 
                        1)
    locinfo <- txt[locinfo.idx]
    locinfo <- paste(locinfo, collapse = ",")
    loc.names <- unlist(strsplit(locinfo, "([,]|[\n])+"))
    loc.names <- trimws(loc.names)
    nloc <- length(loc.names)
    txt <- txt[-locinfo.idx]
    pop.idx <- grep("^([[:space:]]*)POP([[:space:]]*)$", 
                    toupper(txt))
    npop <- length(pop.idx)
    nocomma <- which(!(1:length(txt)) %in% grep(",", 
                                                txt))
    splited <- nocomma[which(!nocomma %in% pop.idx)]
    if (length(splited) > 0) {
      for (i in sort(splited, decreasing = TRUE)) {
        txt[i - 1] <- paste(txt[i - 1], txt[i], sep = " ")
      }
      txt <- txt[-splited]
    }
    pop.idx <- grep("^([[:space:]]*)POP([[:space:]]*)$", 
                    toupper(txt))
    txt[length(txt) + 1] <- "POP"
    nind.bypop <- diff(grep("^([[:space:]]*)POP([[:space:]]*)$", 
                            toupper(txt))) - 1
    pop <- factor(rep(1:npop, nind.bypop))
    txt <- txt[-c(pop.idx, length(txt))]
    temp <- sapply(1:length(txt), function(i) strsplit(txt[i], 
                                                       ","))
    ind.names <- vapply(temp, function(e) e[1], character(1))
    ind.names <- trimws(ind.names)
    vec.genot <- vapply(temp, function(e) e[2], character(1))
    vec.genot <- trimws(vec.genot)
    X <- matrix(unlist(strsplit(vec.genot, "[[:space:]]+")), 
                ncol = nloc, byrow = TRUE)
    if (any(duplicated(ind.names))) {
      rownames(X) <- adegenet::.genlab("", nrow(X))
    }
    else {
      rownames(X) <- ind.names
    }
    colnames(X) <- loc.names
    pop.names.idx <- cumsum(table(pop))
    pop.names <- ind.names[pop.names.idx]
    levels(pop) <- pop.names
    if (!all(unique(nchar(X)) == (ncode * 2))) 
      stop(paste("some alleles are not encoded with", 
                 ncode, "characters\nCheck 'ncode' argument"))
    res <- adegenet::df2genind(X = X, pop = as.character(pop), ploidy = 2, 
                     ncode = ncode, NA.char = NA.char)
    res@call <- prevcall
    if (!quiet) 
      cat("\n...done.\n\n")
    return(res)
  }
  genfiles = read.genepop1(x, ncode=ncode, quiet = TRUE)
  
  requireNamespace("dplyr")
  nreg=nreg
  npops = length(levels(genfiles$pop))
  nloci = length(levels(genfiles$loc.fac))
  nind=length(genfiles$pop)
  ecosystem=rep("eco 1",nind)
  sampsize = summary(genfiles$pop)
  requireNamespace('entropart')
  CheckentropartArguments=FALSE
  defaultW <- getOption("warn") 
  options(warn = -1)
  Hier=as.data.frame(t(HierDpart::Str(nreg, r,n=npops)))
  ## sample size
  if (length(r) != nreg)
    stop("Number of regions should be equal to the number defined in the level")  ## number of pops per region
  if (sum(r) != npops)
    stop("Number of pops should be equal to the number defined in level")
  rsample = list()
  requireNamespace("utils")
  for (i in 1:nreg) {
    rsample[[i]] = sum(sampsize[(sum(head(r, i - 1)) + 1):(sum(head(r, i)))])
  }
  rsample = as.data.frame(rsample)
  rsample = as.numeric(unlist(rsample))
  
  popr=list()
  for (i in 1:nreg) {
    popr[[i]] = list()
    popr[[i]] = as.factor(rep(paste("popr", i), times = sum(sampsize[(sum(head(r, i - 1)) + 1):(sum(head(r, i)))])))  ### be aware that times depend on the sample size and str on your data
    # rsample[[i]] = sum(sampsize[(sum(head(r, i - 1)) + 1):(sum(head(r, i)))])
  }
  
  pop_region = unlist(popr)
  rgenfiles=genfiles
  rgenfiles$pop=pop_region
  genf_overall=genfiles
  genf_overall$pop=as.factor(ecosystem)
  
  ### geting allele fre at regional level
  # genreg=genind2genpop(rgenfiles, pop = NULL, quiet = TRUE, process.other = FALSE,other.action = mean)
  #geco=genind2genpop(genf_overall, pop = NULL, quiet = TRUE, process.other = FALSE,other.action = mean)
  
  ### separate by locus
  gind_pop=adegenet::seploc(genfiles)
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
 # library(entropart)
#  D_pop=lapply(allecount_pop,function(x) apply(x$`data[, -1]`,1,function(x) {entropart::Diversity(x, q=1, CheckArguments = FALSE)}))
  CheckArguments=FALSE
  Dpop = as.data.frame(matrix(data = 0, ncol = npops, nrow = nloci))
  for (i in 1:nloci) {
    for (j in 1:npops) {
      Dpop[i, j] = entropart::Diversity(allecount_pop[[i]]$`data[, -1]`[, j], q, CheckArguments = FALSE)
    }
  }
  rownames(Dpop) = unique(genfiles$loc.fac)
  colnames(Dpop) = unique(genfiles$pop)
  

  Dreg = as.data.frame(matrix(data = 0, ncol = nreg, nrow = nloci))
  for (i in 1:nloci) {
    for (j in 1:nreg) {
      Dreg[i, j] = entropart::Diversity(allecount_reg[[i]]$`data[, -1]`[, j], q, CheckArguments = FALSE)
    }
  }
  rownames(Dreg) = unique(genfiles$loc.fac)
  colnames(Dreg) = unique(Hier$V2)
  
  
  Deco = as.data.frame(matrix(data = 0, ncol = length(unique(ecosystem)), nrow = nloci))
  
  for (i in 1:nloci) {
    for (j in 1:length(unique(ecosystem))) {
      Deco[i, j] = entropart::Diversity(allecount_eco[[i]]$`data[, -1]`[, j], q, CheckArguments = FALSE)
    }
  }
  rownames(Deco) = unique(genfiles$loc.fac)
  colnames(Deco) = unique(Hier$V1)
  
  return(list(Dpop=Dpop,Dreg=Dreg,Deco=Deco,Hier=Hier,nreg=nreg,r=r,n=npops))
  
}
  

# example genepop file
#f <- system.file('extdata',package='HierDpart')
#infile <- file.path(f, "Island.gen")
#file =  diveRsity::readGenepop(infile, gp=2, bootstrap = FALSE)
#HierDisland=HierDq(infile,q=2,ncode=3,nreg = 4,r=c(7,4,2,3))

