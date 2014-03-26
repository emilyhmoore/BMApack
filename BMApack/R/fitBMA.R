#' fitBMA Function
#'
#' Runs regression on all possible combos of covariates and returns coefs, R2, and BMA stats
#'
#' @param x: A numeric matrix of covariates
#' @param y: A numeric vector of the same length as the number of rows in x.
#' @param g: A value for g. 
#' @param parallel: runs in parallel if TRUE
#'
#' @return An bma class object with following slots: 
#'  \item{combo.coef}{A list of coefficients from each regression}
#'  \item{combo.fit}{Vector of R-squared Values} 
#'  \item{bmk}{Vector of posterior probability odds}
#'  \item{exp.vals}{A vector of expected coefficient values}
#'  \item{coefprobs}{A vector of probabilities that the coefficient is non-zero}
#' @author Jacob Montgomery, Dino Hadzic, Jae Hee Jung, and Emily Moore
#' @examples
#' 
#' x1<-rnorm(500)
#' x2<-rnorm(500,3,15)
#' dep<-(x1+2*x2)+rnorm(500,4,100)
#' covars<-cbind(x1,x2) 
#' fitBMA(x=covars, y=dep, parallel=FALSE)
#' @rdname fitBMA
#' @export

#bigx<-matrix(rnorm(10000), ncol=20)
#colnames(bigx)<-paste("Var", 1:ncol(bigx))
#x<-bigx[,1:3]
#y<-3*x[,1]+2*x[,2]+rnorm(500)

setGeneric(name="fitBMA",
           def=function(x, y, g=3, parallel=TRUE,core=10,...)
           {standardGeneric("fitBMA")}
)


setMethod(f="fitBMA",
          definition=function(x, y, g=3, parallel=TRUE,
                              core=10){
  library(plyr)
  if(parallel==TRUE){
  library(foreach)
  library(multicore)
  library(doMC)
  registerDoMC(cores=core) ##Will need for later for parallel stuff
  }
  ##Error thrown if non-unque column names.
  if(length(unique(colnames(x)))<ncol(x)){stop("Must have unique names for each column")}
  
  set <- llply(1:ncol(x),function(X){combn(ncol(x),X,simplify=F)}, .parallel=parallel)
  
  set<-unlist(set, recursive=F)

  ##This for() loop creates a list item. Each item is a regression based on 
  ##the covariate matrix. The powerset deal allows an index of possible values
  run.regs<-function(i, .parallel=parallel){
    list1<-list(NULL) ##empty list
    list1<-list(lm(scale(y)~-1+scale(x[,set[[i]]]))) ##all combinations
    return(list1)
  }

  #Get the list of regressions
  list1<-llply(1:length(set), run.regs, .parallel=parallel)
  ##Get rid of the outermost list, so it's one list per regression
  list1<-unlist(list1, recursive=F)
  
  ##This gets the r.squared values and puts them in a list
  fits<-llply(list1, function(x){summary(x)[['r.squared']]}, .parallel=parallel)
  ##Since lapply makes a list, we unlist to make a vector
  fits<-unlist(fits)

  ##coef.fun will later be used to extract the coefficients from the analysis. 
  ##This function also uses the setNames function in order to identify the 
  #variable for each coefficient.
  coef.fun <- function(i, .parallel=parallel){
    coefs <- list()
    coefs <- setNames(coef(list1[[i]]), colnames(x)[set[[i]]])
    return(coefs)  
  }
  
  #Extracts the coefficients.
  coefs<-llply(1:length(set), coef.fun, .parallel=parallel)   

  ##Create a matrix of the values needed to calculate b|mk:m0| for each model
  gs<-rep(g, length(set)) ##make a vector of the g value
  ns<-rep(length(y), length(set)) ##make a vector of the n value
  
  ##pks.fun will later be used in llply function to create vector pks that stores
  ##length of every element of list "set."
  pks.fun <- function(i, .parallel=parallel){ 
    pks <- list()
    pks <- length(set[[i]])
    return(pks)
  }
  pks <- llply(1:length(set), pks.fun, .parallel=parallel) ##running llply over "set."
  pks <- unlist(pks) ##unlisting pks
  pks <- as.numeric(pks) ##assigning numeric class to pks
  r2s<-fits##r2 values
  
  values<-cbind(gs, ns, pks, r2s)##make matrix of these
  
  ##function is intended to be used to aaply over the rows of the matrix above
  bmk<-function(x){
    bmk<-((1+x[1])^((x[2]-x[3])/2))*((1+x[1]*(1-x[4]))^(-(x[2]-1)/2))
    names(bmk)<-c("bmk.val")
    return(bmk)
  }
  
  ##vector of bmk values for each model
  bmk.vec<-aaply(.data=values,.margins=1,.fun=bmk, .parallel=parallel)
  
  ##Sum of bmk for each model
  sum.bmk<-sum(bmk.vec)
  
  ##Fill in odds for bmk
  ##odds.fun will later be used to calculate odds for bmk.
  odds.fun <- function(i, .parallel=parallel){ 
    odds.bmk <- list()
    odds.bmk <- bmk.vec[i]/sum.bmk
    return(odds.bmk)
  }
  odds.bmk <- llply(1:length(bmk.vec), odds.fun, .parallel=parallel) ##calculating odds, and storing as odds.bmk
  odds.bmk <- unlist(odds.bmk) ##unlisting odds.bmk
  odds.bmk <- as.numeric(odds.bmk) ##assigning class numeric to odds.bmk
  
  ##Function which returns x in y since I couldn't find what I was looking for
  xiny<-function(y,x){x %in% y}
  
  ##Function which determines which sets in the set of models include each variable
  applier<-function(i){
    index2<-laply(set, xiny, x=i, .parallel=parallel) ##is it included in this one? True/false
    index2<-which(index2==TRUE) ##Which ones are true?
    return(index2)
  }

  ##Function which returns the odds of each model including the relevant variable
  theodds<-function(i){
    index3<-laply(1:ncol(x), applier, .parallel=parallel)
    odds.bmk[index3[i,]]
  }

  ##Get the probability values of the mods in question and put them in a matrix
  themods<-laply(1:ncol(x), theodds, .parallel=parallel)
  
  ##Get the relevant coefs
  coefnamer<-function(i){
    coefvec<-unlist(coefs) ##turn list of coefs into a vector.
    coefname<-which(names(coefvec)==colnames(x)[i]) ##Which coefs have a matching name
    coef1<-coefvec[coefname]
    return(coef1)
  }

##Apply coefnamer function over the columns of x
  thecoefs<-llply(1:ncol(x), coefnamer, .parallel=parallel) 
  thecoefs<-unlist(thecoefs)
  thecoefs<-matrix(thecoefs, nrow=ncol(x), byrow=TRUE)
  rownames(thecoefs)<-colnames(x)

  ptimese<-themods*thecoefs ##Utilize R's practice of element-wise multiplication of matrices

  exp.val1<-aaply(ptimese, 1, sum, .parallel=parallel) ##Sum across the rows

  exp.val<-exp.val1*(g/(g+1)) ##Multiply by g/g+1
  names(exp.val)<-colnames(x)
  
  coefprob<-aaply(themods, 1, sum, .parallel=parallel)
  names(coefprob)<-colnames(x)
  
  return(new("bma", x=x, y=y, thecoefs=thecoefs, combo.coef=coefs, 
             combo.fit=fits,bmk=odds.bmk, exp.vals=exp.val, coefprobs=coefprob))
          }#close function definition
) ##Close method

#thing<-fitBMA(bigx[1:100,1:5], y[1:100], parallel=FALSE)



