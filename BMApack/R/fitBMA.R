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
           def=function(x, y, g=3, parallel=TRUE,core=100,...)
           {standardGeneric("fitBMA")}
)


setMethod(f="fitBMA",
          definition=function(x, y, g=3, parallel=TRUE){
  
  ##This function runs the regressions for each combination
  run.regs<-function(i, .parallel=parallel){
    list1<-list(NULL) ##empty list
    list1<-list(lm(scale(y)~-1+scale(x[,set[[i]]]))) ##all combinations
    return(list1)
  }
  
  ##coef.fun will later be used to extract the coefficients from the analysis. 
  ##This function also uses the setNames function in order to identify the 
  #variable for each coefficient.
  coef.fun <- function(i, .parallel=parallel){
    coefs <- list()
    coefs <- setNames(coef(list1[[i]]), colnames(x)[set[[i]]])
    return(coefs)  
  }
  
  ##Error thrown if non-unque column names.
  if(length(unique(colnames(x)))<ncol(x)){stop("Must have unique names for each column")}
  
  ##making the set
  set <- llply(1:ncol(x),function(X){combn(ncol(x),X,simplify=F)}, 
               .parallel=parallel)
  
  ##making each combination a single list item
  set<-unlist(set, recursive=F)

  #Get the list of regression results
  list1<-llply(1:length(set), run.regs, .parallel=parallel)
  ##Get rid of the outermost list, so it's one list per regression
  list1<-unlist(list1, recursive=F)
  
  ##This gets the r.squared values and puts them in a list
  fits<-llply(list1, function(x){summary(x)[['r.squared']]}, .parallel=parallel)
  
  ##Since lapply makes a list, we unlist to make a vector
  fits<-unlist(fits)

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
  
  ##Get the names of the covariates whose coefficients are estimated for each model. 
  covariates.mod <- llply(coefs,names)
  
  	##Identify the covariates.
	covariates <- unique(unlist(covariates.mod))
	
	##Find the number of covariates used in each model.
	covariate.number <- sapply(coefs,length)
	
	##Create an empty list. Fill it with the coefficient estimates of each model, indicating the coefficients of covariates that the model does not compute as NA.
  mod <- vector("list",length(covariate.number))
  
  for(i in seq_along(covariate.number)){
			mod[[i]] <- unname(coefs[[i]])[match(covariates,covariates.mod[[i]])]
		}
	
	##Turn the list into a matrix.
thecoefs <- 	setNames(as.matrix(do.call(rbind,mod),stringsAsFactors=FALSE),nm=covariates)
  
  themods <- thecoefs
  
  ##Replace the coefficient estimates with the odds of each model that the covariate is included. 
  for(i in 1:length(odds.bmk)){
  	themods[i,][which(!is.na(themods[i,]))] <- odds.bmk[i]
  }

  ptimese<-themods*thecoefs ##Utilize R's practice of element-wise multiplication of matrices

  exp.val1<-aaply(ptimese, 2, sum, .parallel=parallel,na.rm=TRUE) ##Sum across the columns

  exp.val<-exp.val1*(g/(g+1)) ##Multiply by g/g+1
  names(exp.val)<-colnames(x)
  
  coefprob<-aaply(themods, 2, sum, .parallel=parallel,na.rm=TRUE)
  names(coefprob)<-colnames(x)
  
    ##'index' shows the model numbers for which each covariate has coefficient estimates larger than zero.
  index <- alply(thecoefs,2,function(x){which(x>0)})
  
  ##For each covariate, calculate the sum of model probabilities for models in which the coefficient estimate is larger than zero. Divide that by the sum of model probabilities for all models in which the covariate is included.
  coefprob.largerthanzero <- laply(1:ncol(thecoefs),function(i){sum(themods[,i][index[[i]]],na.rm=TRUE)})/as.numeric(aaply(themods,2,sum,na.rm=TRUE))
  
  ##The run.regs2 function takes list 1 from above, which is a list of models and extracts the SEs of 
  ##each coef from this list so that the regressions do not need to be rerun. 
  run.regs2 <- function(i, .parallel=parallel){
    list2<-summary(list1[[i]])$coefficients[,2]
    return(list2)
  }

##Use the run.regs2 function to get the standard errors of the coefficient estimates in each model.
  list2<-llply(1:length(set), run.regs2, .parallel=parallel)

##Rename the elements of list2 so it can be clearly identified which covariate the standard error refers to.
ses <- llply(1:length(list2),function(i){
	setNames(list2[[i]],colnames(x)[set[[i]]])
	}
	)

 theses <- themods
 
 ##Replace the odds of each model that the covariate is included with the standard error of the respective coefficient estimate.
  for(i in 1:length(ses)){
  	theses[i,][which(!is.na(theses[i,]))] <- ses[[i]]
  }
  
  ##Square the standard errors of coefficient estimates to get variances, and multiply the variances with model weights. Then sum the resulting matrix by columns to get the expected variance of each covariate. Finally take the square root of expected variances to get the conditional standard deviations.
  conditional.sd <- sqrt(aaply(theses^2*themods,2,sum,na.rm=TRUE))
  
  
  return(new("bma", x=x, y=y, thecoefs=thecoefs, combo.coef=coefs, theses=theses,
             combo.fit=fits,bmk=odds.bmk, exp.vals=exp.val, coefprobs=coefprob, coefprobs.largerthanzero=coefprob.largerthanzero,conditional.sds=conditional.sd))
          }#close function definition
) ##Close method

#thing<-fitBMA(bigx[1:100,1:5], y[1:100], parallel=FALSE)



