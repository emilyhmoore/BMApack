##creating a set of character vectors and conditionals to work with
char<-paste("var", 1:20)

char<-c(letters[1:10])

modselect<-function(x=char, parallel=TRUE,core=10,all.nothing=c("a","b","c"), either.or=c("d", "e"),
                    always=c("f", "g")
                    #,one.if.other=c("h", "i", "j")
                    )
  {
  
  library(plyr)
  
   #run only if parallel is TRUE
  if(parallel==TRUE){
  library(foreach)
  library(multicore)
  library(doMC)
  registerDoMC(cores=core) ##Will need for later for parallel stuff
  }
  
  if(length(all.nothing)==1){stop("If specifying All.nothing, it must have at least two variables")}
  if(length(either.or)==1){stop("If specifying Either.or, it must have at least two variables")}
  #if(length(one.if.other)==1){stop("If specifying one.if.other, it must have at least two variables")}

  ##This is so we can gather all the variables on which there are conditions together
  conditionals<-c(all.nothing, either.or, always
                #,one.if.other
                )

  ##Which variables are the conditionals? It figures this out because we want to be able to 
  ##generically separate all of the conditional type variables from the unconditional type variables.
  theconditionals<-which(char%in%conditionals)
  therest<-char[-theconditionals]

  ##This is an empty list that will be used for the "unconditioned" variables and put into expand grid
truefalse.list<-list()

  ##The truefalse.list will not be created if there is no unconditioned variable. If there are unconditioned variables, the code generates a list that says "true false" for each unconditioned variable. 
  ##I did it this way separately for each type and then merged it because that made more sense in my head
##since later we differentiate between conditioned and unconditioned variables.
if(length(therest)!=0){
length(truefalse.list)<-length(therest)
truefalse.list<-llply(1:length(therest), function(i){truefalse.list[[i]]<-c(TRUE, FALSE)},.parallel=parallel)
names(truefalse.list)<-therest
}

##This is the always condition. It means this variables always has to be in.
alwayscond<-TRUE

##This is the all, nothing condition. It's one variable acting for two or more.
all.nothingcond<-c(TRUE, FALSE)

##This is either.or. For now it returns the first var, the second var, or FALSE for neither.
either.orcond<-c(either.or, FALSE)

##currently written for interaction terms with one variable as the interaction and the other two as 
##constituent terms. Could also be made to work for square terms, I think, by having only "both" "neither"
##and the base term name. 
#one.if.othercond<-c("all", one.if.other[2:length(one.if.other)], "both", "neither")

##This is the list generated for the conditional variables.
conditionallist<-list(alwayscond=alwayscond, all.nothingcond=all.nothingcond, either.orcond=either.orcond
                      #,one.if.othercond=one.if.othercond
                      ) 

##Merge the conditional list and the unconditional list
thirdlist<-c(conditionallist, truefalse.list)

##This is the different possible model specifications that can be included.
##NOTE: I didn't end up making two separate matrices of conditional and unconditional because this actually
##seemed to work just fine, and I think it will be shorter in the long run.

models<-expand.grid(thirdlist)
##This is a new matrix that has the number of models in models but has a column for each variable.
##It then sets the names of the matrix to match the variable names of interest.
newmatrix<-matrix(rep(0),ncol=length(char), nrow=nrow(models))
colnames(newmatrix)<-char
##Making into a dataframe because expand.grid returned a data frame and I want this to be the same.
newmatrix<-as.data.frame(newmatrix)

##Setting the unconditioned variables into the appropriate part of the model
##This simply copies the values from one to the values of the others using the names stored in "therest" object
newmatrix[,therest]<-models[,therest]

##This matches the variables marked as always with the corresponding variable in the models object
newmatrix[,always]<-models[,"alwayscond"]

##This matches the all.nothing condition with the all.nothing variable.
##Basically it just matches the TRUE or FALSE in the model spec to all of the variables included
##in all.nothing
newmatrix[,all.nothing]<-models[,"all.nothingcond"]

##This code basically puts the first variable as TRUE when the models object says to use that 
##variable's name, and it returns FALSE for the other variables. If the previous model object said FALSe
##indicating Neither should be included, then both will be FALSE in the full spec.
##The idea is to use the newmatrix to index for the model building in the fitBMA function. 
	
##had to make this a for () loop. Couldn't get the llply to work on it for some strange reason.
for (i in 1:length(either.or)){newmatrix[,either.or[i]]<-models[,"either.orcond"]==either.or[i]}

#newmatrix[,either.or[1]]<-models[,"either.orcond"]==either.or[1]
#newmatrix[,either.or[2]]<-models[,"either.orcond"]==either.or[2]

return(newmatrix)
}

##Note, a, b, and c (the all.nothings always match as they should, d and e are opposite except
##when both are false as they should be. f and g are always TRUE as they should be. The others vary since
##I commented out the one.if.other specifications.)
modselect()

head(models)
tail(newmatrix)

###Important Notes:
##Model seems to work fine whether always and either.or are specified or not. 
##Model DOES NOT work if all.nothing is not specified. Not exactly sure why this is. 

## Pseudocode ideas for the one if other type variables
##depcond<-"all", "other 1","other 2", "neither"
##if(depcond=="all"){all variables are true}
##if(depcond=="other"){just the "other" variable(s) are true, dependent (on others) variable is false}
##if(depcond=="neither"){Neither of the variables are included}

