##creating a set of character vectors and conditionals to work with
char<-paste("var", 1:20)

char<-c(letters[1:9])

char <- c(letters[1:9])


modselect<-function(x=char, parallel=TRUE,core=10,all.nothing=c("a","b"), either.or=c("c", "d"),
                    always=c("e"),
                    one.if.other=c("f", "g", "h"), int_or_sqr = "interaction"
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
  
  
  #By setting int_or.sqr to "interaction" we are specifying that we are running squared terms rather than an interaction. Will require
  #that one.if.other contains 3 variables.
  if(int_or_sqr == "interaction"){
    
    if(length(one.if.other) < 3){stop("If specifying one.if.other (interaction), it must have three variables")}
    
    ##This is so we can gather all the variables on which there are conditions together
    conditionals<-c(all.nothing, either.or, always, one.if.other
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
    
    ##This is either.or. For now it returns each of the conditioned variables and FALSE for neither (or none).
    either.orcond<-c(either.or, FALSE)
    
    ##one.if.othercond currently only works for interaction terms, with the first variables 1, 2, and 3 (the last 
    ##of which is the interaction term between 1 and 2).
    one.if.othercond<-c(TRUE, one.if.other[1:length(one.if.other)-1], "both", "neither")
    
    ##This is the list generated for the conditional variables.
    conditionallist<-list(alwayscond=alwayscond, all.nothingcond=all.nothingcond, either.orcond=either.orcond,one.if.othercond=one.if.othercond
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
    
    newmatrix
    ##This for loop assigns TRUE to all variables in one.if.other to represent cases in which all variables are included in the model.
    for(i in 1:length(one.if.other)){
      newmatrix[which(models[,"one.if.othercond"]==TRUE),one.if.other[i]] <- TRUE
      
    }
    
    ##This for loop assigns TRUE to each constituent term to represent cases in which a single variable is included in the model.
    for(i in 1:(length(one.if.other)-1)){
      newmatrix[which(models[,"one.if.othercond"]%in%one.if.other[i]),one.if.other[i]] <- TRUE
    }
    
    ##This for loop assigns TRUE to both constituent terms to represent cases in which there is no interaction, but the constituent terms are included in the model.
    for(i in 1:(length(one.if.other)-1)){
      newmatrix[which(models[,"one.if.othercond"]=="both"),one.if.other[i]] <- TRUE
    }
    
    ##This for loop assigns FALSE to all variables in one.if.other to represent cases in which none of the variables are included in the model.
    for(i in 1:(length(one.if.other)-1)){
      newmatrix[which(models[,"one.if.othercond"]=="neither"),one.if.other[i]] <- FALSE
    }
    
    
    ##had to make this a for () loop. Couldn't get the llply to work on it for some strange reason.
    for (i in 1:length(either.or)){newmatrix[,either.or[i]]<-models[,"either.orcond"]==either.or[i]}
  }
  
  
  
  #By setting int_or.sqr to "squared" we are specifying that we are running squared terms rather than an interaction. Will require
  #that one.if.other contains 2 variables.
  if(int_or_sqr == "squared"){
    
    if(length(one.if.other) != 2){stop("If specifying one.if.other (squared), it must have two variables")}
    
    
    ##This is so we can gather all the variables on which there are conditions together
    conditionals<-c(all.nothing, either.or, always, one.if.other
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
    
    ##This is either.or. For now it returns each of the conditioned variables and FALSE for neither (or none).
    either.orcond<-c(either.or, FALSE)
    
    ##one.if.othercond currently only works for interaction terms, with the first variables 1, 2, and 3 (the last 
    ##of which is the interaction term between 1 and 2).
    
    one.if.othercond <- c(one.if.other[1], "both")
    
    ##This is the list generated for the conditional variables.
    conditionallist<-list(alwayscond=alwayscond, all.nothingcond=all.nothingcond, either.orcond=either.orcond,one.if.othercond=one.if.othercond
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
    
    
    #Converts those entries where only the non-squared term is included to 1's, while the squared term
    #remains a 0.
    for(i in 1:length(one.if.other)){
      newmatrix[which(models[,"one.if.othercond"] == one.if.other[1]), one.if.other[1]] <- TRUE
      newmatrix[which(models[,"one.if.othercond"] == one.if.other[1]), one.if.other[2]] <- FALSE
    }
    
    #Converts those entries where both the non-squared and squared terms are included to 1s.
    for(i in 1:length(one.if.other)){
      newmatrix[which(models[,"one.if.othercond"] == "both"), one.if.other[i]] <- TRUE
    }
    
    
    ##had to make this a for () loop. Couldn't get the llply to work on it for some strange reason.
    for (i in 1:length(either.or)){newmatrix[,either.or[i]]<-models[,"either.orcond"]==either.or[i]}
    
    
  }
  
  return(newmatrix)
}


##Note, a, b, and c (the all.nothings always match as they should, d and e are opposite except
##when both are false as they should be. f and g are always TRUE as they should be. The others vary since
##I commented out the one.if.other specifications.)
#modselect()


#head(models)
#tail(newmatrix)

###Important Notes:
##Model seems to work fine whether always and either.or are specified or not. 
##Model DOES NOT work if all.nothing is not specified. Not exactly sure why this is. 
##Also does not appear to work when one.if.other is not specified.

## Pseudocode ideas for the one if other type variables
##depcond<-"all", "other 1","other 2", "neither"
##if(depcond=="all"){all variables are true}
##if(depcond=="other"){just the "other" variable(s) are true, dependent (on others) variable is false}
##if(depcond=="neither"){Neither of the variables are included}


##Modified modelselector3 and wrote additional code for this script, modelselector4. Still need to edit code 
##to enable the user to specify when using squared terms rather than interaction term. Also, need to work on
##chaning for loop() to plyr function.

#Edited code to let user specify whether to include squared terms or interactions. Have not yet figured out 
#how to both. Code also nees to be trimmed. Tried simplifying it but with the linear way in which we build
#the model matrix, wasn't able to figure out how to.
