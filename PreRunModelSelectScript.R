##This is the modelSelect function. Run this ahead of time so the modelSelect function exists
##In the workspace prior to the presentation. 

varNames <- colnames(x)

##The modelSelect function returns the correct model configurations.  
modelSelect<-function(varNames=colnames(x), 
                      parallel=FALSE,
                      allNothing, 
                      eitherOr,
                      always,
                      conditionals,
                      conditionedOnTheseVariables
)
{ 
  
  ##The restricteds object contains variables that are conditioned.
  restricteds<-c(unlist(allNothing),
                 always,
                 unlist(eitherOr), 
                 unlist(conditionals), 
                 unlist(conditionedOnTheseVariables))
  
  ##restrictedsIndex returns the index of the conditioned variables. 
  ##This is necessary because we want to separate the conditioned ones 
  ##from the unconditioned variables.
  restrictedsIndex <- which(varNames%in%restricteds)
  unrestricteds <- varNames[-restrictedsIndex]
  
  ##The always condition is always included.
  alwaysCondition <- TRUE
  
  ##The allNothing condition is either included or not included.
  allNothingCondition<-replicate(length(allNothing),list(c(TRUE, FALSE)))
  
  ##The restrictedsList is all configurations for the conditioned variables combined.
  restrictedsList<-c(alwaysCondition=alwaysCondition, 
                     allNothingCondition=allNothingCondition)
  
  ##Save the list names of the allNothingConditions in order to use them as indices later.
  allNothingConditionNames <- names(restrictedsList)[-1]
  
  ##This is all restricteds that are not always or allNothing types
  otherrestricteds<-restricteds[-c(which(restricteds%in%always), 
                                   which(restricteds%in%unlist(allNothing)))]
  
  ##Make a list for the variables that are just going to be true false before they are 
  ##stripped away
  otherrestrictedsList<-list()
  if(length(otherrestricteds)!=0){
    
    otherrestrictedsList<-llply(1:length(otherrestricteds),
                                function(i){otherrestrictedsList[[i]]<-c(TRUE, FALSE)},
                                .parallel=parallel)
  }
  
  names(otherrestrictedsList)<-otherrestricteds
  
  restrictedsList<-c(restrictedsList, otherrestrictedsList)
  
  ##Expand grid on the restricted variables.
  restrictedsModels <- expand.grid(restrictedsList)
  
  
  restrictedsMatrix <-matrix(rep(FALSE),ncol=length(c(unlist(allNothing), always)), 
                             nrow=nrow(restrictedsModels))
  
  colnames(restrictedsMatrix)<-c(unlist(allNothing), always)
  
  ##Put in the configurations for the alwaysCondition variables into restrictedsMatrix.
  restrictedsMatrix[,always]<-restrictedsModels[,"alwaysCondition"]
  
  ##Do the same for the allNothingCondition. Since the allNothingCondition is a list, 
  ##each list should be considered separately.
  
  for(i in 1:length(allNothing)){
    restrictedsMatrix[,allNothing[[i]]]<-restrictedsModels[,allNothingConditionNames[i]]
  }
  
  ##cbind that to the expandgrid results for any of the "otherrestricteds" 
  ##which is just restricteds that are not always or allNothing types.
  ##They vary as normal in expand grid and are stripped out later.
  restrictedsMatrix<-cbind(restrictedsMatrix,restrictedsModels[,otherrestricteds])
  
  ###############################################################################
  ###############This is where we need to strip out "bad" models#################
  ###############BEFORE the temp variable is created for use with################
  ###############the unrestricteds###############################################
  ###############################################################################
  
  ##This strips out models that fail the eitherOr test. 
  ##Basically, it makes sure there is only one TRUE or all FALSES
  ##The as.logical line in there because, for some reason,
  ##it doesn't think the restrictedsMatrix is a logical.
  
  ##good models are TRUE
  eitherOrTest<-function(x){length(which(x==TRUE))==1 | any(as.logical(x))==FALSE}
  
  ##asks if the first value is FALSE. If it is, the model gets a TRUE
  ##if the first value is TRUE, then the model only gets a TRUE if everything is TRUE.
  conditionalsTest<-function(x){x[1]==FALSE | all(as.logical(x))==TRUE}
  
  ##Cannot apply over the whole row or it will apply to always and allNothing too
  ##so this indexes the matrix according only to those models in eitherOr
  ##The unlist part is to get it as a vector that can be used for indexing the whole matrix.
  eitherOrTestResults <- NULL
  for(i in 1:length(eitherOr)){
    eitherOrStripIndex <- unlist(alply(restrictedsMatrix[eitherOr[[i]]], 1, eitherOrTest))
    eitherOrTestResults <- c(eitherOrTestResults,eitherOrStripIndex)
  }
  
  conditionalsTestResults<-NULL
  for (i in 1:length(conditionals)){
    conditionalsStripIndex<-unlist(
      alply(
        ##This nasty piece is to bind together the conditionals with the variables
        ##on which they're dependent. There should be a way to do this as a list
        ##of lists instead, which would be more parsimonious. 
        restrictedsMatrix[,cbind(conditionals[[i]], conditionedOnTheseVariables[[i]])],
        1,conditionalsTest)##apply over rows the fun. conditionalsTest
    )
    conditionalsTestResults<-c(conditionalsTestResults, conditionalsStripIndex)
  }
  
  ##Since eitherOrTestResults is a vector, transform it into a matrix with rows 
  ##indicating each set of the eitherOr condition and rows indicating model numbers. 
  ##The transformation into a matrix is for convenience in the next part. 
  eitherOrTestResults <- matrix(eitherOrTestResults,ncol=length(eitherOr),byrow=FALSE)
  
  conditionalsTestResults<-matrix(conditionalsTestResults, ncol=length(conditionals), byrow=FALSE)
  
  ##Given the matrix of eitherOrTestResults, create a vector whose element is 
  ##TRUE if all eitherOr conditions are TRUE. Otherwise, it is FALSE. This is 
  ##done because if the user specifies more than one eitherOr condition, models 
  ##for which the conditions hold for all eitherOr conditions should be calculated.
  TestResultsCombined <- aaply(cbind(eitherOrTestResults,conditionalsTestResults),1,
                               function(x){
                                 ifelse(all(x),TRUE,FALSE)
                               })
  
  
  ##This indexes the matrix (and resaves it) by the test results .
  ##So if the test came back TRUE, the model is kept. If the test is false,
  ##it removes that row.
  restrictedsMatrix<-restrictedsMatrix[TestResultsCombined,]
  
  ######################End Tests################################################
  
  
  ##This function matches the models with a particular temp value to a row 
  ##in the restrictedsMatrix 
  ##Thus, for each model where temp==1 in unrestricteds, we match it to the first row
  ##of the restricteds. Same with 2 and so on all the way through the number of rows
  ##in the unrestricteds matrix (number of those type of models.
  bindTogether<-function(i){cbind(unrestrictedsMatrix[unrestrictedsMatrix$temp==i,], 
                                  restrictedsMatrix[i,])}
  
  ##This is an empty list for the unconditioned variables that will be put into the 
  ##expand.grid function.
  unrestrictedsList<-list()
  
  
  if(length(unrestricteds)!=0){
    
    ##The unrestrictedsList will not be created if all variables are conditioned. 
    ##If there are unconditioned variables, however, the following code generates a 
    ##list that says TRUE and FALSE for each unconditioned variable.
    length(unrestrictedsList)<-length(unrestricteds)
    unrestrictedsList<-llply(1:length(unrestricteds), 
                             function(i){unrestrictedsList[[i]]<-c(TRUE, FALSE)},
                             .parallel=parallel)
    unrestrictedsList<-c(unrestrictedsList, list(temp=1:nrow(restrictedsMatrix)))
    names(unrestrictedsList)<-c(unrestricteds, "temp")
    
    ##Expand grid on the unconditioned variables.
    unrestrictedsMatrix <- expand.grid(unrestrictedsList)
    
    ##Here, I'm llplying over all of the rows of the conditioned variable combination matrix
    ##This is because temp in the unrestricted matrix takes on a new value for each model 
    ##in the conditioned matrix.So this matches temp==1 in unconditioned to row 1 
    ##of the conditioned matrix 
    ##lply returns a list and laply doesn't work in this context, so I use do.call with rbind
    ##to get a matrix here.
    modelMatrix<-do.call("rbind",llply(1:nrow(restrictedsMatrix), bindTogether))
    
    ##Finally, we remove temp. This makes it so modelMatrix is exactly the same but without
    ##the temp variable.
    modelMatrix$temp<-NULL
    
    ##This ensures that the variables go back into the order they originally were in for the input
    ##matrix of fitbma. This is essential because they can easily get out of order in the
    ##conditioning process.
    modelMatrix<-modelMatrix[colnames(x)]
    
  }else{
    modelMatrix <- restrictedsMatrix
    
    ##This ensures that the variables go back into the order they originally were in for the input
    ##matrix of fitbma. This is essential because they can easily get out of order in the
    ##conditioning process.
    modelMatrix<-modelMatrix[colnames(x)]
  }
  
  return(modelMatrix)
}##close modelSelect function

x=matrix(rnorm(1500), ncol=15)
colnames(x)<-paste("var", 1:15)
y<-5*x[,1]+2*x[,2]+rnorm(100)

trial<-modelSelect(varNames=colnames(x), parallel=FALSE, 
       allNothing=list(c("var 1", "var 2"),c("var 6", "var 7")), 
       always="var 3", 
       eitherOr=list(c("var 4", "var 5"), c("var 8", "var 9")),
       conditionals=list(c("var 10"),c("var 12")),
       conditionedOnTheseVariables<-list(c("var 11"), c("var 13", "var 14")))

##PIECEWISE TO RUN!
##Just select-run this as a block so all of the stuff will be in the work space individually. 
varNames=colnames(x)
parallel=FALSE 
allNothing=list(c("var 1", "var 2"),c("var 6", "var 7")) 
always="var 3" 
eitherOr=list(c("var 4", "var 5"), c("var 8", "var 9"))
conditionals=list(c("var 10"),c("var 12"))
conditionedOnTheseVariables<-list(c("var 11"), c("var 13", "var 14"))


  ##The restricteds object contains variables that are conditioned.
  restricteds<-c(unlist(allNothing),
                 always,
                 unlist(eitherOr), 
                 unlist(conditionals), 
                 unlist(conditionedOnTheseVariables))
  
  ##restrictedsIndex returns the index of the conditioned variables. 
  ##This is necessary because we want to separate the conditioned ones 
  ##from the unconditioned variables.
  restrictedsIndex <- which(varNames%in%restricteds)
  unrestricteds <- varNames[-restrictedsIndex]
  
  ##The always condition is always included.
  alwaysCondition <- TRUE
  
  ##The allNothing condition is either included or not included.
  allNothingCondition<-replicate(length(allNothing),list(c(TRUE, FALSE)))
  
  ##The restrictedsList is all configurations for the conditioned variables combined.
  restrictedsList<-c(alwaysCondition=alwaysCondition, 
                     allNothingCondition=allNothingCondition)
  
  ##Save the list names of the allNothingConditions in order to use them as indices later.
  allNothingConditionNames <- names(restrictedsList)[-1]
  
  ##This is all restricteds that are not always or allNothing types
  otherrestricteds<-restricteds[-c(which(restricteds%in%always), 
                                   which(restricteds%in%unlist(allNothing)))]
  
  ##Make a list for the variables that are just going to be true false before they are 
  ##stripped away
  otherrestrictedsList<-list()
  if(length(otherrestricteds)!=0){
    
    otherrestrictedsList<-llply(1:length(otherrestricteds),
                                function(i){otherrestrictedsList[[i]]<-c(TRUE, FALSE)},
                                .parallel=parallel)
  }
  
  names(otherrestrictedsList)<-otherrestricteds
  
  restrictedsList<-c(restrictedsList, otherrestrictedsList)
  
  ##Expand grid on the restricted variables.
  restrictedsModels <- expand.grid(restrictedsList)
  
  
  restrictedsMatrix <-matrix(rep(FALSE),ncol=length(c(unlist(allNothing), always)), 
                             nrow=nrow(restrictedsModels))
  
  colnames(restrictedsMatrix)<-c(unlist(allNothing), always)
  
  ##Put in the configurations for the alwaysCondition variables into restrictedsMatrix.
  restrictedsMatrix[,always]<-restrictedsModels[,"alwaysCondition"]
  
  ##Do the same for the allNothingCondition. Since the allNothingCondition is a list, 
  ##each list should be considered separately.
  
  for(i in 1:length(allNothing)){
    restrictedsMatrix[,allNothing[[i]]]<-restrictedsModels[,allNothingConditionNames[i]]
  }
  
  ##cbind that to the expandgrid results for any of the "otherrestricteds" 
  ##which is just restricteds that are not always or allNothing types.
  ##They vary as normal in expand grid and are stripped out later.
  restrictedsMatrix<-cbind(restrictedsMatrix,restrictedsModels[,otherrestricteds])
  
  ###############################################################################
  ###############This is where we need to strip out "bad" models#################
  ###############BEFORE the temp variable is created for use with################
  ###############the unrestricteds###############################################
  ###############################################################################
  
  ##This strips out models that fail the eitherOr test. 
  ##Basically, it makes sure there is only one TRUE or all FALSES
  ##The as.logical line in there because, for some reason,
  ##it doesn't think the restrictedsMatrix is a logical.
  
  ##good models are TRUE
  eitherOrTest<-function(x){length(which(x==TRUE))==1 | any(as.logical(x))==FALSE}
  
  ##asks if the first value is FALSE. If it is, the model gets a TRUE
  ##if the first value is TRUE, then the model only gets a TRUE if everything is TRUE.
  conditionalsTest<-function(x){x[1]==FALSE | all(as.logical(x))==TRUE}
  
  ##Cannot apply over the whole row or it will apply to always and allNothing too
  ##so this indexes the matrix according only to those models in eitherOr
  ##The unlist part is to get it as a vector that can be used for indexing the whole matrix.
  eitherOrTestResults <- NULL
  for(i in 1:length(eitherOr)){
    eitherOrStripIndex <- unlist(alply(restrictedsMatrix[eitherOr[[i]]], 1, eitherOrTest))
    eitherOrTestResults <- c(eitherOrTestResults,eitherOrStripIndex)
  }
  
  conditionalsTestResults<-NULL
  for (i in 1:length(conditionals)){
    conditionalsStripIndex<-unlist(
      alply(
        ##This nasty piece is to bind together the conditionals with the variables
        ##on which they're dependent. There should be a way to do this as a list
        ##of lists instead, which would be more parsimonious. 
        restrictedsMatrix[,cbind(conditionals[[i]], conditionedOnTheseVariables[[i]])],
        1,conditionalsTest)##apply over rows the fun. conditionalsTest
    )
    conditionalsTestResults<-c(conditionalsTestResults, conditionalsStripIndex)
  }
  
  ##Since eitherOrTestResults is a vector, transform it into a matrix with rows 
  ##indicating each set of the eitherOr condition and rows indicating model numbers. 
  ##The transformation into a matrix is for convenience in the next part. 
  eitherOrTestResults <- matrix(eitherOrTestResults,ncol=length(eitherOr),byrow=FALSE)
  
  conditionalsTestResults<-matrix(conditionalsTestResults, ncol=length(conditionals), byrow=FALSE)
  
  ##Given the matrix of eitherOrTestResults, create a vector whose element is 
  ##TRUE if all eitherOr conditions are TRUE. Otherwise, it is FALSE. This is 
  ##done because if the user specifies more than one eitherOr condition, models 
  ##for which the conditions hold for all eitherOr conditions should be calculated.
  TestResultsCombined <- aaply(cbind(eitherOrTestResults,conditionalsTestResults),1,
                               function(x){
                                 ifelse(all(x),TRUE,FALSE)
                               })
  
  
  ##This indexes the matrix (and resaves it) by the test results .
  ##So if the test came back TRUE, the model is kept. If the test is false,
  ##it removes that row.
  restrictedsMatrix<-restrictedsMatrix[TestResultsCombined,]
  
  ######################End Tests################################################
  
  
  ##This function matches the models with a particular temp value to a row 
  ##in the restrictedsMatrix 
  ##Thus, for each model where temp==1 in unrestricteds, we match it to the first row
  ##of the restricteds. Same with 2 and so on all the way through the number of rows
  ##in the unrestricteds matrix (number of those type of models.
  bindTogether<-function(i){cbind(unrestrictedsMatrix[unrestrictedsMatrix$temp==i,], 
                                  restrictedsMatrix[i,])}
  
  ##This is an empty list for the unconditioned variables that will be put into the 
  ##expand.grid function.
  unrestrictedsList<-list()
  
  
  if(length(unrestricteds)!=0){
    
    ##The unrestrictedsList will not be created if all variables are conditioned. 
    ##If there are unconditioned variables, however, the following code generates a 
    ##list that says TRUE and FALSE for each unconditioned variable.
    length(unrestrictedsList)<-length(unrestricteds)
    unrestrictedsList<-llply(1:length(unrestricteds), 
                             function(i){unrestrictedsList[[i]]<-c(TRUE, FALSE)},
                             .parallel=parallel)
    unrestrictedsList<-c(unrestrictedsList, list(temp=1:nrow(restrictedsMatrix)))
    names(unrestrictedsList)<-c(unrestricteds, "temp")
    
    ##Expand grid on the unconditioned variables.
    unrestrictedsMatrix <- expand.grid(unrestrictedsList)
    
    ##Here, I'm llplying over all of the rows of the conditioned variable combination matrix
    ##This is because temp in the unrestricted matrix takes on a new value for each model 
    ##in the conditioned matrix.So this matches temp==1 in unconditioned to row 1 
    ##of the conditioned matrix 
    ##lply returns a list and laply doesn't work in this context, so I use do.call with rbind
    ##to get a matrix here.
    modelMatrix<-do.call("rbind",llply(1:nrow(restrictedsMatrix), bindTogether))
    
    ##Finally, we remove temp. This makes it so modelMatrix is exactly the same but without
    ##the temp variable.
    modelMatrix$temp<-NULL
    
    ##This ensures that the variables go back into the order they originally were in for the input
    ##matrix of fitbma. This is essential because they can easily get out of order in the
    ##conditioning process.
    modelMatrix<-modelMatrix[colnames(x)]
    
  }else{
    modelMatrix <- restrictedsMatrix
    
    ##This ensures that the variables go back into the order they originally were in for the input
    ##matrix of fitbma. This is essential because they can easily get out of order in the
    ##conditioning process.
    modelMatrix<-modelMatrix[colnames(x)]}

