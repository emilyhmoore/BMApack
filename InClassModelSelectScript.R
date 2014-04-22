##modelSelect function 

##Here are the arguments that would go into a modelSelect function as an example.  
varNames=colnames(x)

parallel=FALSE 

allNothing=list(c("var 1", "var 2"),c("var 6", "var 7"))

always="var 3" 

eitherOr=list(c("var 4", "var 5"), c("var 8", "var 9"))

conditionals=list(c("var 10"),c("var 12"))

conditionedOnTheseVariables<-list(c("var 11"), c("var 13", "var 14"))

##this is a trial of the whole modelSelect function, which we've already set up on the computer.
trial<-modelSelect(varNames=colnames(x), parallel=FALSE, 
                   allNothing=list(c("var 1", "var 2"),c("var 6", "var 7")), 
                   always="var 3", 
                   eitherOr=list(c("var 4", "var 5"), c("var 8", "var 9")),
                   conditionals=list(c("var 10"),c("var 12")),
                   conditionedOnTheseVariables<-list(c("var 11"), c("var 13", "var 14")))

##This is what the final product looks like
head(trial)
tail(trial)

##Here are some pieces of the code to look at. 
##(Note that for time constraints, we're not showing all of it)  

###########################################

##The restricteds object contains variables that are conditioned.
(restricteds<-c(unlist(allNothing),
               always,
               unlist(eitherOr), 
               unlist(conditionals), 
               unlist(conditionedOnTheseVariables)))

(restrictedsIndex <- which(varNames%in%restricteds))
(unrestricteds <- varNames[-restrictedsIndex])

###########################################

#
(alwaysCondition <- TRUE)

#
(allNothingCondition <-replicate(length(allNothing),list(c(TRUE, FALSE))))

###########################################

#
(restrictedsList)

###########################################

#
restrictedsModels <- expand.grid(restrictedsList) ##Here is the head of the set of models at this pt.

head(restrictedsModels) ##Note, due to the way we've specified, we cut down a bunch of models just by
##coding some variables a special way. 

#
dim(restrictedsModels) ##Look at all those models!

##This is a test for stripping out models that fail the eitherOr condition
eitherOrTest <- function(x){length(which(x==TRUE))==1 | any(as.logical(x))==FALSE}

##This is a test for stripping out models that fail the conditionals restrictions. 
conditionalsTest<-function(x){x[1]==FALSE | all(as.logical(x))==TRUE}       

##The first column tells us which models failed for the first set of eitherOrs
##the second column tells us which models failed for the second set of eitherOrs
eitherOrTestResults <- matrix(eitherOrTestResults,ncol=length(eitherOr),byrow=FALSE)

head(eitherOrTestResults,20)

##This returns a vector to use for indexing out "bad" models.
TestResultsCombined <- aaply(cbind(eitherOrTestResults,conditionalsTestResults),1,
                             function(x){ifelse(all(x),TRUE,FALSE)})
##this is what the vector looks like. 
head(TestResultsCombined, 50)              
#

restrictedsMatrix <- restrictedsMatrix[TestResultsCombined,]

head(restrictedsMatrix, 10)
tail(restrictedsMatrix, 10)

#
##This function takes the rows from the restricedMatrix above and replaces it for each
##value of the temp variable.
bindTogether <- function(i){cbind(unrestrictedsMatrix[unrestrictedsMatrix$temp==i,], restrictedsMatrix[i,])}

##Do not run##

##The unrestrictedslist has the same number of elements as there are unrestricted variables.
unrestrictedsList <- llply(1:length(unrestricteds), 
                           function(i){unrestrictedsList[[i]]<-c(TRUE, FALSE)},
                           .parallel=parallel)

##In addition, we add a temp variable that takes on a value for each row in the restrictedsMatrix.
unrestrictedsList <- c(unrestrictedsList, list(temp=1:nrow(restrictedsMatrix)))
                
unrestrictedsMatrix <- expand.grid(unrestrictedsList)

##This is what the unrestrictedsMatrix looks like.
head(unrestrictedsMatrix)

##Finally we use the bindTogether function discussed ago to match the temp variable value to the
##appropriate row in the restrictedsMatrix.
modelMatrix <- do.call("rbind",llply(1:nrow(restrictedsMatrix), bindTogether))

##Remove the temp variable
modelMatrix$temp<-NULL

##Reorder the variables to be in the same order as the original x matrix. 
modelMatrix <- modelMatrix[colnames(x)]

#
modelMatrix[500:550,] ##Look at it!!!

###########################################

##Originally, 32,768 models 2^15
##After modelSelect(), 1,080 models
