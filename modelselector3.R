##creating a set of character vectors and conditionals to work with
char<-paste("var", 1:10)
all.nothing<-c("var 1", "var 2")
either.or<-c("var 3", "var 4")
always<-"var 5"

##This is so we can gather all the variables on which there are conditions together
conditionals<-c(all.nothing, either.or, always)

##Which variables are the conditionals? It figures this out because we want to be able to 
##generically separate all of the conditional type variables from the unconditional type variables.
theconditionals<-which(char==conditionals)
therest<-char[-theconditionals]

##This is a list that will be used for the "unconditioned" variables and put into expand grid
##It generates a list that says "true false" for each unconditioned variable. 
truefalse.list<-list()
length(truefalse.list)<-length(therest)
truefalse.list<-llply(1:length(therest), function(i){truefalse.list[i]<-c(TRUE, FALSE)})
names(truefalse.list)<-therest

##This is the always condition. It means this variables always has to be in.
alwayscond<-TRUE

##This is the all, nothing condition. It's one variable acting for two. 
all.nothingcond<-c(TRUE, FALSE)

##This is either.or. For now it returns the first var, the second var, or FALSE for neither.
either.orcond<-c(either.or[1], either.or[2], FALSE)

##This is the list generated for the conditional variables. I will manipulate it in a moment. 
conditionallist<-list(alwayscond=alwayscond, all.nothingcond=all.nothingcond, either.orcond=either.orcond) 

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

##This code basically puts var 3 as TRUE when the models object says "var 3", it returns
##FALSE for the corresponding Var 4 and vice versa. If the previous model object said FALSe
##indicating Neither should be included, then both will be FALSE in the full spec.
##The idea is to use the newmatrix to index for the model building in the fitBMA function.
##As of now, it will only work if the user specifies two either.or variables, but I think it should be
##able to generalize fairly easily. 

newmatrix[,either.or[1]]<-models[,"either.orcond"]==either.or[1]
newmatrix[,either.or[2]]<-models[,"either.orcond"]==either.or[2]


head(models)
head(newmatrix)

