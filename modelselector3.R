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

##This produces the model combinations for the unconditioned variables. 
modelcomb.unconditional<-expand.grid(truefalse.list)

rep("seq1", length(therest))


