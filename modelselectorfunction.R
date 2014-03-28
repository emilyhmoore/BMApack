char.vec<-paste("var", 1:5)

##either.or variables are those in which one is included or the other but never both
##all.nothing variables are those in which a set should always be included or never included 
##one.if.other are variables such as squares and interactions that must include constituent terms

model.selector<-function(input, either.or, all.nothing=c("var 1," "var 2"), one.if.other){
  names<-(input)
  set <- llply(1:length(input),function(X){combn(length(input),X,simplify=F)}, 
               .parallel=parallel)
  
  ##making each combination a single list item
  set<-unlist(set, recursive=F)
  
  ##setting variable names
  named.set<-llply(1:length(set), function(i){names[set[[i]]]})
  
  ##are the variables in there?
  all.nothingset<-llply(1:length(named.set), function(i){all.nothing %in% named.set[[i]]})
  
  ##if the length of the unique is one, it means either all or none of the all.nothings are included
  theall.nothings<-llply(1:length(all.nothingset),function(i){length(unique(all.nothingset[[i]]))==1})
  
  ##these are th emodels we want, so which ones are they??
  pick.me<-which(theall.nothings==TRUE)
  
  ##This is the reduced set of models
  good.models<-set[pick.me]
}