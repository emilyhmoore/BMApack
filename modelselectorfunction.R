char.vec<-paste("var", 1:5)

##either.or variables are those in which one is included or the other but never both
##all.nothing variables are those in which a set should always be included or never included 
##one.if.other are variables such as squares and interactions that must include constituent terms

model.selector<-function(input, either.or=NULL, all.nothing=NULL, one.if.other=NULL){
  
  if(length(all.nothing)==1){stop("All.nothing must have at least two variables")}
  
  names<-(input)
  set <- llply(1:length(input),function(X){combn(length(input),X,simplify=F)}, 
               .parallel=parallel)
  
  ##making each combination a single list item
  set<-unlist(set, recursive=F)
  
  ##setting variable names
  named.set<-llply(1:length(set), function(i){names[set[[i]]]})
  
  good.models<-list()

  if(length(all.nothing)==0){good.models<-set} else{
  ##are the variables in there?
  all.nothingset<-llply(1:length(named.set), function(i){all.nothing %in% named.set[[i]]})

  ##if the length of the unique is one, it means either all or none of the all.nothings are included
  theall.nothings<-llply(1:length(all.nothingset),function(i){length(unique(all.nothingset[[i]]))==1})

  ##these are th emodels we want, so which ones are they??
  pick.me<-which(theall.nothings==TRUE)
  
  ##This is the reduced set of models
  good.models<-named.set[pick.me]
  }
  
  good.models2<-list()
  if(length(either.or)==0){good.models2<-good.models} else{
  either.orset<-llply(1:length(good.models), function(i){either.or %in% good.models[[i]]})

  ##if the length of the unique is one, it means either all or none of the all.nothings are included
  theeither.ors<-llply(1:length(either.orset),function(i){length(unique(either.orset[[i]]))==1})
  
  pick.me2<-which(theeither.ors==FALSE)

  good.models2<-good.models[pick.me2]
  }
  
  return(good.models2)
}

model.selector(char.vec, either.or=c("var 3", "var 4"), all.nothing=c("var 1", "var 2"),one.if.other=NULL)
