char.vec<-paste("var", 1:7)
input<-char.vec
##either.or variables are those in which one is included or the other but never both
##all.nothing variables are those in which a set should always be included or never included 
##one.if.other are variables such as squares and interactions that must include constituent terms

model.selector<-function(input, either.or=NULL, all.nothing=NULL, one.if.other=NULL, parallel=FALSE){
  
  if(length(all.nothing)==1){stop("If specifying All.nothing, it must have at least two variables")}
  if(length(either.or)==1){stop("If specifying Either.or, it must have at least two variables")}
  if(length(one.if.other)==1){stop("If specifying one.if.other, it must have at least two variables")}
  
  names<-(input) ##For now since the input is a character vector, it's just the input

  set <- llply(1:length(input),function(X){combn(length(input),X,simplify=F)}, 
               .parallel=parallel)
  
  ##making each combination a single list item
  set<-unlist(set, recursive=F)
  
  ##setting variable names
  named.set<-llply(1:length(set), function(i){names[set[[i]]]}, .parallel=parallel)
  
  good.models<-list()

  thetests<-function(i){
    theall.nothings<-length(unique(all.nothing %in% named.set[[i]]))==1 ##we want true here
    theeither.ors<-((length(unique(either.or %in% named.set[[i]]))==1)==FALSE | 
                      any(all.nothing %in% named.set[[i]]==FALSE))##good models are true
    theoneif.others<-((one.if.other %in% named.set[[i]])[1]==TRUE & 
                      all((one.if.other %in% named.set[[i]])[2:length(one.if.other)])==FALSE)==FALSE 
                      ##good models come out of this mess^ as TRUE
    good<-all(theall.nothings, theeither.ors, theoneif.others)
    return(good)   
  }
  
    ##are the variables in there?
    good.models<-llply(1:length(named.set), thetests ,.parallel=parallel)

    named.set[unlist(good.models)]

  return(good.models3)
}

system.time(trial<-model.selector(char.vec, 
                                  either.or=NULL, 
                                  all.nothing=c("var 1", "var 2"),
                                  one.if.other=c("var 3", "var 4", "var 5")
))

##With this specification, it takes 31 models and trims it down to 9 models. 
##With ten variables, it takes 1023 models and trims it down to 319. 
##With eleven variables, it takes 2047 and trims it down to 639.
##With 15 variables, it takes 32767 and trims it down to 10239, takes 
##about 2.8 seconds on my memory poor computer for 15. 
