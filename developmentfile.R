library(devtools)
library(roxygen2)
find_rtools() ##For using devtools with windows. 
setwd("~/GitHub/BMAPack")
create(path="./BMApack", check=FALSE)

current.code <- as.package("BMAPack")
load_all(current.code)
document(current.code)

install(pkg=current.code, local=TRUE)

check(current.code)

##Example data

help(fitBMA)
help.search('summary,bma-method')
example(fitBMA)
example('summary,bma-method')
example('plot,bma-method') ##may take a sec


