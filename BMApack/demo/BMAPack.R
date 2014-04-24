data(ABDagg)
ABDagg$demspendadv_x_partyadv <- ABDagg$demspendadv * ABDagg$partyadv
always <- "dincumbadv"
eitherOr <- list(c("directionm", "direction"))
allNothing <- list(c("lossmsq", "losssq"))
conditionals <- c(list("demspendadv_x_partyadv"))
conditionedOnTheseVariables <- list(c("demspendadv", "partyadv"))
x <- ABDagg[,-1]
x <- as.matrix(x)
y <- ABDagg[,1]
g <- 3
parallel <- FALSE


(BMAObject <- fitBMA(x, y, g, parallel, allNothing, eitherOr, always,conditionals,conditionedOnTheseVariables))

summary(BMAObject)

# The data used for this demo is ABDagg, which can be found in BMAPack. An additional interaction term has been added as a variable to the dataset, demspendadv_x_partyadv, in order to demonstrate the functionality of the conditionals and conditionedOnTheseVariables arguments. The above provides both the output from the fitBMA function as well as the summary.bma function. With respect to plot.bma, because the data is standardized and the intercept term is removed, standard errors will be quite small. The y-axis will sometimes exceed 1 for this reason. Please note, the area under that curve sums to 1, and the probability that a coefficient assumes a value along an interval is equal to the area under the curve over that interval.

plot(BMAObject)







