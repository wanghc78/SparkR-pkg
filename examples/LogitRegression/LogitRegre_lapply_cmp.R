app.name <- "LogitRegression_lapply_cmp"
source("setup_logitRegre.R")

library(vecapply)

run <- function(data) {
    
    #X includes "1" column, Y column vec
    grad.func <- function(yx) {
        y <- yx[1]
        x <- yx  
        x[1] <- 1 #modify the 1st column
        logit <- 1/(1 + exp(-sum(theta*x)))
        (y-logit) * x
    }
    
    sc <- data$sc
    includePackage(sc, vecapply)
    YX <- data$YX
    ndim <- data$ndim
    niter<-data$niter
    
    theta <- double(ndim+1) #initial guess as 0
    alpha <- 1 / length(YX)
    
    for(iter in 1:niter) {
        delta <- lapply(YX, grad.func)
        #cat('delta =', delta, '\n')
        theta <- theta + alpha * reduce(delta, '+') 
        cat('theta =', theta, '\n')
        #print(cost(X,y, theta))
    }
    print(theta)
}

run <- va_cmpfun(run)

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
