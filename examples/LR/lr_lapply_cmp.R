app.name <- "LinearRegression_lapply"
source("setup_lr.R")

library(vecapply)

run <- function(data) {
    grad.func <- function(yx) {
        y <- yx[1]
        x <- yx
        x[1] <- 1 #modify the 1st element
        error <- (sum(x * theta) - y)
        error * x #return delta
    }
    
    sc <- data$sc
    includePackage(sc, vecapply)
    YX <- data$YX
    ndim <- data$ndim
    niter <- data$niter
    
    theta <- double(ndim+1) #initial guess as 0
    alpha <- 0.05 / length(YX) / ndim # a small step
    
    for(iter in 1:niter) {
        delta <- lapply(YX, grad.func)
        #cat('delta =', delta, '\n')
        theta <- theta - alpha * reduce(delta, '+')
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



