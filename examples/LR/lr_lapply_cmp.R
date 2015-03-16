app.name <- "LinearRegression_lapply_cmp"
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
    
    
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        delta <- lapply(YX, grad.func)
        #cat('delta =', delta, '\n')
        theta <- theta - alpha * reduce(delta, '+')
        ctm <- proc.time()
        cat("[INFO]Iter", iter, "Time =", (ctm - ptm)[[3]], '\n')
        ptm <- ctm
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



