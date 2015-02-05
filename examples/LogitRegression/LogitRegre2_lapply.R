app.name <- "LogitRegression2_lapply"
source("setup_logitRegre.R")


run <- function(data) {
    
    #X includes "1" column, Y column vec
    grad.func <- function(yx) {
        y <- yx[1]
        x <- yx[-1]
        dot <- sum(x * w)
        logit <- 1 / (1 + exp(-y * dot))
        x * ((logit - 1) * y)
    }
    
    YX <- data$YX
    ndim <- data$ndim
    niter<-data$niter
    
    # Initialize w to a random value
    w <- double(ndim) #runif(n=ndim, min = -1, max = 1)
    cat("Initial w: ", w, "\n")
    
    for(iter in 1:niter) {
        w <- w - reduce(lapply(YX, grad.func), "+")
        cat("On iteration ", iter, "w = ", w, "\n")
    }
    cat("Final w: ", w, "\n")
}


if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
