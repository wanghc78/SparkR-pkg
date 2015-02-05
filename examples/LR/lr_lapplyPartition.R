appname <- "LinearRegression_lapplyPartition"
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
    
    V_grad.func <- function(V_yx) {
        #message("vgrad input V_yx's str:", str(V_yx))
        V_y <- V_yx[,1]
        V_x <- V_yx
        V_x[,1] <- va_repVecData(1, V_yx) #modify the 1st element
        V_error <- (rowSums(V_x * va_repVecData(theta, V_x)) - V_y)
        V_delta <- V_error * V_x
        #message("error str: ", str(V_delta))
    }

    sc <- data$sc
    includePackage(sc, vecapply)
    YX <- data$YX
    ndim <- data$ndim
    niter <- data$niter
    
    theta <- double(ndim+1) #initial guess as 0
    alpha <- 0.05 / length(YX) / ndim # a small step
    
    
    for(iter in 1:niter) {
        V_YX <- lapplyPartition(YX,
                        function(data){
                            message("va_list2vec called!")
                            va_list2vec(data)
                        }
                ) #cache here
        delta <- lapplyPartition(V_YX, V_grad.func)
        #cat('delta =', delta, '\n')
        theta <- theta - alpha * reduce(lapplyPartition(delta, function(vData){list(colSums(vData))}), '+')
        cat('theta =', theta, '\n')
        #print(cost(X,y, theta))
    }
    print(theta)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}