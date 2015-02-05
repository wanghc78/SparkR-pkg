app.name <- "LogitRegression_lapply"
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
    
    V_grad.func <- va_vecClosure(grad.func)
    
    YX <- data$YX
    ndim <- data$ndim
    niter<-data$niter
    
    theta <- double(ndim+1) #initial guess as 0
    alpha <- 1 / length(YX)
    
    for(iter in 1:niter) {
        V_YX <-  { if(! exists(".va.listVal", inherits = FALSE) || !identical(.vasrc.listVal, YX)) {
                .va.listVal <- cache(lapplyPartition(YX,
                                function(data){
                                    message("va_list2vec called!")
                                    va_list2vec(data)
                                }
                        )) # cache here
                .vasrc.listVal <- YX
            }
            .va.listVal
        }
        delta <- lapplyPartition(V_YX, V_grad.func)
        #cat('delta =', delta, '\n')
        theta <- theta + alpha * reduce(lapplyPartition(delta, function(vData){list(colSums(vData))}), '+') 
        cat('theta =', theta, '\n')
        #print(cost(X,y, theta))
    }
    print(theta)
}


if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
