app.name <- "LogitRegression2_lapplyPartitionManual"
source("setup_logitRegre.R")


run <- function(data) {
    
    gradient <- function(partition) {
        partition = partition[[1]]
        Y <- partition[, 1]  # point labels (first column of input file)
        X <- partition[, -1] # point coordinates
        
        # For each point (x, y), compute gradient function
        dot <- X %*% w
        logit <- 1 / (1 + exp(-Y * dot))
        grad <- t(X) %*% ((logit - 1) * Y)
        list(grad)
    }
    
    YX <- data$YX
    ndim <- data$ndim
    niter<-data$niter
    
    # Initialize w to a random value
    w <- double(ndim) #runif(n=ndim, min = -1, max = 1)
    cat("Initial w: ", w, "\n")
    
    # Read data points and convert each partition to a matrix
    points <- cache(lapplyPartition(YX, 
                                    function(part){
                                      list(matrix(unlist(part), ncol=ndim+1))
                                    })
                   )
                   
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        w <- w - reduce(lapplyPartition(points, gradient), "+")
        ctm <- proc.time()
        cat("[INFO]Iter", iter, "Time =", (ctm - ptm)[[3]], '\n')
        ptm <- ctm
        cat("w = ", w, "\n")
    }
    cat("Final w: ", w, "\n")
}


if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
