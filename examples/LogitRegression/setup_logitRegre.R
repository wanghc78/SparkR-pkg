# This file is used to prepare the data set
# It requires app.name

if(!exists('app.name')) { app.name <- "UnknownApp"}

library(SparkR)

setup <- function(args=c('local', '24', '1000000', '10', '50')) {

    if(length(args) < 1L) {
        cat("Usage: ", app.name," <master> <slices> <samples> <dimensions> <iters>\n")
        q("no")
    }
    
    master <- args[1]
    if(is.na(master)) { master <- 'local' }
    
    slices <-as.integer(args[2])
    if(is.na(slices)){ slices <- 24 }

    n<-as.integer(args[3])
    if(is.na(n)){ n <- 1000000L }
    
    ndim <-as.integer(args[4])
    if(is.na(ndim)){ ndim <- 10L }
    
    niter<-as.integer(args[5])
    if(is.na(niter)){ niter <- 50L }
    
    cat('[', app.name, ' on ', master, '] slices=', slices, ', n=', n, ', ndim=', ndim, ', niters=', niter, '\n')
    
    # Initialize Spark context
    sc <- sparkR.init(master=master, sparkEnvir=list(spark.executor.memory="2g"), app.name)
    rdd <- parallelize(sc, 1:n, slices)
    
    YX <- lapplyPartition(rdd, function(elems){
                local_n <- length(elems)
                X<- matrix(runif(local_n*ndim, -1, 1), nrow=ndim, ncol=local_n)
                theta <- rep(1,ndim)
                Y<- 1/(1+exp(-(1+colSums(theta*X)))) # now the coefficient are all 1
                lapply(1:local_n, function(i){c(Y[i],X[,i])})
            })
    
    data <- list(sc=sc, slices = slices, YX=YX, ndim=ndim, niter=niter);
    
    return(data)
}

