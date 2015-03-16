# This file is used to prepare the data set
# It requires app.name

if(!exists('app.name')) { app.name <- "UnknownApp"}

library(SparkR)

setup <- function(args=c('local', '24', '1000000', '2', '25')) {
    
    if(length(args) < 1L) {
        cat("Usage: ", app.name," <master> <slices> <samples> <NumOfVars> <iters>\n")
        q("no")
    }
    
    master <- args[1]
    if(is.na(master)) { master <- 'local' }
    
    slices <-as.integer(args[2])
    if(is.na(slices)){ slices <- 24 }
    
    n<-as.integer(args[3])
    if(is.na(n)){ n <- 1000000L }
    
    nvar <-as.integer(args[4])
    if(is.na(nvar)){ nvar <- 2L }
    
    
    niter<-as.integer(args[5])
    if(is.na(niter)){ niter <- 25L }
    
    cat('[INFO][', app.name, ' on ', master, '] slices =', slices, ', n=', n, ', nvar=', nvar, ', niter=', niter, '\n')
    
    #generate pre-centered data. Note the data shape is n x nvar
    # Initialize Spark context
    sc <- sparkR.init(master=master, sparkEnvir=list(spark.executor.memory="8g"), app.name, sparkRBackendPort=12346)
    rdd <- parallelize(sc, 1:n, slices)
    
    A <- matrix(c(1, 1, -1, 3), 2, 2)
    
    X <- lapplyPartition(rdd, function(elems){
                local_n <- length(elems)
                S <- matrix(runif(local_n*nvar), nrow=local_n, ncol=nvar)
                X <- scale(S %*% A, scale = FALSE) #pre-centering
                X <- lapply(1:local_n, function(i){X[i,]})
            })
    
    data <- list(sc=sc, X = X, A = A, nvar=nvar, niter = niter)
    
    return(data)
}