# This file is used to prepare the data set
# It requires app.name

if(!exists('app.name')) { app.name <- "UnknownApp"}

library(SparkR)



setup <- function(args=c('local', '24', '1000000', '3', '10', '15')) {
    
    if(length(args) < 1L) {
        cat("Usage: ", app.name," <master> <slices> <samples> <dimensions> <clusters> <iters>\n")
        q("no")
    }
    
    master <- args[1]
    if(is.na(master)) { master <- 'local' }
    
    slices <-as.integer(args[2])
    if(is.na(slices)){ slices <- 24 }
    
    n <-as.integer(args[3])
    if(is.na(n)){ n <- 1000000L }
    
    ndim <-as.integer(args[4])
    if(is.na(ndim)){ ndim <- 3L }
    
    clusters<-as.integer(args[5])
    if(is.na(clusters)){ clusters <- 10L }
    
    niter<-as.integer(args[6])
    if(is.na(niter)){ niter <- 15L }
    
    cat('[INFO][', app.name, ' on ', master, '] slices=', slices, ', n=', n, ', ndim=', ndim, ', clusters=', clusters, ', niter=', niter, '\n', sep='')
    
    # Initialize Spark context
    sc <- sparkR.init(master=master, sparkEnvir=list(spark.executor.memory="8g"), app.name)
    rdd <- parallelize(sc, 1:n, slices)
    
    list_data <- lapplyPartition(rdd, function(elems){
                local_n = length(elems)
                mean_shift <- rep(0:(clusters-1), length.out = ndim*local_n)
                data <- matrix(rnorm(ndim*local_n, sd = 0.3) + mean_shift, ncol=ndim)
                lapply(1:local_n, function(i) {list(NA, data[i,])}) #[[1]] key, [[2]]loc 
            })
    
    return(list(sc=sc, data=list_data, clusters=clusters, niter=niter))
}