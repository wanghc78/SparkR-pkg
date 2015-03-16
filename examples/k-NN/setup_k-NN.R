# This file is used to prepare the data set
# It requires app.name

if(!exists('app.name')) { app.name <- "UnknownApp"}

library(SparkR)

setup <- function(args=c('local', '24', '10000', '10000', '10', '5')) {
    
    if(length(args) < 1L) {
        cat("Usage: ", app.name," <master> <slices> <train_samples> <test_samples> <clusters> <k>\n")
        q("no")
    }
    
    master <- args[1]
    if(is.na(master)) { master <- 'local' }
    
    slices <-as.integer(args[2])
    if(is.na(slices)){ slices <- 24 }
    
    train_n<-as.integer(args[3])
    if(is.na(train_n)){ train_n <- 10000L }
    
    test_n<-as.integer(args[4])
    if(is.na(test_n)){ test_n <- 10000L }   
    
    clusters<-as.integer(args[5])
    if(is.na(clusters)){ clusters <- 10L }
    
    k<-as.integer(args[6])
    if(is.na(k)){ k <- 5L }    
    
    cat('[INFO][', app.name, ' on ', master, '] slices=', slices, ', train_n=', train_n, ', test_n=', test_n, ', clusters=', clusters, ', k=', k, '\n', sep='')
    
    # Initialize Spark context
    sc <- sparkR.init(master=master, sparkEnvir=list(spark.executor.memory="8g"), app.name, sparkRBackendPort=12346)

    #generate training
#    list_train_set <- lapplyPartition(parallelize(sc, 1:train_n, slices), function(elems){
#                local_train_n <- length(elems)
#                mean_shift <- rep(0:(clusters-1), length.out = 3*local_train_n)
#                train_set <- matrix(rnorm(3*local_train_n, sd = clusters/2) + mean_shift, ncol=3)
#                lapply(1:local_train_n, function(i) {
#                            label_str <-paste('C', as.character(mean_shift[i]), sep="")
#                            list(label=label_str, val=train_set[i,])
#                        })
#            })
    
    #train cannot be an RDD. so use local object and broadcast
    mean_shift <- rep(0:(clusters-1), length.out = 3*train_n)
    train_set <- matrix(rnorm(3*train_n, sd = clusters/2) + mean_shift, ncol=3)
    list_train_set <- lapply(1:train_n, function(i) {
                label_str <-paste('C', as.character(mean_shift[i]), sep="")
                list(label=label_str, val=train_set[i,])
            })
    
    list_test_set <- lapplyPartition(parallelize(sc, 1:test_n, slices), function(elems){
                local_test_n <- length(elems)
                test_set <- matrix(runif(3*local_test_n, min=-clusters, max=2*clusters-1), ncol=3)
                lapply(1:local_test_n, function(i) {
                            list(label="", val=test_set[i,])
                        })
            })
    
    data <-list(sc = sc, train_set=list_train_set, 
            test_set=list_test_set,
            clusters=clusters,
            k=k)
    return(data)
}