# Nearest Neighbor
# 
# Input 3-dim points, 10 categories, 'C1' to 'C10'.
#   Traing: 10K. 
#   Testing: 10K
# 
# Author: Haichuan Wang
###############################################################################

app.name <- "k-NN_lapply"
source("setup_k-NN.R")


run <- function(data) {
    sc <- data$sc
    list_train<-data$train_set
    train_n <- length(list_train)
    list_test<-data$test_set
    test_n <- length(list_test)
    clusters<- data$clusters
    k <- data$k
    cat('k-NN: k =', k,', Category =', clusters, ', Train =', train_n, ', Test =', test_n, '\n')
    broadcast(sc, list_train)
    #outer loop, map function for each test
    
    kNN.fun <- function(test_item) {
        #calculate the distance to all 
        dists.fun <- function(train_item) {
            sum((train_item$val - test_item$val)^2)
        }
        dists_list <- lapply(list_train, dists.fun)
        
        #change to dists_vec, and do the sorting
        dists <- unlist(dists_list)
        
        mink.indices <-order(dists)
        #then should pick the first k items, find t
        train_items_indices <- mink.indices[1:k]

        train_items_category <- character(k)
        for(i in 1:k) {
          train_items_category[i] <- list_train[[train_items_indices[i]]]$label
        }
        
        #now get the their label and vote
        test_item$label <- names(which.max(table(train_items_category)))
        test_item
    }
    
    ptm <- proc.time() #previous iteration's time
    list_test <- lapply(list_test, kNN.fun)
    
    #get the cl
    test_cl <- countByKey(list_test)
    for(i in 1:length(test_cl)) {
        item <- test_cl[[i]]
        cat(item[[1]], ':', item[[2]], '\n') 
    }
    cat("[INFO]Time =", (proc.time()-ptm)[[3]], '\n')
    cat("End of k-NN\n")
}


if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}