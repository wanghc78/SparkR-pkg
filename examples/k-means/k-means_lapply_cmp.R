# k-means by built-in kmeans
# 
# Input: 3-dim points, k-means to 10 clusters, with iteration 10.
#   The argument is the input number of points, 100K by default
# Author: Haichuan
###############################################################################
app.name <- "k-means_lapply_cmp"
source("setup_k-means.R")
library(vecapply)

run <- function(data) {
    sc <- data$sc
    includePackage(sc, vecapply)
    clusters <- data$clusters
    niter <- data$niter
    list_data <- cache(data$data)
    
    centers <- lapply(takeSample(list_data, FALSE, clusters, 255L), 
            function(labelcenter){labelcenter[[2]]})
    #pick clusters as default centers
    for(i in 1:niter) {
        cat("Iteration", i, '...\n')
        #map each item into distance to 10 centers.
        map_fun <- function(ptr){
            dist.inner.func <- function(center){
                sum((ptr[[2]]-center)^2) #the loc is in ptr[[2]]
            }
            ctr_dists <- lapply(centers, dist.inner.func)
            ptr[[1]] <- which.min(ctr_dists) #reset the key
            ptr
        }
        
        list_data <- lapply(list_data, map_fun)
        cluster_sizes <- countByKey(list_data)
        reduced_centers <- collect(reduceByKey(list_data, "+", clusters))
        # center count
        clusterSizesVec <- sapply(cluster_sizes, "[[", 2) #unsorted
        clusterSizesVec <- clusterSizesVec[order(sapply(cluster_sizes, "[[", 1))]
        
        norm_center_fun <- function(unnorm_ctr){
            unnorm_ctr[[2]] / clusterSizesVec[unnorm_ctr[[1]]]
        }
        centers <- lapply(reduced_centers, norm_center_fun)
    }
    #calculate the distance to the 10 centers
    
    cat("Centers:\n")
    print(centers);
}

run <- va_cmpfun(run)

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
