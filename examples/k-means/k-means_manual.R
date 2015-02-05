# use the code in the original SparkR

# Still something wrong here. Doesn't know the data input


###############################################################################
app.name <- "k-means_manual"
source("setup_k-means.R")

run <- function(data) {
    K <- data$clusters
    niter <- data$niter
    points <- data$data
    
    dist.fun <- function(P, C) {
        apply(
                C,
                1, 
                function(x) { 
                    colSums((t(P) - x)^2)
                }
        )
    }
    
    closestPoint <-  function(P, C) { max.col(-dist.fun(P, C)) }
        
    
    # kPoints <- take(points, K)
    kPoints <- do.call(rbind, lapply(takeSample(points, FALSE, K, 255L), 
            function(labelcenter){labelcenter[[2]]}))
    for(i in 1:niter) {
        cat("Iteration", i, '...\n')
        closest <- lapplyPartition(
                lapply(points,
                        function(p) {
                            cp <- closestPoint(p[[2]], kPoints); 
                            mapply(list, unique(cp), split.data.frame(cbind(1, p[[2]]), cp), SIMPLIFY=FALSE)
                        }),
                function(x) {do.call(c, x)
                })
        
        pointStats <- reduceByKey(closest,
                function(p1, p2) {
                    t(colSums(rbind(p1, p2)))
                },
                2L)
        
        newPoints <- do.call(
                rbind,
                collect(lapply(pointStats,
                                function(tup) {
                                    point.sum <- tup[[2]][, -1]
                                    point.count <- tup[[2]][, 1]
                                    point.sum/point.count
                                })))
        
        D <- dist.fun(kPoints, newPoints)
        tempDist <- sum(D[cbind(1:3, max.col(-D))])
        kPoints <- newPoints
        cat("Finished iteration (delta = ", tempDist, ")\n")
    }
    
    cat("Centers:\n")
    print(kPoints);
#    cat("clusterSizes:\n")
#    print(clusterSizes);
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}

