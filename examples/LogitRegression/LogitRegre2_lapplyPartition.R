app.name <- "LogitRegression2_lapplyPartition"
source("setup_logitRegre.R")

library(vecapply)

run <- function(data) {
    
    #X includes "1" column, Y column vec
    grad.func <- function(yx) {
        y <- yx[1]
        x <- yx[-1]
        dot <- sum(x * w)
        logit <- 1 / (1 + exp(-y * dot))
        x * ((logit - 1) * y)
    }
    
    sc <- data$sc
    includePackage(sc, vecapply)
    
    V_grad.func <- va_vecClosure(grad.func)
    YX <- data$YX
    ndim <- data$ndim
    niter<-data$niter
    
    # Initialize w to a random value
    w <- double(ndim) #runif(n=ndim, min = -1, max = 1)
    cat("Initial w: ", w, "\n")
    
    for(iter in 1:niter) {
        V_YX <- lapplyPartition(YX,
                function(data){
                    message("va_list2vec called!")
                    va_list2vec(data)
                }
        ) #cache here
        w <- w - reduce(lapplyPartition(lapplyPartition(V_YX, V_grad.func),
                                        function(vData){list(colSums(vData))}
                                        ),
                       "+")
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
