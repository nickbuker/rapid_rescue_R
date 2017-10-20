library(fossil)


centroid_finder <- function(x, k) {
    d <- earth.dist(x)
    km <- kmeans(d, centers = k)
    x$cluster <- km$cluster
    return (aggregate(.~cluster, data = x, mean)[, c(1,3,2)])
} 
