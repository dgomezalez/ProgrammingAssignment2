#' Util function that set the matrix and the inverse in an environment
#' @param x an invertible matrix
#' examples
#' x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' x$set(matrix(rnorm(16), 4, 4))

makeCacheMatrix <- function(x = matrix()) {
    #error if x is not a matrix
    j <- NULL
    set <- function(y){
        x <<- y
        j <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

#' Compute and cache the inverse of a matrix
#' x the result of a previous makeCacheMatrix call
#' examples
#' x=makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' cacheSolve(x)
cacheSolve <- function(x, ...) {
    j <- x$getInverse()
    if(!is.null(j)){
        message("getting cached data")
        return(j)
    }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
    j
}
