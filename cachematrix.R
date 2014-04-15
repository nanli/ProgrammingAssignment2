## Functions for caching the inverse of a Matrix
## The functions can be tested with the following script
##
## mat=rbind(c(1, -1/4), c(-1/4, 1))
## v <- makeCacheMatrix(mat)
## cacheSolve(v)
## cacheSolve(v)
##
## The second call to cacheSolve returns a cached value


## This funciton creates a special "matrix" object that can cache its inverse
## The returned object is a list of set, get, getSolve and setSolve objects

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve # when the matrix is solved, assign the value to the object environment
    getSolve <- function() m # for retrieving the already solved value
    list(set = set, get = get,
    setSolve = setSolve,
    getSolve = getSolve)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve
## should retieve the inverse from the cache

Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {  # if the matrix has been solved already, then the cached value will be retrieved here
        message("getting cached data")
        return(m)
    }
    # if the matrix has not been solved, then we compute it from scratch
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}