## 'Cache matrices' are matrixes that can carry a pre-computed inverse matrix
## to save computation time.

## Creates a 'cache matrix' which can carry a pre-computed inverse matrix with
## it (used in conjunction with cacheSolve). Takes as an argument the original
## value of the matrix and retunrs a list with the following functions:
##  - set(matrix): Sets the value of the matrix
##  - get(): Returns the current value of the matrix
##  - setinverse(inverse): Caches the inverse matrix
##  - getinverse(): Returns the inverse matrix if it has been set. Otherwise
##    it returns null.
makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    set <- function(m) {
        matrix <<- m
        inverse <<- NULL
    }
    get <- function() matrix
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(
        set=set,
        get=get,
        setinverse=setinverse,
        getinverse=getinverse
    )
}


## Returns the inverse matrix of a 'cache matrix'. Only computes it the first
## time. Afterwards, it just re-uses the already computed value.
cacheSolve <- function(cachematrix, ...) {
    cachedinverse <- cachematrix$getinverse()
    if (is.null(cachedinverse)) {
        # No luck. We need to calculate it:
        inverse <- solve(cachematrix$get(), ...)
        # And cache it for later
        cachematrix$setinverse(inverse)
        inverse
    } else {
        # Inverse was cached, just return it:
        cachedinverse
    }
}
