## These functions provide a utility for caching calculated
## values for the inverse of a matrix.
##
## Assumption: only handles inversible matrices.

## This function creates a special matrix that is able
## to store it's value outside the current environment.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes the return value of makeCacheMatrix()
## will either retrieve a cached value of the matrix
## or caculate a new inverse if none exists in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
