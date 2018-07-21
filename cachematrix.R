## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##implemetation of the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                      ##initializing
    set <- function(y) {           ##setting the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x                  ##getting data or the matrix
    setinv <- function(solve) m <<- solve    ##setting the inverse of the matrix
    getinv <- function() m       ## getting the inverse of the matrix
    list(set = set, get = get,  ##returning all the correct methods
    setinv = setinv,
    getinv = getinv)
}


## Write a short comment describing this function

##implementation of the cacheSolve function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()           ##getting the inverse
        if(!is.null(m)) {           ##if computed already, then getting the cached data
            message("getting cached data")
            return(m)               ##returning the inverse if aleady available
        }
        data <- x$get()           ##getting the data or matrix
        m <- solve(data, ...)    ##finding the inverse as cached data not available
        x$setinv(m)              ##setting the inverse
        m                      ##returning the inverse
}
