## "makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse."

## input is a matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function () x # return the original matrix
    setinv <- function(solve) inv <<- solve # will be called by cacheSolve
    getinv <- function() inv # return the inverse matrix
    list(get = get, # return the methods in this object
         setinv = setinv,
         getinv = getinv)
}


## "cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache."

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data...")
        return(inv)
    }
    orig <- x$get() # get the original matrix
    inv <- solve(orig)
    x$setinv(inv)
    inv
}






















