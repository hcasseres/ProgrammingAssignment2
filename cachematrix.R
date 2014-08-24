## Acknowledgement of external sources: I used the class forums, especially
## Bill Hilton's posts, to help me understand this assignment.

# Given a matrix as input, makeCacheMatrix and cacheSolve work together to 
# calculate and cache the inverse of a function. If the inverse has already been
# cached, cacheSolve returns the cached value instead of recalculating it.

## makeCacheMatrix takes a matrix as input. Its output is an object containing a
## list of functions (get, set, setinv, and getinv).

## If we use <- (assignment) instead of <<- (superassignment) in setinv(), the 
## inverse is not cached. We need to use superassignment in order to set inv in
## the environment of makeCacheMatrix() instead of setinv().
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # will hold the cached inverse of x
    set <- function(y) {
        x <<- y # reset the matrix to y
        inv <<- NULL # if matrix changes, reset the inverse to NULL
        }
    get <- function () x # return the original matrix
    setinv <- function(newinv) inv <<- newinv # will be called by cacheSolve
    getinv <- function() inv # return the inverse matrix
    list(get = get, # creates a list of the methods in this object
        set = set,
        setinv = setinv,
        getinv = getinv)
}


## The input to cacheSolve is an object created by makeCacheMatrix. If the 
## inverse was already calculated, cacheSolve returns the cached value.
## Otherwise, it calculates the inverse of the matrix that was the input to
## makeCacheMatrix.

## If the matrix is changed via set(), inv is also reset to NULL, so cacheSolve
## will recalculate the inverse.

cacheSolve <- function(x, ...) {
    # check for an existing solution
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if there was no cached solution, calculate the inverse
    orig <- x$get() # get the original matrix
    inv <- solve(orig) # find the solution
    x$setinv(inv) # use setinv() to cache the solution
    inv
}