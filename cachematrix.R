## Acknowledgement of external sources: I used the class forums, especially
## Bill Hilton's posts, to help me understand this assignment.

## makeCacheMatrix takes a matrix as input. Its output is an object containing a
## list of functions (get, set, setinv, and getinv).
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL # if matrix changes, reset the inverse to null
        }
    get <- function () x # return the original matrix
    setinv <- function(solve) inv <<- solve # will be called by cacheSolve
    getinv <- function() inv # return the inverse matrix
    list(get = get, # return the methods in this object
        set = set,
        setinv = setinv,
        getinv = getinv)
}

## cacheSolve takes as input the object created by makeCacheMatrix. If the
## inverse was already calculated, it returns the cached value. Otherwise, it
## calculates the inverse of the matrix that was the input to makeCacheMatrix.

## If the matrix is changed via set(), inv is also reset to null, so cacheSolve
## will not return the old inverse.

cacheSolve <- function(x, ...) {
    # check for an existing solution
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if there was no cached solution, calculate the inverse
    orig <- x$get() # get the original matrix
    inv <- solve(orig)
    x$setinv(inv) 
    inv
}