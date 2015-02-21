
## The following is a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" object

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}
