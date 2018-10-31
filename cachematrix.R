## makeCacheMatrix
## Builds an object (list) which can contain a matrix and its inverse
##
## cacheSolve
## Helper function to obtain the inverse of a matrix stored in an
## object created with makeCacheMatrix

## The code is heavily inspired by the example "cachevector.R" from week 3 of
## "R Programming"

## This function returns a list with 4 elements
## - 'set' provides a set() function, allowing to store a matrix in the object
## - 'get' provides a get() function, which returns the matrix stored in the object
## - 'setinverse' provides the function setinverse(), which stores the inverse of
##  the matrix in a variable in the local environment.
## - 'getinverse' provides the function getinverse(), which returns the (possibly
##  cached) inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    # Local variable to store the inverse. If NULL, not yet computed
    minv <- NULL
    # List constructor with a default empty content
    set <- function(new_matrix) {
        x <<- new_matrix
        minv <<- NULL # The stored inverse is no longer valid because the
                    # stored content has changed
    }
    # Function returning the stored matrix
    get <- function() x
    # Function returning the matrix inverse (local variable minv)
    getinverse <- function() minv
    # Function to store the matrix inverse in the local variable minv
    setinverse <- function(y) minv <<- y
    # The value returned by the call to makeCacheMatrix is the list with the
    # four components, each related to one of the functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function returns the inverse of the matrix stored in its first argument,
## which must be a list created with makeCacheMatrix
## The inverse is recovered from the cached value, if valid
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## The '...' argument allows passing additional arguments to the solve() function
    minv <- x$getinverse()
    if (is.null(minv)) {
        # The cached value is not valid. We need to compute the inverse
        # We assume that x is invertible
        minv <- solve(x$get(), ...)
        # Cached the result for future use
        x$setinverse(minv)
    } else {
        message("cached data")
    }
    # Return the calculated or cached inverse
    minv
}
