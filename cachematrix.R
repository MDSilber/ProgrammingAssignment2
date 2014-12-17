## Put comments here that give an overall description of what your
## functions do

## These functions provide an easy way to cache the inverse of a matrix.
## makeCacheMatrix is a function that sets up the "scaffolding" for the
## caching mechanism, and cacheSolve() is the function that actually
## handles the caching and retrieval from the cache

## This is a global variable that's used to cache the inverse of a matrix
inverse <- NULL

## Write a short comment describing this function
## This function just sets up the "special list" that's used for caching.
## It maintains a copy of the original matrix, as well as the inverse of that 
## matrix (if it has been calculated and set). It also has setter methods for
## the original and the inverse matrices

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## Setter function for the original matrix
    set <- function(y) {
        x <<- y
        ## Reset the inverse to NULL (cached value is no longer valid)
        inverse <<- NULL
    }
    
    ## Getter function for the original matrix
    get <- function() {
        x
    }
    
    ## Setter method for the inverse of the matrix
    setInverse <- function (inv) {
        ## Set the global variable, which acts as the cache
        inverse <<- inv
    }
    
    ## Getter method for the inverse of the original matrix
    getInverse <- function() {
        inverse
    }
    
    ## Return a list of functions to be used by the cacheSolve function
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function checks to see if the inverse of the matrix has been cached.
## If it has, it just returns that inverse. If not, it calculates the inverse,
## caches it, and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    ## If cached value is not NULL
    if (!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    ## If cached value is not present
    else {
        ## Calculate the inverse of the matrix
        matrix <- x$get()
        inverse <- solve(matrix)

        ## Cache the inverse of the matrix
        x$setInverse(inverse)
        inverse
    }
}
