## This script provides two functions that work together to cache
## the inverse of a matrix. Since calculating the inverse can be
## computationally expensive, caching avoids repeated calculations:
## 1. makeCacheMatrix: creates a special "matrix" object that
## can store both the matrix itself and its cached inverse.
## 2. cacheSolve: computes the inverse of the special "matrix"
## object. If the inverse has already been calculated and
## cached, it retrieves the cached value instead of recomputing.

## makeCacheMatrix creates a special "matrix" object that can store
## a matrix and cache its inverse for later retrieval.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # get the current matrix
        get <- function() x

        setinv <- function(inverse) inv <<- inverse

        # get the cached inverse
        getinv <- function() inv

        # return list of closure functions
        list(
                set = set, get = get,
                setinv = setinv,
                getinv = getinv
        )
}



## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse is already cached, it retrieves the
## cached value instead of recomputing it.
cacheSolve <- function(x, ...) {
        inv <- x$getinv() 

        # If inverse already cached, return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        # Otherwise compute inverse
        data <- x$get()
        inv <- solve(data, ...) 
        x$setinv(inv) 
        inv
}
