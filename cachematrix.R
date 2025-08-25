## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

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
