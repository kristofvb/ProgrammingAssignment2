## Collection of functions to help in computing the inverse of a matrix.

## Function wrapping a matrix in a way that makes it possible to cache the
## computation of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        set <- function(m) {
                x <<- m
                inv <<- NULL
        }
        getinverse <- function() inv
        setinverse <- function(inverse) inv <<- inverse
        list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## Function calculating the inverse of a matrix. The matrix should be wrapped by
## making a call to makeCacheMatrix. This function expects the matrix to be
## a square invertible matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("returning cached data")
                return(inv)
        }
        orig <- x$get()
        inv <- solve(orig, diag(nrow(orig)), ...)
        x$setinverse(inv)
        inv
}
