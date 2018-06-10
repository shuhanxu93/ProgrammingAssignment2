## This pair of functions caches the costly computation of matrix inversion.

## The first function creates a special "matrix".

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function calculates the inverse of the special "matrix" created by
## by makeCacheMatrix above. If the inverse has already been calculated, the 
## cached value is returned

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cache data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
