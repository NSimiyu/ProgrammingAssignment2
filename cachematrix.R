## This function creates a square matrix and stores it in the cache
## It also calculates the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinv <- function() invm <<- solve(x)
        getinv <- function() invm
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function returns the inverse of the matrix x from the cache

cacheSolve <- function(x, ...) {
        invm <- x$getinv()
        if(!is.null(invm)) {
                message("getting cached inverse matrix")
                return(invm)
        }
        matdata <- x$get()
        invm <- solve(matdata, ...)
        x$setinv(invm)
        invm
}
