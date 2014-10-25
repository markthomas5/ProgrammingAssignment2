## These functions create a matrix, which can be inverted and the 
## inverted matrix can be cached for later use

makeCacheMatrix <- function(x = numeric()) {
## This creates the matrix and $set sets the matrix, $get returns the
## matrix, $setinverse sets the inverse of the matrix, $getinverse
## returns the inverse of the matrix
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x,...) {
## Computes the inverse of the matrix 'x', or returns the cached value 
## if available
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
