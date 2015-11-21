## makeCacheMatrix and cacheSolve function together to calculate the inverse of a matrix
## and store the inverse in cache, such that it will be easily retreivable for
## future operations, instead of recomputed every time it is called for

## makeCacheMatrix creates an invertible "matrix" object that can cache the
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## if the inverse has already been calculated, cacheSolve will retrieve the inverse
## from the cache. 
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
