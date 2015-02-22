## makeCacheMatrix takes an invertible matrix and 
## returns an object (referred to elsewhere as 'CacheMatrix')
## that can cache the inverse of the passed in matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a CacheMatrix object
## (created by makeCacheMatrix) and returns 
## the inverse of that matrix.
## If the inverse has already been calculated,
## this function returns the cached version rather than
## calculating the inverse again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message ("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
