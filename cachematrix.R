## These two functions are used to calculate and cache the
## inverse of a matrix, and to access the cached inverse
## for all subsequent calls.

## Encapsulates the matrix 'x'
## $get returns the matrix
## $set changes the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return the inverse of matrix 'x'.
## The inverse is calculated only when necessary.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
