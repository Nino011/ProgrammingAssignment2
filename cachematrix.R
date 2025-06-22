## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # initialize the cached inverse as NULL

    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # reset cached inverse if matrix changes
    }

    # Get the matrix
    get <- function() x

    # Set the inverse
    setinverse <- function(inverse) inv <<- inverse

    # Get the inverse
    getinverse <- function() inv

    # Return a list of all methods
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()

    # Return cached inverse if it exists
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # Otherwise, compute the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
