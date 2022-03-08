## makeCacheMatrix creates a "matrix container" (list of 4)
## makeCacheMatrix assigns a matrix to the container and it automatically
## stores the inverse of the matrix
## The inverse of the matrix can be accessed at any time with $getinv()

## Other capabilities of the container is to change the matrix stored
## $set(NEWMATRIX), store a matrix as the inverse $setinv(). To store the
## inverse of a new matrix use $setinv(solve(NEWMATRIX)).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
