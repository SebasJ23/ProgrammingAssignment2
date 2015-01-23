## This pair of functions return the inverse of a matrix and caches it, so the
## next time the inverse of that specific matrix is needed, the functions will 
## not compute it, but retrieve it from the cache.

## makeCacheMatrix creates a special "matrix" object that caches the inverse of 
## a given matrix.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) im <<- Inv
    getInv <- function() im
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve uses the product of makeCacheMatrix to check if the inverse of 
## the matrix has already been calculated. If it has, it skips the computation 
## and retrieves the inverse from the cache. If the inverse has not been 
## calculated, it computes it and stores it in the cache.

cacheSolve <- function(x, ...) {
    im <- x$getInv() 
    if(!is.null(im)) {
        message("getting cached data")
        return(im) ## Return a matrix that's the inverse of 'x' if it's on cache.
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setInv(im)
    im ## Return a matrix that is the inverse of 'x' if it's not on cache.
}
