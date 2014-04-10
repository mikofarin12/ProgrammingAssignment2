## 'makeCacheMatrix' and 'cacheSolve' are a pair of functions that allow
## caching the inverse of a matrix to avoid calculating it more than once.
##
## Sample usage:
## m <- matrix(1:4, 2, 2)
## cm <- makeCacheMatrix(m)
## inv <- cacheSolve(cm)

## 'makeCacheMatrix' returns a list that bundles a matrix and its inverse.
## The matrix can be accessed via '$get()' and changed via '$set()'.
## The inverse can be accessed via '$getInv()' and changed via '$setInv()'.

makeCacheMatrix <- function(x = matrix()) {
    # cached inverse
    inv <- NULL

    # accessors
    get <- function() x
    getInv <- function() inv

    # mutators
    set <- function(newX) {
        x <<- newX
        inv <<- NULL
    }
    setInv <- function(newInv) inv <<- newInv

    # return the bundle
    list(get = get, set = set,
         getInv = getInv, setInv = setInv)
}


## 'cacheSolve' accepts a matrix bundle created by 'makeCacheMatrix',
## and returns the inverse of the matrix.  It uses the inverse matrix cached
## in the bundle if already calculated, otherwise it calculates and stores the
## inverse.  Any additional parameters are passed on to 'solve'.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()

    # invert if necessary
    if (is.null(inv)) {
        m <- x$get()
        inv <- solve(m, ...)
        x$setInv(inv)
    }

    # return the inverse matrix
    inv
}
