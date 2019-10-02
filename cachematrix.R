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

# cached inverse of matrix
        inv <- NULL

 ## getter/setter for matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
  ## getter/setter for matrix inverse
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv

 ## return list of functions for matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 'cacheSolve' accepts a matrix bundle created by 'makeCacheMatrix',
## and returns the inverse of the matrix.  It uses the inverse matrix cached
## in the bundle if already calculated, otherwise it calculates and stores the
## inverse.  Any additional parameters are passed on to 'solve'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
 # compute inverse of matrix
        mat <- x$get()
        inv <- solve(mat, ...)
 # cache inverse
        x$setInverse(inv)
  # return inverse of matrix
        inv
}
