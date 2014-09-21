## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## The functions 'makeCacheMatrix' and 'cacheSolve' provide a means to construct a
## matrix that can remember its inverse and to retrieve the inverse.
##
## usage example:
##
##  > X <- makeCacheMatrix(matrix(rnorm(9),3,3))  # create matrix
##
##  > cacheSolve(X)                               # retrieve inverse matrix (solve if not yet created) 
##
## test:
##
##  > cacheSolve(X)==solve(X$get())               # assert that the cached results is same as a newly solved

## The function 'makeCacheMatrix' creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## constructs a special matrix for 'x' that can cache its inverse.
        ##
        ## provided functions:
        ## - set : retrieve the matrix
        ## - get : change   the matrix
        ## - setCachedInverse : retrieve the cached value for the inverse matrix
        ## - getCachedInverse : change   the cached value for the inverse matrix

        # provide variable for cached value
        cachedInverse <- NULL

        # data accessors
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x

        # cached value accessors
        setCachedInverse <- function(solve) cachedInverse <<- solve
        getCachedInverse <- function()      cachedInverse

        # return
        list(set = set,
             get = get,
             setCachedInverse = setCachedInverse,
             getCachedInverse = getCachedInverse)
}


## The function 'cacheSolve' returns the inverse of a matrix that has been constructed with makeCacheMatrix. If the inverse has already been calculated 'cachesolve' retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # get cached inverse matrix
        inverse <- x$getCachedInverse()

        # return cached value when it exists
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        # else compute and remember inverse matrix

        data <- x$get()
        inverse <- solve(data, ...)
        x$setCachedInverse(inverse)

        # return
        inverse
}
