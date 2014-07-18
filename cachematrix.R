##
## Matrices with Cacheable Inverse
## ----------------------------------------------------------------------------
## Here are provided two functions to work with matrices for which you
## require its inverse very often. The idea behind these functions is
## quite easy:
##
##   1) Use the function  'makeCacheMatrix'  to create such a matrix, eg.
##
##        m <- makeCacheMatrix(matrix(c(1, 2, 5, 3), nrow=2, ncol=2, byrow=TRUE))
##
##   2) Use the functions  m$set()  and  m$get()  to set/get values of  m,
##      eg.
##
##        m$set(matrix(c(22, 12, 15, 9), nrow=2, ncol=2, byrow=TRUE))
##        m$get()
##
##   3) Every time you need the inverse of  m, simply call  cacheSolve(m)
##      wherever you need it, eg.
##
##        m$get() %*% cacheSolve(m)
##
##      will return the identity matrix  I2  (aka matrix(c(1, 0, 0, 1), nrow=2, ncol=2))
##
## Please note that functions  m$setsolve()  and  m$getsolve()  are intended
## to be used only by function  cacheSolve(), do not use them directly.
## ____________________________________________________________________________


## Create a special matrix  M  able to cache its inverse  M^(-1)
##
makeCacheMatrix <- function(x = matrix()) {

    # this matrix object consist of a list with one variable and four functions

    inv <- NULL   # this is the cache storage for the inverse, initially empty

    set <- function(y) {
        x <<- y           # set the new matrix value and
        inv <<- NULL      # invalidate the cache contents
    }
    get <- function() x   # return the matrix value

    setsolve <- function(s) inv <<- s   # store the inverse in the cache
    getsolve <- function() inv          # return the contents of the cache

    # return the special matrix
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return the inverse of a matrix constructed with  makeCacheMatrix
##
cacheSolve <- function(x, ...) {

    # check the cache
    s <- x$getsolve()

    # the inverse is already in the cache
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }

    # not in the cache: compute the inverse and store it in the cache
    s <- solve(x$get(), ...)
    x$setsolve(s)
    s
}
