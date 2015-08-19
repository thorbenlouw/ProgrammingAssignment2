## This file contains two functions 'makeCacheMatrix' and
## 'cacheSolve' that can be used to cache the inverse of a 
## given matrix to avoid recomputing it every time.
## 
## Use 'makeCacheMatrix(x)' to create a list of closures acting as a
## wrapper object around x that has the methods Get, Set, GetInverse and SetInverse
## 
## The complementary function 'cacheSolve' uses a wrapped object
## to either return the cached value of the inverse if it hasn't already
## been computed, or computed it and set it as the cached value in the
## wrapper object if it has.


## Creates a wrapper object around the given matrix
## that is a list with the methods Get, Set, GetInverse and SetInverse exposed

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## Replaces the matrix around which the closure is formed
    set <- function(x.new) {
        x <<- x.new
        inv <<- NULL
    }
    
    ## Retrieves the original matrix around which the closure is formed
    get <- function() x
    
    ## SetInverse to set a cached value for the matrix inverse
    setInverse <- function(inv.new) inv <<- inv.new
    
    ## Returns the cached value for the matrix if it has been set, 
    ##     otherwise NULL
    getInverse <- function() inv
    
    list(Set = set, Get = get,
         SetInverse = setInverse,
         GetInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'. 'x' must previously have
## been created previously using makeCacheMatrix.
## This function caches the inverse in the closure to avoid expensive future 
## recomputation, or returns the cached version if it has already been set
cacheSolve <- function(x, ...) {
    inv <- x$GetInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$Get()
    inv <- solve(data, ...)
    x$SetInverse(inv)
    inv
}
