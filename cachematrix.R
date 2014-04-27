## Functions for solving the inverse of a matrix and caching the results.
## 
## Author: Shannon Lloyd <shanloid@gmail.com>

## Creates a wrapper for a matrix whose inverse is cacheable. The wrapper is
## just a list of functions for getting and setting the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL
  set <- function(y) {
      x <<- y
      xi <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) {
      xi <<- inverse
  }
  getInverse <- function() xi
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Solves the inverse of a matrix created via makeCacheMatrix. If the inverse
## has already been computed, and the matrix itself has not been changed, this
## will return a cached value. If no cached value is present, it will be
## computed and cached for subsequent reads.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("Retrieving cached data")
    return(i)
  }
  message("No cached inverse, so solving matrix")
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
