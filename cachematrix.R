## R Programming - Assignment 2

## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly (there are 
## also alternatives to matrix inversion that we will not 
## discuss here). Below are functions that cache the inverse 
## of a matrix.

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
# Set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Get the value of the matrix
  get <- function() x
  
  # Set the inverse of matrix
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  
  # Get the inverse of matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by  makeCacheMatrix  above. If the inverse has already
## been calculated (and the matrix has not changed), then  cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
# Get the inverse of matrix
  m <- x$getInverse()
  # Check to see if matrix exists
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  # Get inverse of matrix if it doesn't exist
  m <- solve(x$get())
  x$setInverse(m)
  m
}


