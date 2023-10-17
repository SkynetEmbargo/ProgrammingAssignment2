# Following functions cache the inverse of a matrix.

# Computing the inverse of a square matrix can be done with the solve function in R.
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.

# [makeCache Matrix] function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the matrix
  mat <- x
  # Initialize the cache
  cache <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    mat <<- y
    cache <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    mat
  }
  
  # Function to compute and cache the inverse
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  # Function to get the cached inverse if available, otherwise compute and cache it
  getInverse <- function() {
    if (!is.null(cache)) {
      message("Getting cached data")
      return(cache)
    }
    
    message("Computing the inverse and caching it")
    inverse <- solve(mat)
    cache <<- inverse
    inverse
  }
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# [cacheSolve] function computes the inverse of the special "matrix" 
# returned by [makeCacheMatrix] above. In case inverse is already calculated
# (and the matrix has not changed), then it retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Check if the cached inverse exists
  if (!is.null(x$getInverse())) {
    message("Inverse matrix found in cache.")
    return(x$getInverse())
  }
  
  # If not in cache, compute and cache the inverse
  inverse <- solve(x$get(), ...)
  x$setInverse(inverse)
  inverse
}
