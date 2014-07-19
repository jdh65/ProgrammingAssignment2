## The functions below that are used to create a special object that
## stores an invertible matrix and caches its inverse.

## This first function, makeCacheMatrix, creates a vector which contains functions to:
## 1) set the matrix, 2) retrieve the matrix, 3) solve for the inverse of the matrix,
## and 4) retrieve the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second function, cacheSolve, calculates the inverse of the matrix from
## makeCacheMatrix, unless that inverse has already been calculated, in which case,
## the function retrieves the cached value of the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}