## Function to calculate the inverse of a matrix more efficiently
## Function calculates the inverse of a matrix and caches the result.
## If the function is called again to calculate the inverse of the same matrix, it will retrieve the inverse from the cache and return

## Function makeCacheMatrix defines a list of functions to set & get the values of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function cacheSolve calculates the inverse of a matrix only if the result is not already available in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
       return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
