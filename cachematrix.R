## This program contains two functions to cache the inverse of a matrix to avoid computing it repeatedly.


## For a matrix x, make a cache which contains four functions x$set, x$get, x$setinverse, and x$getinverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## For a Matrix cache x, returns the inverse matrix if it contains one.
## Otherwise get the matrix from x, compute the inverse matrix and return it after caching it into x.

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
