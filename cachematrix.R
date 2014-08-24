## In summary, this set of functions gets and caches the inverse of a matrix

## This function makes a cache of the inverse of a matrix. It contains functions to set the value of the matrix, get the value of the matrix, set the value of the inverse of the matrix, and get the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function checks if the inverse of the matrix has been cached. If it has, it returns the cached value without doing any calculations. If not, it calculates the inverse of the matrix and then sets the value in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
