## These functions together serve to cache the inverse
## of a matrix, potentially avoiding the computationally
## demanding operation in the future

## makeCacheMatrix
## This function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## assuming x is an invertible matrix
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("using cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
