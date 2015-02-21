## A set of functions to set and cache the inverse of a matrix

## makeCacheMatrix() - Creates an object that is a list of functions.
## set - Used to set the matrix for which you want to calculate an inverse matrix.
## get - Used by the "cacheSolve" function if the inverse has not been previously
##       calculated or if the matrix has changed.
## setmatrix - sets the calculated matrix to the object "m" in makeCacheMatrix().
## getmatrix - caches the calculated matrix so long as the "set" function has not
##             been executed.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Calculates the inverse of a matrix if the matrix has not been modified by
## executing the "set" function in makeCacheMatrix(), otherwise returns the
## cached inverse matrix.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix,...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
