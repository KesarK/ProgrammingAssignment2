## This program uses 2 funtions to cache the inverse of a matrix
## and helps generalize the complicated proceedure. 

## this function creates a matrix object that can cache its inverse.

##
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
##
## This is a function which computes the inverse of the matrix returned 
## by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}

