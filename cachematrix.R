## makeCacheMatrix creates a special matrix that can cache its inverse. cacheSolve takes as input 
## the special matrix created by makecacheMatrix and returns the inverse of the matrix. If a valid
## inverse has been cached, it is returned, otherwise an inverse is created using the solve function and is cached 
## before returning.

## makeCacheMatrix creates a special matrix that can cache its inverse. This function returns a list
## of four functions that can be used to get ans set the matrix as well as it's inverse. It takes 
## as input a matrix whose inverse esists.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(input) invx <<- input
  getinv<- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve takes as input the special matrix created by makecacheMatrix and returns the inverse of the matrix.
## If a valid inverse is cached, it is returned else inverse is created using the solve function and is cached 
## before returning.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}
