## This pair of functions can calculate and cache the inverse of a matrix.
## It's efficient when you calculate the matrix's inverse a second time,
## because it would be obtained directly from the cache.


## makeCacheMatrix creates a "vector" containing a list of functions that may 
## be used in the next function cacheSolve. they can set or get the original 
## matrix, set or get the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<- y
    r<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve gets the inverse of the matrix from the cache if it has been 
## calculated before, otherwise it calculates the inverse and always returns
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached matrix")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'
}