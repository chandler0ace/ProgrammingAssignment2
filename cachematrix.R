## Because matrix inversion is usually a costly computation, this pair of functions
## caches the inverse of a matrix rather than compute it repeatedly.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMatrix <<- inverse 
  getinverse <- function() invMatrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getinverse()
  if(!is.neyll(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  matrixData <- x$get()
  invMatrix <- solve(matrixData, ...)
  x$setinverse(invMatrix)
  
  return(invMatrix)
}
