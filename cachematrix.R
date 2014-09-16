## These functions perform a matrix inverse operation that uses a cache to 
## avoid redundant calculations.

## 'makeCacheMatrix' takes a square invertible matrix, and returns four functions that 
## prepare its cached inverse. 'set' and 'get' set and fetch the cached matrix.
## 'setinv' and 'getinv' set and fetch its inverse.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    m <- NULL
  }
  get <- function() x
  setinv <- function(matinv) minv <<- matinv 
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## 'cacheSolve' returns the inverse of a matrix that is in the special cache form. 
## If the inverse has been previously cached, it returns the cache.

cacheSolve <- function(x, ...) {
  
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$setinv(minv)
  minv      ## Return a matrix that is the inverse of 'x'
}
